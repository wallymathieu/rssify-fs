namespace Rssify.Core
open System
open System.Text
open System.Security.Cryptography

module SHA512=

  let ofList (arr:string list)=
    use sha = SHA512.Create()
    let bytes =
        arr |> Array.ofList
            |> Array.collect Encoding.UTF8.GetBytes
            |> sha.ComputeHash
    BitConverter.ToInt64(bytes, 0)

module String=
  let toGuid (s:string) = Guid( MD5.Create().ComputeHash( Encoding.Default.GetBytes s ) )

open FSharpPlus
open FSharp.Data

type Selectors = {
  CssDate : string option
  CssTitle : string option
  CssDescription : string option
  CssNext : string option
}
with 
  static member Default = { CssDate=None; CssTitle=None; CssDescription=None; CssNext=None }

type FeedItem = {
  Title: string
  Date: DateTime
  Description: string
  Link: Uri
}
module DateTime=
  open System.Globalization
  open System.Collections.Generic
  open Microsoft.Recognizers.Text.DateTime
  open Microsoft.Recognizers.Text

  let tryParseISO8601 (x:string) =
    match DateTime.TryParseExact(x, [|"yyyy-MM-ddTHH:mm:ss.fffZ"; "yyyy-MM-ddTHH:mm:ssZ"; "yyyyMMddTHHmmssZ"; 
                                      "yyyy-MM-dd"; "yyyyMMdd"|], null, DateTimeStyles.RoundtripKind) with
    | true,v->Some v|false,_->None

  let private model= DateTimeRecognizer().GetDateTimeModel()
  let tryRecognize (x:string) =
    let tryResolveDateTime (m:ModelResult)=
      let values = m.Resolution.["values"]
      match values with
      | :? ResizeArray<Dictionary<string,string>> as d ->
          if d.Count >= 1 && d.[0].ContainsKey "value" then tryParseISO8601 d.[0].["value"] else None
      | _ -> failwith "expected ResizeArray<Dictionary<string,string>>"
    match model.Parse(x) |> Seq.tryHead with
    | Some r ->
      match r.TypeName with 
      | "datetimeV2.date" -> tryResolveDateTime r
      | "datetimeV2.datetime" -> tryResolveDateTime r
      | typename->
           failwithf "Unknown TypeName %s" typename
    | None -> None
  let tryParse x = tryParseISO8601 x <|> tryRecognize x
type Digested = { Date: DateTime; Title: string option; Description: string option; Next: Uri option }
module HtmlDocument=
  let digest (s:Selectors) (htmlDoc:HtmlDocument) =
    let selectInnerText selector =
      htmlDoc.CssSelect selector |> List.tryHead
      |> map HtmlNode.innerText
    let selectHref selector =
      htmlDoc.CssSelect selector |> List.tryHead
      |> map (HtmlNode.attributeValue "href")

    let metaSelect value =
      htmlDoc.CssSelect "meta"
      |> tryFind ( HtmlNode.hasAttribute "property" value )
      |> map (HtmlNode.attributeValue "content")

    let dateSelect selector : DateTime option = selectInnerText selector >>= DateTime.tryParse
    let publishedTime : DateTime option = metaSelect "article:published_time" >>= DateTime.tryParse
    let ogTitle = metaSelect "og:title"
    let ogDescription = metaSelect "og:description"

    let date =  s.CssDate >>= dateSelect <|> publishedTime |> Option.defaultValue DateTime.UtcNow
    let title = s.CssTitle >>= selectInnerText <|> ogTitle
    let description = s.CssDescription >>= selectInnerText <|> ogDescription
    let next = s.CssNext >>= selectHref |> map Uri
    {Date=date; Title=title; Description=description; Next = next}

module FeedItem =
  let ofValues (link:Uri,{ Date=date; Title=title; Description=description; Next=_}) =
    let numberAndLinkTitle = link.PathAndQuery.Replace("_"," ").Replace("-"," ").Replace("/"," ")
    {Title = Option.defaultValue numberAndLinkTitle title
     Date = date
     Description = Option.defaultValue "" description
     Link = link }
  let date (fi:FeedItem) = fi.Date
type SiteId = SiteId of int64
type Site={Title:string option;
           Link:Uri;
           Description:string option
           Selectors: Selectors}

[<CLIMutable>]
type RssOptions = {
  /// polling timespan
  PollTimeout : TimeSpan
  ItemsToPoll : int }

type IStore =
  abstract member VisitSite: SiteId*Site -> unit Async
  abstract member GetSite: SiteId -> Site option Async
  abstract member GetSitesToPoll: unit -> (SiteId*Site) seq Async
  abstract member GetFeedItems: SiteId -> FeedItem seq Async
  abstract member AddPolledItems: SiteId*FeedItem seq -> unit Async

module Store=
  type Sites = {
      Id:int64
      mutable LastVisit:DateTime
      Site:Site
      mutable Items:FeedItem list
      mutable LastPolled:DateTime
  }
  with
    static member GetSite (m:Sites) = m.Site
    static member GetItems (m:Sites) = m.Items
    static member GetLastPolled (m:Sites) = m.LastPolled

  let inMemory (opts:RssOptions)=
    let mutable map = Map.empty
    { new IStore with
      member __.VisitSite (SiteId id,s:Site) =
        match Map.tryFind id map with 
        | Some v -> v.LastVisit <- DateTime.UtcNow
        | None -> map<-Map.add id { Id=id; LastVisit=DateTime.UtcNow; Site = s; Items=[]; LastPolled=DateTime.MinValue } map
        async.Return ()
      member __.GetSitesToPoll () =
        let timeout = DateTime.UtcNow - opts.PollTimeout
        Map.values map 
        |> Seq.filter (fun s -> timeout > s.LastPolled)
        |> Seq.map (fun s->(SiteId s.Id,Sites.GetSite s)) |> async.Return
      member __.GetSite (SiteId siteId) =
        Map.tryFind siteId map |> Option.map Sites.GetSite |> async.Return
      member __.GetFeedItems (SiteId siteId) = 
        Map.tryFind siteId map
        |> (Option.map Sites.GetItems >> Option.map Seq.ofList >> Option.defaultValue Seq.empty) |> async.Return
      member __.AddPolledItems (SiteId siteId, items) = 
        let items = List.ofSeq items
        match Map.tryFind siteId map with
        | Some v ->
          v.Items <- v.Items @ items
          v.LastPolled <- DateTime.UtcNow
        | None -> failwith "Should only poll a site that has been visited"
        async.Return () }
          

  open Marten
  
  let marten (session:IDocumentSession) (opts:RssOptions)=
      { new IStore with
        member __.VisitSite (SiteId id,s:Site) = async {
          match! Session.loadByInt64Async<Sites> (id) session with 
          | Some v -> v.LastVisit <- DateTime.UtcNow; session.Store v
          | None -> session.Store { Id=id; LastVisit=DateTime.UtcNow; Site = s; Items=[]; LastPolled=DateTime.MinValue }
          do! Session.saveChangesAsync session }
        member __.GetSite (SiteId siteId) =
          Session.loadByInt64Async<Sites> siteId session |> (Async.map << map) Sites.GetSite
        member __.GetSitesToPoll () =
          let timeout = DateTime.UtcNow - opts.PollTimeout
          Session.query<Sites> session
          |> Queryable.filter <@ fun s -> timeout > s.LastPolled @>
          |> Queryable.take opts.ItemsToPoll
          |> Queryable.toListAsync
          |> (Async.map<<Seq.map) (fun s -> (SiteId s.Id,Sites.GetSite s))
        member __.GetFeedItems (SiteId siteId) = Session.loadByInt64Async<Sites> siteId session
                                                 |> (Async.map) (Option.map Sites.GetItems
                                                                 >> map Seq.ofList >> Option.defaultValue Seq.empty)
        member __.AddPolledItems (SiteId siteId, items) = async {
          let items = List.ofSeq items
          match! Session.loadByInt64Async<Sites> siteId session with
          | Some v ->
            v.Items <- v.Items @ items
            v.LastPolled <- DateTime.UtcNow
            session.Store v
          | None -> failwith "Should only poll a site that has been visited"
          do! Session.saveChangesAsync session } }


module Site=
  /// iterate on site as long as there is a next url
  let munch (link:Uri) (s:Selectors) = async {
    let digest = HtmlDocument.digest s
    let! head = HtmlDocument.AsyncLoad (string link) |> Async.map digest
    let mutable result = ResizeArray()
    result.Add (link, head)
    let mutable nextLink = head.Next
    while nextLink.IsSome do
      let link = nextLink.Value
      let! head = HtmlDocument.AsyncLoad (string link) |> Async.map digest
      nextLink <- head.Next
      result.Add (link, head)
    return result :> seq<_>
  }
  /// munch items for a site
  let munchAndStore (store:IStore) (id,site:Site) = async {
    let s = site.Selectors
    let! last = store.GetFeedItems id |> Async.map (Seq.sortByDescending FeedItem.date >> Seq.tryHead)
    match last with
    | Some item ->
      let! res = munch item.Link s |> Async.map (Seq.map FeedItem.ofValues >> Seq.skip 1)
      do! store.AddPolledItems (id, res)
    | None -> 
      let! res = munch site.Link s |> Async.map (Seq.map FeedItem.ofValues)
      do! store.AddPolledItems (id, res) }
module Rss=
  // from: http://www.fssnip.net/7QI/title/Generate-rss-feed-from-F-record-items
  open System.Xml.Linq
  let htmlencode = System.Net.WebUtility.HtmlEncode
  let feed
        (site : Site)
        (items : FeedItem seq) =
    let xn = XName.Get
    let elem name (valu:string) = XElement(xn name, valu)
    let elems =
        items |> sortBy FeedItem.date
        |> map(fun i ->
          let link = string i.Link
          let guid = string <| String.toGuid i.Link.PathAndQuery
          XElement(xn "item",
            elem "title" (htmlencode i.Title),
            elem "link" link,
            elem "guid" guid,
            elem "pubDate" (i.Date.ToString("r")),
            elem "description" (htmlencode i.Description)
          ))
    XDocument(
      XDeclaration("1.0", "utf-8", "yes"),
        XElement(xn "rss",
          XAttribute(xn "version", "2.0"),
          elem "title" <| Option.defaultValue "" site.Title,
          elem "link" <| string site.Link,
          elem "description" <| Option.defaultValue "" site.Description,
          elem "language" "en-us", // TODO: fix
          XElement(xn "channel", elems)
        ) |> box)
