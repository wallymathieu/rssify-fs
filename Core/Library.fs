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
  Date : string option
  Title : string option
  Description : string option
  Next : string option
}
with 
  static member Default = { Date=None; Title=None; Description=None; Next=None }

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

    let date =  s.Date >>= dateSelect <|> publishedTime
    let title = s.Title >>= selectInnerText <|> ogTitle
    let description = s.Description >>= selectInnerText <|> ogDescription
    let next = s.Next >>= selectHref
    (date, title, description, next)

module FeedItem =
  let ofValues (link:Uri,(date, title, description, _)) =
    let numberAndLinkTitle = link.PathAndQuery.Replace("_"," ").Replace("-"," ").Replace("/"," ")
    {Title = Option.defaultValue numberAndLinkTitle title
     Date = Option.defaultValue DateTime.UtcNow date
     Description = Option.defaultValue "" description
     Link = link }
  let date (fi:FeedItem) = fi.Date
type SiteId = SiteId of int64
type Site={Id:SiteId;
           Title:string;
           Link:Uri;
           Description:string;}
type IStore =
  abstract member GetTimestamp: SiteId -> DateTime option
  abstract member GetFeedItems: SiteId -> FeedItem seq
  abstract member AddFeedItems: SiteId*FeedItem seq -> unit
module Store=
  type InMemory()=
    let mutable map = Map.empty
    let mutable timestamps = Map.empty
    interface IStore with
      member __.GetTimestamp (SiteId siteId) = Map.tryFind siteId timestamps
      member __.GetFeedItems (SiteId siteId) = Map.tryFind siteId map |> Option.defaultValue Seq.empty
      member __.AddFeedItems (SiteId siteId, items) = 
          map <- Map.change siteId (function | Some v -> Some (Seq.append v items) | None -> Some items) map
          timestamps <- Map.add siteId DateTime.UtcNow timestamps

module Site=
  /// iterate on site as long as there is a next url
  let munch (link:Uri) (s:Selectors) = seq {
    let mutable head = HtmlDocument.Load (string link) |> HtmlDocument.digest s
    yield (link, head)
    let nextLink = let (_,_,_,n) = head in n
    while nextLink.IsSome do
      let link = nextLink.Value
      head <- HtmlDocument.Load link |> HtmlDocument.digest s
      yield (Uri(link), head)
  }
  /// munch items for a site
  let munchAndStore (site:Site) (s:Selectors) (store:IStore) =
    let last = store.GetFeedItems site.Id |> Seq.sortByDescending FeedItem.date |> Seq.tryHead
    match last with
    | Some item ->
      let res = munch item.Link s |> map FeedItem.ofValues |> Seq.skip 1
      store.AddFeedItems (site.Id, res)
    | None -> 
      let res = munch site.Link s |> map FeedItem.ofValues
      store.AddFeedItems (site.Id, res)
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
          elem "title" site.Title,
          elem "link" <| string site.Link,
          elem "description" site.Description,
          elem "language" "en-us", // TODO: fix
          XElement(xn "channel", elems)
        ) |> box)


    