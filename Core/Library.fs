namespace Rssify.Core
open System
open FSharpPlus
open FSharp.Data
open System.Globalization
open System.Collections.Generic
open Microsoft.Recognizers.Text.DateTime
open Microsoft.Recognizers.Text

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
    Link: string
}
module DateTime=
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

