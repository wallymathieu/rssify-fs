namespace Rssify.Core
open System
open FSharpPlus
open FSharp.Data
open System.Globalization

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
    let tryParse (x:string) = 
        match DateTime.TryParseExact(x, [|"yyyy-MM-ddTHH:mm:ss.fffZ"; "yyyy-MM-ddTHH:mm:ssZ"; "yyyyMMddTHHmmssZ"; 
                                          "yyyy-MM-dd"; "yyyyMMdd"|], null, DateTimeStyles.RoundtripKind) with
        | true,v->Some v|false,_->None

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

