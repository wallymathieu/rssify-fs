#r "nuget: FSharpPlus,1.1.7"
#r "nuget: FSharp.Data,3.3.3"

open FSharpPlus
open FSharp.Data

let res = HtmlDocument.Parse """"
<html>
<head>
<meta property="og:title" content="TheTitle" />
</head>
</html>
"""
res.CssSelect  "meta"
|> List.filter ( HtmlNode.hasAttribute "property" "og:title" )
|> List.tryHead

#r "nuget: Microsoft.Recognizers.Text.DateTime,1.5.0"
open Microsoft.Recognizers.Text.DateTime
open System.Collections.Generic
let recognizer =DateTimeRecognizer()
let model= recognizer.GetDateTimeModel()
let res2 = model.Parse("January 9, 2021").[0]
//TypeName = "datetimeV2.date";
let values = res2.Resolution.["values"]
match values with
| :? ResizeArray<Dictionary<string,string>> as d when d.Count >1 && d.[0].ContainsKey "value" -> d.[0].["value"]
| _ -> failwith "!"
