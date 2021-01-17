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


