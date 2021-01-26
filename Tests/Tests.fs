module Rssify.Tests

open System
open Xunit
open FSharpPlus
open FSharp.Data
open Rssify.Core

let onlyOpenGraph = HtmlDocument.Parse """"
<html>
<head>
<meta property="og:title" content="TheTitle" />
<meta property="og:description" content="TheDescription" />
<meta property="article:published_time" content="20210117T072303Z" />
</head>
</html>
"""

let taggedHtml = HtmlDocument.Parse """"
<html>
<head>
</head>
<body>
<article class="post-single">
  <header class="post-header">
    <h1 class="post-title">TheTitle</h1>
    <div class="post-description">TheDescription</div>
    <div class="post-date">January 9, 2021</div>
    <a class="post-next" href="https://someblog.com/14324">Next</a>
  </header> 
  <div class="post-content">

  </div>
  <footer class="post-footer">
  </footer>
</article>
</body>
</html>
"""

module ISO8601=

    [<Fact>]
    let ``Can understand date`` () = Assert.True (Option.isSome <| DateTime.tryParse "2021-01-17")
    //TODO: [<Fact>]
    let ``Can understand datetime A`` () = Assert.True (Option.isSome <| DateTime.tryParse "2021-01-17T07:23:03+00:00")
    [<Fact>]
    let ``Can understand datetime B`` () = Assert.True (Option.isSome <| DateTime.tryParse "2021-01-17T07:23:03Z")
    [<Fact>]
    let ``Can understand datetime C`` () = Assert.True (Option.isSome <| DateTime.tryParse "20210117T072303Z")


[<Fact>]
let ``Can understand open graph format`` () =
    let {Date=date; Title=title; Description=description; Next=_} = HtmlDocument.digest Selectors.Default onlyOpenGraph
    Assert.Equal(Some "TheTitle", title)
    Assert.Equal(Some "TheDescription", description)
    Assert.Equal(Some (DateTime(2021,1,17,7,23,03)), date)

[<Fact>]
let ``Can understand tagged HTML`` () =
    let s = { 
        CssDate=Some ".post-date"
        CssTitle=Some ".post-title"
        CssDescription=Some ".post-description"
        CssNext=Some ".post-next" }
    let {Date=date; Title=title; Description=description; Next=_} = HtmlDocument.digest s taggedHtml
    Assert.Equal(Some "TheTitle", title)
    Assert.Equal(Some "TheDescription", description)
    Assert.Equal(Some (DateTime(2021,1,9)), date)

