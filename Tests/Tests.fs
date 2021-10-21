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
    Assert.Equal(Some <| DateTime(2021,1,17,7,23,03), date)

[<Fact>]
let ``Can understand tagged HTML`` () =
    let s = { 
        CssDate=Some ".post-date"
        CssTitle=Some ".post-title"
        CssDescription=Some ".post-description"
        CssNext=Some ".post-next"
        CssList=None }
    let {Date=date; Title=title; Description=description; Next=_} = HtmlDocument.digest s taggedHtml
    Assert.Equal(Some "TheTitle", title)
    Assert.Equal(Some "TheDescription", description)
    Assert.Equal(Some <| DateTime(2021,1,9), date)

let defaultSite={ Title = None; Link=Uri("https://somesite.com"); Description = None; Selectors = Selectors.Default }
let defaultOptions = { PollTimeout = TimeSpan(0,1,0); VisitTimeout = TimeSpan(30,0,0,0); ItemsToPoll = 10 }
open FSharp.Quotations.Evaluator
open Store
let timeoutFilter = QuotationEvaluator.Evaluate <| getTimeoutFilter defaultOptions 
let now =  DateTime.UtcNow
[<Fact>]
let ``A recently visited site that has not been polled in a while should be polled`` () =
  let site = { Id=1L; LastVisit = now; LastPolled = now.AddDays(-100.0); Site=defaultSite; Items=[] }
  Assert.True( timeoutFilter site )
[<Fact>]
let ``A recently visited site that has been polled recently should not be polled`` () =
  let site = { Id=1L; LastVisit = now; LastPolled = now; Site=defaultSite; Items=[] }
  Assert.False( timeoutFilter site )
[<Fact>]
let ``A not visited site that has not been polled in a while should not be polled`` () =
  let site = { Id=1L; LastVisit = now.AddYears(-10); LastPolled = now.AddDays(-100.0); Site=defaultSite; Items=[] }
  Assert.False( timeoutFilter site )
[<Fact>]
let ``A not visited site that has been polled recently should not be polled`` () =
  let site = { Id=1L; LastVisit = now.AddYears(-10); LastPolled = now; Site=defaultSite; Items=[] }
  Assert.False( timeoutFilter site )

[<Fact>]
let ``A one item feed can be generated`` () =
  let splitLines str= split ["\n";"\r"] str |> filter (not << String.IsNullOrEmpty)
  let site={ Title = Some "SiteTitle"; Link=Uri("https://somesite.com"); Description = Some "SiteDescription"; Selectors = Selectors.Default }
  let items = [{ Title="Title"; Date=DateTime(2000,1,1); Description="Description"; Link=Uri("https://somesite.com/somefeedlikething1") }] 
  let rssFeed = Rss.feed site items
  Assert.Equal<string> (splitLines """<rss version="2.0">
  <title>SiteTitle</title>
  <link>https://somesite.com/</link>
  <description>SiteDescription</description>
  <language>en-us</language>
  <channel>
    <item>
      <title>Title</title>
      <link>https://somesite.com/somefeedlikething1</link>
      <guid>89a49a1b-1290-c502-f7b7-fab23773deb8</guid>
      <pubDate>Sat, 01 Jan 2000 00:00:00 GMT</pubDate>
      <description>Description</description>
    </item>
  </channel>
</rss>
""", splitLines <| string rssFeed )