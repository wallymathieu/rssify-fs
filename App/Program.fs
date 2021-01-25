// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open FSharpPlus
open FSharpPlus.Data
type DictEntry = System.Collections.DictionaryEntry

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Features
open Microsoft.AspNetCore.Authentication
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open System.Text.RegularExpressions
open Giraffe
open Rssify.Core

(*
 /rss?url=encodedUrl&date=encodedDateSelector&description=encodedDescriptionSelector..
*)
(*
module Web=
    let uriToSiteAndSelectors
*)
module Web=
  [<CLIMutable>]
  type QueryString =
    {
      FeedTitle       : string option
      FeedDescription : string option
      Link            : string
      Date            : string option
      Title           : string option
      Description     : string option
      Next            : string option
    }
  let toSiteAndSelectors (s:IStore) (q:QueryString)=
    let id = [q.FeedTitle;q.FeedDescription;Some q.Link;q.Date;q.Title;q.Description;q.Next] 
             |> List.choose id
             |> SHA512.ofList
             |> SiteId
    let link = Uri q.Link
    let head = lazy( Seq.tryHead <| s.GetFeedItems id )
    let defaultTitle () = match head.Value with | Some h -> h.Title | None -> link.PathAndQuery
    let defaultDescription () = match head.Value with | Some h -> h.Description | None -> ""
    let site = {
      Id          = id
      Title       = Option.defaultWith defaultTitle q.FeedTitle
      Link        = link
      Description = Option.defaultWith defaultDescription q.FeedDescription
    }
    let selectors = {
      Date        = q.Date
      Title       = q.Title
      Description = q.Description
      Next        = q.Next
    }
    (site, selectors)

  let parsingErrorHandler err = RequestErrors.BAD_REQUEST err
  let tryBindQuery<'T> = tryBindQuery<'T> parsingErrorHandler None
  let webApp (s:IStore) =
    let rss q =
      let (site,selectors) = toSiteAndSelectors s q
      let timestamp = s.GetTimestamp site.Id
      match timestamp with
      | Some t when (DateTime.UtcNow - t).TotalDays < 1.0 -> ()
      | _ -> Site.munchAndStore site selectors s
      let items = s.GetFeedItems site.Id
      let rssXml = Rss.feed site items
      xml (string rssXml)

    choose [ route "/" >=> (text "")
             route "/rss" >=> (tryBindQuery<QueryString> rss )]

let errorHandler (ex : Exception) (logger : ILogger) =
  logger.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
  clearResponse >=> setStatusCode 500 >=> text ex.Message

let configureServices (services : IServiceCollection) =
  services
      .AddGiraffe()
      .AddDataProtection() |> ignore

let configureLogging (loggerBuilder : ILoggingBuilder) =
  loggerBuilder.AddFilter(fun lvl -> lvl.Equals LogLevel.Error)
               .AddConsole()
               .AddDebug() |> ignore

[<EntryPoint>]
let main argv =
  // parse arguments

  let store = Store.InMemory()
  let configureApp (app : IApplicationBuilder) =
    app.UseGiraffeErrorHandler(errorHandler)
       .UseGiraffe (Web.webApp store)

  WebHost.CreateDefaultBuilder()
        .Configure(Action<IApplicationBuilder> configureApp)
        .ConfigureServices(configureServices)
        .ConfigureLogging(configureLogging)
        .Build()
        .Run()
  0
