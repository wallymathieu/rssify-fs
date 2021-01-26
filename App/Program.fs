// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.Text
open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Options
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
type DictEntry = System.Collections.DictionaryEntry

open FSharpPlus
open FSharpPlus.Data
open Giraffe

open Rssify.Core
open Marten
open Marten.Events

(*
 /rss?url=encodedUrl&date=encodedDateSelector&description=encodedDescriptionSelector..
*)
module Web=
  let xml (s:string) : HttpHandler =
    let bytes = Encoding.UTF8.GetBytes s
    fun (_ : HttpFunc) (ctx : HttpContext) ->
        ctx.SetContentType "application/xml; charset=utf-8"
        ctx.WriteBytesAsync bytes

  [<CLIMutable>]
  type StoreConfig = {

    SchemaName        : string
  }
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
  let toSiteAndSelectors (q:QueryString)=
    let id = [q.FeedTitle;q.FeedDescription;Some q.Link;q.Date;q.Title;q.Description;q.Next] 
             |> List.choose id
             |> SHA512.ofList
             |> SiteId
    let link = Uri q.Link
    {
      Id          = id
      Title       = q.FeedTitle
      Link        = link
      Description = q.FeedDescription
      Selectors   = {
        CssDate        = q.Date
        CssTitle       = q.Title
        CssDescription = q.Description
        CssNext        = q.Next
      }
    }

  let parsingErrorHandler err = RequestErrors.BAD_REQUEST err
  let tryBindQuery<'T> = tryBindQuery<'T> parsingErrorHandler None
  let webApp =
    let rss q : HttpHandler =
      fun next ctx ->
        let s = ctx.RequestServices.GetRequiredService<IStore>()
        let site = toSiteAndSelectors q
        let visitAndItemsToXml (items:FeedItem seq) = async {
          let head = Seq.tryHead items
          let defaultWith thunk = function | None -> thunk() | some -> some
          let defaultTitle () = match head with | Some h -> Some h.Title | None -> Some site.Link.PathAndQuery
          let defaultDescription () = match head with | Some h -> Some h.Description | None -> None
          let site = { site with Title = defaultWith defaultTitle site.Title
                                 Description = defaultWith defaultDescription site.Description }
          do! s.VisitSite site
          let rssXml = Rss.feed site items
          return xml (string rssXml) next ctx }

        let items = s.GetFeedItems site.Id
        items
        |> Async.bind visitAndItemsToXml
        |> Async.RunSynchronously // NOTE

    choose [ route "/" >=> (text "")
             route "/rss" >=> (tryBindQuery<QueryString> rss )]

type TimedHostedPollingService(svc:IServiceProvider)=
  let mutable timer=None
  let opts = svc.GetRequiredService<IOptions<RssOptions>>().Value
  let logger = svc.GetRequiredService<ILogger<TimedHostedPollingService>>()
  let subscribe (s:IStore) = async {
    let! sites = s.GetSitesToPoll()
    for site in sites do
        logger.LogInformation ("Starting to munch {SITE_TITLE}", site.Title)
        do! Site.munchAndStore site s
        logger.LogInformation ("Finished munching {SITE_TITLE}", site.Title) }

  member __.OnTimeout(_:obj)=
    logger.LogInformation "OnTimeout"
    using (svc.CreateScope()) (fun scope->
      let store = scope.ServiceProvider.GetRequiredService<IStore>()
      subscribe store |> Async.RunSynchronously)
  interface IHostedService with
    member __.StartAsync _ =
      logger.LogInformation ("Start polling {POLLING}", opts.PollTimeout)
      timer <- Some <| new Timer(__.OnTimeout, null, TimeSpan.Zero, opts.PollTimeout); Task.CompletedTask
    member __.StopAsync _ =
      logger.LogInformation "Stop polling"
      timer |> Option.iter (fun t -> t.Change(Timeout.Infinite, 0) |> ignore); Task.CompletedTask
  interface IDisposable with
    member __.Dispose() =
      timer |> Option.iter (fun t -> t.Dispose())


let errorHandler (ex : Exception) (logger : ILogger) =
  logger.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
  clearResponse >=> setStatusCode 500 >=> text ex.Message

let configureServices (ctx:WebHostBuilderContext) (services : IServiceCollection) =
  services
      .AddRouting()
      .AddGiraffe()
      .AddDataProtection() |> ignore
  services
      .AddSingleton<IHostedService>(fun di -> new TimedHostedPollingService(di)
                                              :> IHostedService)
      .AddOptions()
      .Configure<RssOptions>(ctx.Configuration)
      .Configure<Web.StoreConfig>(ctx.Configuration) |> ignore
  let conn=ctx.Configuration.GetConnectionString "Default"

  if String.IsNullOrEmpty conn then
    services.AddSingleton<IStore>(fun di->Store.inMemory (di.GetRequiredService<IOptions<RssOptions>>().Value)) |> ignore
  else
    services
      .AddSingleton<DocumentStore>(fun di ->
        DocumentStore.For(fun opt ->
          let config = di.GetRequiredService<IOptions<Web.StoreConfig>>()
          opt.Connection(conn)
          opt.AutoCreateSchemaObjects <- AutoCreate.All
          opt.DatabaseSchemaName <- config.Value.SchemaName
          opt.Events.StreamIdentity <- StreamIdentity.AsString
        )
      )
      .AddScoped<IDocumentSession>(fun di-> di.GetRequiredService<DocumentStore>().OpenSession())
      .AddScoped<IStore> (fun di->
        let session = di.GetRequiredService<IDocumentSession>()
        let opts = di.GetRequiredService<IOptions<RssOptions>>()
        Store.marten session opts.Value
      ) |> ignore


let configureLogging (loggerBuilder : ILoggingBuilder) =
  loggerBuilder.AddConsole()
               .AddDebug() |> ignore

[<EntryPoint>]
let main argv =

  let configureApp (app : IApplicationBuilder) =
    app.UseGiraffeErrorHandler(errorHandler)
       .UseGiraffe (Web.webApp)

  WebHost.CreateDefaultBuilder()
        .Configure(Action<IApplicationBuilder> configureApp)
        .ConfigureServices(configureServices)
        .ConfigureLogging(configureLogging)
        .Build()
        .Run()
  0
