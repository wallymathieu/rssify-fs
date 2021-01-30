# rssify-fs

Turning sites into feeds

```bash
(cd App; export ASPNETCORE_URLS=http://localhost:5000; dotnet run)
```

In order to use Rssify you can write some F# to turn the parameters into query form:

```f#
open System
open System.Net
let query =
        [ "link", "https://somesite.com"
          "feedtitle", "Some site"
          "next", ".next"
        ]
        |> List.map ( fun (k,v) -> sprintf "%s=%s" k (WebUtility.UrlEncode v) )
        |> String.concat "&"
let url = "http://localhost:5000/rss?"+query
```

## Development

Using Docker:

```bash
docker-compose build
docker-compose start
docker-compose stop service ; docker-compose up -d --build service
```

Locally:

```bash
(cd App; export ASPNETCORE_URLS=http://localhost:5000; dotnet run --watch)
```

## Deploy

See [docs](https://docs.microsoft.com/en-us/aspnet/core/host-and-deploy/linux-nginx?view=aspnetcore-5.0)
