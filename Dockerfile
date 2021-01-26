FROM mcr.microsoft.com/dotnet/sdk:5.0-alpine AS build

WORKDIR /app

# Copy csproj and restore as distinct layers
COPY App/*.fsproj ./App/
COPY Core/*.fsproj ./Core/
RUN dotnet restore ./App

# Copy everything else and build
COPY App/. ./App/
COPY Core/. ./Core/
RUN dotnet publish App -c Release -o out

# Build runtime image
FROM mcr.microsoft.com/dotnet/aspnet:5.0-alpine AS base
WORKDIR /app
COPY --from=build /app/out .
ENTRYPOINT ["dotnet", "App.dll"]