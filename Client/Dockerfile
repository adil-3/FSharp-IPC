FROM mcr.microsoft.com/dotnet/runtime:7.0 AS base
WORKDIR /app

FROM mcr.microsoft.com/dotnet/sdk:7.0 AS build
WORKDIR /src
COPY ["Client/Client.fsproj", "Client/"]
RUN dotnet restore "Client/Client.fsproj"
COPY . .
WORKDIR "/src/Client"
RUN dotnet build "Client.fsproj" -c Release -o /app/build

FROM build AS publish
RUN dotnet publish "Client.fsproj" -c Release -o /app/publish

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "Client.dll"]
