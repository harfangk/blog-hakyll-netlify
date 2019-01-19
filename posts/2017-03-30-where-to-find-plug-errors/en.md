---
title: How to Display Plug Errors
---

This post is aimed at those who are just starting to use `Plug`, a great web middleware for Elixir language.

## TL;DR

If you want to access various error messages from `Plug` that do not show up in `Logger`, `Plug.Debugger` will make those failed requests appear. Check out [Plug.Debugger](https://hexdocs.pm/plug/Plug.Debugger.html#content).

<!--more-->

## 404 Error Message Not Found

While building an app that received JSON through HTTP POST requests, I noticed an interesting behavior. When I send a request with malformed JSON payload, the request would magically banish without any trace in either the log or the server.

So when I send a request with malformed JSON like:

```json
{
  "
}
```

The server would respond with `500 Internal Server Error` without displaying or logging internal error message.

After some digging, I found out that the sort of error I was looking for - `Plug.Parsers.ParseError` - is not logged by default. That makes sense - I don't want my log to be flooded with error messages about potentially unlimited number of bad requests. But what if I actually wanted to check the error message?

The solution is to use `Plug.Debugger` in the module that uses `Plug.Router`.

The following is part of the code for the web interface of my app. Notice the `use Plug.Debugger` I included here. 

```elixir
defmodule UrlShortener.Web do
  use Plug.Router
  use Plug.Debugger

  plug Plug.Parsers, parsers: [:json],
                     pass: ["application/vnd.api+json"],
                     json_decoder: Poison

  plug :match
  plug :dispatch

  get "/" do
    conn
    |> send_resp(200, "Url shortener microservice app. Refer to https://github.com/harfangk/url_shortener for further information.")
  end

  ...
  
end
```

After calling that macro, your server now internally logs the error message. 

```elixir
02:58:25.008 [debug] ** (Plug.Parsers.ParseError) malformed request, a Poison.SyntaxError exception was raised with message "Unexpected end of input"
    (plug) lib/plug/parsers/json.ex:54: Plug.Parsers.JSON.decode/2
    (plug) lib/plug/parsers.ex:210: Plug.Parsers.reduce/6
    (url_shortener) lib/url_shortener/web.ex:1: UrlShortener.Web.plug_builder_call/2
    (url_shortener) lib/plug/debugger.ex:123: UrlShortener.Web.call/2
    (plug) lib/plug/adapters/cowboy/handler.ex:15: Plug.Adapters.Cowboy.Handler.upgrade/4
    (cowboy) /Users/bonghyunkim/Documents/Projects/url_shortener/deps/cowboy/src/cowboy_protocol.erl:442: :cowboy_protocol.execute/4
```

It also responds to the HTTP request with an error message, so that could be another way for debugging your app.
