---
title: Plug 에러 메시지를 표시하는 방법
---

`Plug`는 엘릭서 언어로 작성된 뛰어난 웹 미들웨어입니다. 이 글은 `Plug`를 사용하기 시작한 지 얼마 되지 않은 사람들을 대상으로 작성했습니다.

## TL;DR

`Plug`에서 발생하는 에러 메시지를 확인하고 싶지만 `Logger`에 나타나지 않을 경우, `Plug.Debugger`를 사용하면 실패한 요청을 출력하도록 할 수 있습니다. [Plug.Debugger 관련 문서](https://hexdocs.pm/plug/Plug.Debugger.html#content)를 확인해 보세요.

<!--more-->

## 404 Error Message Not Found

HTTP POST 요청을 통해서 JSON을 받는 앱을 만드는 과정에서 재밌는 현상을 목격했습니다. 잘못된 형식의 JSON을 담은 요청을 보내면 그 요청이 로그나 서버에 아무런 기록도 남기지 않고 감쪽같이 사라졌습니다.

즉, 다음과 같이 잘못된 형식의 JSON을 담은 요청을 보낸다고 합시다.

```json
{
  "
}
```

그러면 서버는 내부 에러 메시지를 표시하거나 로그에 기록하지 않고, 단순히 요청에 `500 Internal Server Error`로만 응답합니다.

좀 검색을 해보니 제가 찾던 `Plug.Parsers.ParseError` 같은 에러는 로그에 기록하지 않는 것이 기본 설정이란 것을 알게 되었습니다. 생각해보니 합리적인 판단 같습니다. 잘못된 요청이 몇 개가 올지 알 수도 없는데 제 로그가 그런 오류 메시지로 가득차는 것은 저도 바라지 않습니다. 하지만 그 에러 메시지를 확인해보고 싶다면 어떻게 해야 할까요?

해결책은  `Plug.Router`를 사용하는 모듈에 `Plug.Debugger`를 사용하는 것입니다.

다음은 제 앱의 웹 인터페이스 관련 코드 중 일부입니다. 코드 중에 `use Plug.Debugger`를 넣어두었습니다.

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

저 매크로를 추가한 뒤에 코드를 컴파일해서 서버를 실행하면 서버가 내부적으로 에러 메시지를 로그에 기록하기 시작합니다.

```elixir
02:58:25.008 [debug] ** (Plug.Parsers.ParseError) malformed request, a Poison.SyntaxError exception was raised with message "Unexpected end of input"
    (plug) lib/plug/parsers/json.ex:54: Plug.Parsers.JSON.decode/2
    (plug) lib/plug/parsers.ex:210: Plug.Parsers.reduce/6
    (url_shortener) lib/url_shortener/web.ex:1: UrlShortener.Web.plug_builder_call/2
    (url_shortener) lib/plug/debugger.ex:123: UrlShortener.Web.call/2
    (plug) lib/plug/adapters/cowboy/handler.ex:15: Plug.Adapters.Cowboy.Handler.upgrade/4
    (cowboy) /Users/bonghyunkim/Documents/Projects/url_shortener/deps/cowboy/src/cowboy_protocol.erl:442: :cowboy_protocol.execute/4
```

참고로 저 매크로를 사용하면 서버에서 에러를 보여주는 것 뿐 아니라, HTTP 요청에도 구체적인 에러 메시지를 응답으로 보내줍니다. 디버깅할 때 편리합니다.
