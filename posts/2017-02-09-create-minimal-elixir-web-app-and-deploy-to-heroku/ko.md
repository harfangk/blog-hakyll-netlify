---
title: 플러그와 카우보이만 사용해서 미니멀한 엘릭서 웹 앱을 만들고 허로쿠에 배포하기
---

이 글에서는 얼랭/OTP로 만들어진 HTTP서버인 카우보이(Cowboy), 그리고 엘릭서로 만들어진 조합형 웹 미들웨어인 플러그(Plug)만 사용해서 미니멀한 엘릭서 웹 앱을 만들고 이를 허로쿠에 배포하는 방법을 소개합니다.

이 글을 읽는 이는 미니멀한 웹 앱에 대해서 찾아볼 정도의 사람이니 엘릭서와 웹 개발에 대한 기본적인 지식을 갖추고 있다고 가정하겠습니다.

<!--more-->

글 작성 시점에서 엘릭서 1.4.1 버전을 사용하고 있습니다. 엘릭서 1.4에서 어플리케이션 디렉토리 구조와 `mix.exs` 파일 설정이 바뀌었으므로 설정 차이점을 최소화하기 위해서 엘릭서 1.4 버전 이상을 사용할 것을 권장합니다.

완성된 샘플 앱의 소스 코드는 [여기](https://github.com/harfangk/timeconverter)에서 받을 수 있습니다. 완성되어서 구동 중인 허로쿠 앱은 [여기](https://blooming-thicket-28926.herokuapp.com/) 있습니다. 

## 엘릭서 앱 만들기

날짜/시간을 iso8601 형식과 unix 형식 사이에서 변환할 수 있는 매우 간단한 앱을 만들 것입니다.

먼저 쉘에서 `mix new timeconverter --sup`을 실행해서 수퍼비전 트리를 내장한 새 엘릭서 앱을 만듭니다.

{% highlight bash %}
$ mix new timeconverter --sup
* creating README.md
* creating .gitignore
* creating mix.exs
* creating config
* creating config/config.exs
* creating lib
* creating lib/timeconverter.ex
* creating lib/timeconverter/application.ex
* creating test
* creating test/test_helper.exs
* creating test/timeconverter_test.exs

Your Mix project was created successfully.
You can use "mix" to compile it, test it, and more:

    cd timeconverter
    mix test

Run "mix help" for more commands.
{% endhighlight %}

## HTTP 서버 추가하기

카우보이와 플러그를 설치합니다. `mix.exs`를 열어서 해당 패키지를 추가해주세요.

{% highlight elixir %}
defmodule Timeconverter.Mixfile do
  ...
  defp deps do
    [
      {:cowboy, "~> 1.0"},
      {:plug, "~> 1.0"}
    ]
  end
 end
{% endhighlight %}

그리고 `mix deps.get`을 실행해서 필요한 패키지를 설치합니다.

이어서 앱의 HTTP 인터페이스 기능을 수행할 `Timeconverter.Router` 모듈을 생성합니다. `router.ex` 파일을 `lib/timeconverter` 디렉토리 안에 생성한 뒤에 다음 코드를 입력해 주세요.

{% highlight elixir %}
defmodule Timeconverter.Router do
  use Plug.Router
  
  plug :match
  plug :dispatch

  get "/" do
    conn
    |> send_resp(200, "wow!")
  end

  match _ do
    conn
    |> send_resp(200, "woooow!")
  end
  
  def start_link do
    {:ok, _} = Plug.Adapters.Cowboy.http(Timeconverter.Router, [])
  end
end
{% endhighlight %}

`Plug.Router`는 HTTP 요청에 응답하는 경로를 생성할 수 있는 매크로를 제공합니다. `match`와 `dispatch`는 `Plug.Router`를 사용할 때 반드시 넣어야 하는 플러그들입니다. 

`get "/"`는 루트 주소에 대한 GET 요청에만 반응합니다. 그 외의 모든 요청은 `match _`로 연결됩니다.

`Plug.Router`에 대한 문서는 [여기](https://hexdocs.pm/plug/Plug.Router.html#content)에서 살펴보세요.

`start_link/3`는 `Plug.Router`가 아니라 `GenServer` 모듈에 정의된 함수입니다. `Timeconverter.Router` 모듈이 수퍼비전 트리 안에서 실행되었을 때 어떤 일을 할 지 지정합니다. 이 앱에서는 카우보이를 HTTP 모드로 실행하도록 설정해두었습니다. 

앱이 실행되면 `Timeconverter.Router`도 자동으로 실행되도록 어플리케이션 수퍼비전 트리에 넣어줍니다. `lib/timeconverter/application.ex`를 열고 `Timeconverter.Router` 워커를 추가합니다.

{% highlight elixir %}
defmodule Timeconverter.Application do
  ...
  def start(_type, _args) do
    ...
    children = [
      # Starts a worker by calling: Timeconverter.Worker.start_link(arg1, arg2, arg3)
      # worker(Timeconverter.Worker, [arg1, arg2, arg3]),
      worker(Timeconverter.Router, [])
    ]
    ...
  end
end
{% endhighlight %}

이제 Timeconverter 앱이 실행되면 카우보이 HTTP 서버도 실행될 것입니다.

쉘에서 `mix run --no-halt`를 실행해보세요. 혹시 `rebar` 패키지를 설치해야한다는 안내가 뜨면 설치하면 됩니다. 앱이 실행되면 `localhost:4000`을 브라우저에서 실행해보세요. 앱이 성공적으로 구동되었다면 "wow!"라는 간단한 텍스트를 보여줄 것입니다.

## 날짜/시간 형식을 변환하는 도메인 로직 추가하기

미니멀한 웹 앱을 만들고 배포하는 것이 이 글의 목적이니 도메인 로직은 대충 건너뛰도록 하겠습니다. 어차피 별로 재미도 없는 예시니까요.

`lib/timeconverter.ex` 파일의 내용을 다음 코드로 대체해 주세요. 글 서두에 적었듯이 이 코드는 깃헙에서도 제공하고 있습니다.

{% highlight elixir %}
defmodule Timeconverter do
  @moduledoc """
  Documentation for Timeconverter.
  """
  
  def convert_datetime(%{"format" => format, "time" => time}) do
    case format do
      "to_unix"    -> iso8601_to_unix(time)
      "to_iso8601" -> unix_to_iso8601(time)
      _            -> {:error, ~s{Format should be either "to_unix" or "to_iso8601"}}
    end
  end
  def convert_datetime(%{}), do: {:error, ~s{time and format parameters should be specified. Format should be either "to_unix" or "to_iso8601", and time should be in either of those formats.} }

  defp iso8601_to_unix(time) do
    valid_iso8601 = ~r/^(?:[0-9]\d{3}-(?:(?:0[1-9]|1[0-2])-(?:0[1-9]|1\d|2[0-8])|(?:0[13-9]|1[0-2])-(?:29|30)|(?:0[13578]|1[02])-31)|(?:[1-9]\d(?:0[48]|[2468][048]|[13579][26])|(?:[2468][048]|[13579][26])00)-02-29)T(?:[01]\d|2[0-3]):[0-5]\d:[0-5]\d(?:Z|[+-][01]\d:[0-5]\d)$/
    case Regex.match?(valid_iso8601, time) do
      true -> {:ok, time |> DateTime.from_iso8601() |> elem(1) |> DateTime.to_unix()}
      _    -> {:error, ~s{Invalid format for iso8601 input. Should be like "2015-01-17T21:23:02+00:00"}}
    end
  end

  defp unix_to_iso8601(time) do
    valid_unix = ~r/^[-+]?\d+$/
    case Regex.match?(valid_unix, time) do
      true -> {:ok, time |> String.to_integer() |> DateTime.from_unix!() |> DateTime.to_iso8601()}
      _    -> {:error, ~s{Invalid format for unix input. Should be like "1464096368"}}
    end
  end
end
{% endhighlight %}

Regex와 DateTime 모듈의 함수를 사용하는 별로 특별할 것 없는 코드입니다. 이제 HTTP 경로와 도메인 로직을 연결해봅시다. `lib/timeconverter/router.ex` 파일을 열고 경로 관련 함수를 변경해주세요.

{% highlight elixir %}
defmodule Timeconverter.Router do
  ...
  get "/" do
    conn = Plug.Conn.fetch_query_params(conn)
    result = inspect(Timeconverter.convert_datetime(conn.params))
    conn
    |> send_resp(200, result)
  end

  match _ do
    conn
    |> send_resp(404, "This is not the page you are looking for")
  end
  ...
end
{% endhighlight %}

`Plug.Conn`은 HTTP 요청과 응답을 나타낸다고 보면 됩니다. 각 `Plug.Conn` 스트럭트에는 하나의 HTTP 연결에 관련된 모든 유용한 정보가 포함되어 있으며 필요에 따라 그 정보를 사용할 수 있습니다.

파라미터 값은 `conn.params`에서 받을 수 있는데, `Plug.Conn.fetch_query_params/1` 함수를 실행한 이후에만 쿼리 파라미터를 받을 수 있습니다. 쿼리 파라미터를 받은 뒤 `conn.params`를 도메인 로직 함수인 `Timeconverter.convert_datetime/1`에 전달합니다. HTTP 엔드포인트 레이어와 도메인 로직을 이렇게 간단히 연결할 수 있습니다.

앱을 종료한 뒤 다시 실행해보세요. 이제 앱이 `time`과 `format` 쿼리 파라미터에 따라 다르게 반응할 것입니다.

## 허로쿠에 배포하기

이 단계를 진행하기 위해서는 허로쿠 계정과 커맨드 라인 인터페이스가 필요합니다. 아직 설치하지 않았다면 [허로쿠 개발자 센터](https://devcenter.heroku.com/articles/heroku-cli)에 나온대로 설치 및 설정을 해주세요.

허로쿠에 배포하기 위해서는 빌드팩이 필요합니다. 허로쿠에서 엘릭서를 공식적으로 지원하지는 않지만 HashNuke가 제작한 엘릭서용 오픈소스 빌드팩이 있습니다. 그 빌드팩을 사용해서 허로쿠에 배포하도록 하겠습니다. HashNuke의 빌드팩 관련 문서는 [여기](https://github.com/HashNuke/heroku-buildpack-elixir)에서 살펴 보세요.

배포하기 전에 몇 가지 더 준비할 것이 있습니다. 먼저 앱의 루트 디렉토리에 `elixir_buildpack.config` 파일을 생성하고, 다음 설정을 입력해주세요.

{% highlight bash %}
# Erlang version
erlang_version=19.2

# Elixir version
elixir_version=1.4.1
{% endhighlight %}

엘릭서 버전을 명시하지 않을 경우 빌드팩에서는 2017년 2월 시점에서 엘릭서 1.3 버전을 기본적으로 사용합니다. 우리 앱은 엘릭서 1.4를 사용하고 있으므로 엘릭서 버전을 빌드팩에 명시해줘야 합니다. 그렇지 않으면 빌드에 실패합니다.

이어서 `lib/timeconverter/router.ex` 파일을 열고 허로쿠용 포트 설정을 추가해주세요.

{% highlight elixir %}
defmodule Timeconverter.Router do
  ...
  def start_link do
    {:ok, _} = Plug.Adapters.Cowboy.http(Timeconverter.Router, [], port: get_port())
  end
  ...
  defp get_port() do
    port_env_variable = System.get_env("PORT")
    if is_nil(port_env_variable) do
      4000
    else
      String.to_integer(port_env_variable)
    end
  end
end
{% endhighlight %}

허로쿠는 환경 변수 PORT를 통해서 앱이 사용할 포트를 지정해줍니다. `System.get_env("PORT")` 함수를 사용해서 지정된 포트 값을 받아와야 앱을 허로쿠에서 실행할 수 있습니다. `get_port/0`의 `if`문은 PORT 환경 변수가 없는 로컬 환경에서 앱을 실행할 때에도 포트 넘버가 지정되도록 해줍니다.

앱의 루트 디렉토리로 가서 `heroku create --buildpack "https://github.com/HashNuke/heroku-buildpack-elixir.git"`을 실행해서 빌드팩을 통해서 허로쿠 앱을 생성합니다.

스크립트가 허로쿠 앱을 만들고 heroku라는 깃 리모트 리포도 자동으로 설정할 것입니다. `git push heroku master`를 실행하면 앱이 허로쿠로 배포됩니다. `heroku open`을 실행해서 앱을 웹브라우저에서 열어보고 제대로 동작하는지 확인해보세요.
