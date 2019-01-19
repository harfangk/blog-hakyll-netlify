---
title: Programming Phoenix에서 버전 때문에 달라진 예제 코드 모음
---

지금 Programming Phoenix 출판본에 있는 예제 코드를 따라해보면 일부 코드에서 문제가 발생합니다. 에러가 나기도 하고, 테스트를 통과하지 않을 때도 있고, 함수 지원 중단 경고가 뜨기도 합니다. 책에서 문제가 발생하는 예제 코드를 모아서 해결책을 정리해 보았습니다.

<!--more-->

예제 코드에서 문제가 발생하는 이유는 설치된 Phoenix 버전과 책에서 사용된 버전이 다르기 때문입니다. 지금 판본의 책은 Phoenix 1.1.x 버전을 바탕으로 쓰였지만 책에 나온 설치 명령을 실행하면 최신 1.2.x 버전이 설치됩니다.

예제 코드에 문제가 발생하는 것 자체를 피하고 싶으시면 그냥 아래 커맨드를 사용해서 Phoenix 1.1.6을 설치하시면 됩니다.
```bash
$ mix archive.install https://github.com/phoenixframework/archives/raw//master/phoenix_new-1.1.6.ez
```

저자들의 말에 따르면 Phoenix 1.3이 출시되면 그에 맞춰서 책을 개정한다고 하니 그 이후로는 버전 문제로 인해 예제 코드에서 에러가 발생하지 않게 될 것 같습니다.

추가: 이 글을 작성할 당시에는 엘릭서 1.3.4 버전을 사용했는데 최근에 엘릭서가 1.4 버전으로 업데이트되면서 `mix.exs` 파일의 내용이 조금 바뀌었고, `mix new MyApp --sup` 명령어가 생성하는 디렉토리 구조도 조금 바뀌었습니다. 하지만 책에 있는 예제를 따라하는 데는 영향이 없습니다.

### Building Forms, Chapter 4 (PDF page 60)
*rumbl/web/models/user.ex*:
```elixir
def changeset(model, params \\ :empty) do 
  model 
  |> cast(params, ~w(name username), []) 
  |> validate_length(:username, min: 1, max: 20) 
end 
```

위 코드에서 `cast/4`를 `cast/3` + `validate_required/3`로 바꿔주세요. 

```elixir
def changeset(model, params \\ :empty) do
  model
  |> cast(params, [:name, :username, :password])
  |> validate_required([:name, :username])
  |> validate_length(:username, min: 1, max: 20)
end
```

`cast/4`는 지원 중단 예정입니다.  
[Ecto 문서](https://hexdocs.pm/ecto/Ecto.Changeset.html#cast/4)  
[Phoenix 문서](https://github.com/phoenixframework/phoenix/issues/1564)

### Building Forms, Chapter 4 (PDF page 60-61)
*rumbl/web/models/user.ex*:

```elixir
def changeset(model, params \\ :empty) do 
  model 
  |> cast(params, ~w(name username), []) 
  |> validate_length(:username, min: 1, max: 20) 
end 
```

위 코드에서 `changeset/2`의 인자를 변경해주세요.

```elixir
def changeset(model, params \\ %{}) do 
  model 
  |> cast(params, ~w(name username), []) 
  |> validate_length(:username, min: 1, max: 20) 
end 
```

Ecto 2.0부터 `:empty`가 `%{}`로 대체되었습니다. `:empty`를 사용해도 아직 작동은 하더군요. 왠지 모르겠지만 나중에 나오는 `Video`와 `Category` 모델에는 저자들이 `%{}`를 사용했습니다. 이후 챕터에도 몇 번 보이는데 심각한 문제가 아니기 때문에 재차 언급하지는 않겠습니다.

### Presenting User Account Links, Chapter 5 (PDF page 86)
*rumbl/web/templates/layout/app.html.eex* 를 열어보면 아래와 같은 코드가 있을 겁니다.
```erb
<header class="header">
  <nav role="navigation">
    <ul class="nav nav-pills pull-right">
      <li><a href="http://www.phoenixframework.org/docs">Get Started</a></li>
    </ul>
  </nav>
  <span class="logo"></span>
</header>
```

`div` 대신 `header` 태그를 쓰거나 `ol` 대신 `ul`을 쓰고 다른 클래스를 지정하는 등 책에 나와있는 코드랑은 조금 다릅니다. Phoenix가 업데이트 되면서 자동생성된 코드가 변경된 것 같습니다. 저자는 아래와 같이 파일을 수정하라고 합니다. 

```erb
<div class="header">  <ol class="breadcrumb text-right">    <%= if @current_user do %>      <li><%= @current_user.username %></li>
      <li>        <%= link "Log out", to: session_path(@conn, :delete, @current_user),
         method: "delete" %>      </li>    <% else %>      <li><%= link "Register", to: user_path(@conn, :new) %></li> 
      <li><%= link "Log in", to: session_path(@conn, :new) %></li>    <% end %> </ol>  <span class="logo"></span>
</div>
```

저는 아래 나온 것처럼 했습니다. 렌더링된 페이지가 조금 다르긴 하지만 별 문제 없이 동작하더군요.

```erb
<header class="header">
  <nav role="navigation">
    <ul class="nav nav-pills pull-right">
      <%= if @current_user do %>
        <li><%= @current_user.username %></li>
        <li><%= link "Log out", to: session_path(@conn, :delete, current_user), 
        method: "delete" %></li>
      <% else %>  
        <li><%= link "Register", to: user_path(@conn, :new) %></li>
        <li><%= link "Log in", to: session_path(@conn, :new) %></li>
      <% end %>
    </ul>
  </nav>
  <span class="logo"></span>
</header>
```

### Examining the Generated Controller and View, Chapter 6 (PDF page 97)
책을 보면 *rumbl/web/controllers/video_controller.ex* 파일에 아래와 같은 내용이 있을 거라고 되어있습니다.
```elixir
plug :scrub_params, "video" when action in [:create, :update]
```

저 코드는 Ecto 2.0 와의 호환성을 위해 더 이상 컨트롤러 안에 자동으로 추가되지 않도록 바뀌었습니다.  
[Phoenix 문서](https://github.com/phoenixframework/phoenix/issues/1564)

### Testing Logged-Out Users, Chapter 8 (PDF page 135-137)
`test "requires user authentication on all actions"`가 실패한다면 저와 비슷한 실수를 했을 수도 있습니다. *rumbl/web/router.ex* 파일을 확인해보시고, 아래와 같이 되어 있나 살펴보세요.

```elixir
scope "/", Rumbl do
  pipe_through :browser # Use the default browser stack

  resources "/videos", VideoController
  resources "/users", UserController, only: [:index, :show, :new, :create]
  resources "/session", SessionController, only: [:new, :create, :delete]
  get "/", PageController, :index
end

scope "/manage", Rumbl do
  pipe_through [:browser, :authenticate_user]

  resources "/videos", VideoController
end
```

`scope "/", Rumbl do … end` 블럭 부분에 있는 `resources "videos", VideoController` 줄을 삭제해주세요. Generating Resources, Chapter 6 (PDF page 92-93)에 나온 제너레이터 결과 텍스트를 잘못 읽고 `resources "/videos", VideoController`를 *rumbl/web/router.ex*에 추가해서 발생한 문제입니다.

### Testing Side Effect-Fere Model Code, Chapter 8 (PDF page 149-150) 
*rumbl/test/models/user_test.exs*

```elixir
test "changeset does not accept long usernames" do
  attrs = Map.put(@valid_attrs, :username, String.duplicate("a", 30))
  assert {:username, {"should be at most %{count} character(s)", [count: 20]}} in 
  errors_on(%User{}, attrs)
end 
```

assertion 부분 코드를 아래와 같이 바꿔주세요.

```elixir
test "changeset does not accept long usernames" do
  attrs = Map.put(@valid_attrs, :username, String.duplicate("a", 30))
  assert {:username, "should be at most 20 character(s)"} in 
  errors_on(%User{}, attrs)
end 
```

*rumbl/test/support/model_case.ex*에 정의되어 있는 `errors_on/2` 함수가 바뀌어서 발생한 문제입니다. 
	
### Testing Code with Side Effects, Chapter 8 (PDF page 152)
In *rumbl/test/models/user_repo_test.exs*
```elixir
test "converts unique_constraint on username to error" do 
  insert_user(username: "eric")
  attrs = Map.put(@valid_attrs, :username, "eric") 
  changeset = User.changeset(%User{}, attrs) 
  assert {:error, changeset} = Repo.insert(changeset) 
  assert {:username, "has already been taken"} in changeset.errors 
end 
```

마지막 assertion 부분 코드를 아래와 같이 변경해주세요.

```elixir
  test "converts unique_constraint on username to error" do
    insert_user(username: "eric")
    attrs = Map.put(@valid_attrs, :username, "eric")
    changeset = User.changeset(%User{}, attrs)

    assert {:error, changeset} = Repo.insert(changeset)
    assert {:username, {"has already been taken", []}} in changeset.errors
  end
```

`assert/2` 함수가 작동하는 방식이 바뀐 것 같습니다.

### Isolating Wolfram, Chapter 13 (PDF page 247)
*rumbrella/apps/info_sys/test/backends/http_client.exs*:
 
```elixir
defmodule InfoSys.Test.HTTPClient do
  @wolfram_xml File.read!("test/fixtures/wolfram.xml")
  def request(url) do
    url = to_string(url)
    cond do
      String.contains?(url, "1+%2B+1") -> {:ok, {[], [], @wolfram_xml}}
      true -> {:ok, {[], [], "<queryresult></queryresult>"}}
    end
  end
end
```

`cond` 블럭에 있는 첫 번째 매치 케이스를 다음과 같이 바꿔주세요.

```elixir
defmodule InfoSys.Test.HTTPClient do
  @wolfram_xml File.read!("test/fixtures/wolfram.xml")
  def request(url) do
    url = to_string(url)
    cond do
      String.contains?(url, "1%20+%201") -> {:ok, {[], [], @wolfram_xml}}
      true -> {:ok, {[], [], "<queryresult></queryresult>"}}
    end
  end
end
```

*rumbrella/apps/info_sys/lib/info_sys/wolfram.ex*의 `fetch_xml/1`에 있는 `URI.encode("1 + 1")`를 `iex`에서 호출해보세요. 책에 나온 `"1+%2B+1"`가 아니라 `"1%20+%201"`가 결과로 나옵니다. 이 때문에 `InfoSys.Test.HTTPClient.request/1`에 있는 `cond` 케이스 매치에 실패하는 겁니다.

전 여기서 `wolfram.xml` 스텁 파일을 만들 때도 한 가지 실수를 했습니다. 어쩐지 책에서 스텁 파일이 `</queryresult>`로 끝나지 않아서 의아하게 여겼는데, 알고 보니 책에 나온 내용은 원래 스텁 파일의 앞부분 몇 줄만 나와있는 것이었습니다. 파일 이름을 클릭해서 실제 파일을 확인해보시면 137줄짜리 XML 파일이 나오니 그걸 전부 복사해주세요.

### Isolating Wolfram, Chapter 13 (PDF page 248)
*rumbrella/apps/rumbl/test/test_helper.exs*:

```elixir
Code.require_file "../../info_sys/test/backends/http_client.exs", __DIR__ 
ExUnit.start 
	
Mix.Task.run "ecto.create", ~w(-r Rumbl.Repo --quiet) 
Mix.Task.run "ecto.migrate", ~w(-r Rumbl.Repo --quiet) 
Ecto.Adapters.SQL.begin_test_transaction(Rumbl.Repo) 
```

생성된 파일의 내용이 책에 나온 것이랑 조금 다릅니다. 그냥 책에서 지시하는 대로 `http_client.exs`를 불러오는 경로만 입력해주면 문제 없이 작동합니다.

```elixir
Code.require_file "../../info_sys/test/backends/http_client.exs", __DIR__
ExUnit.start
	
Ecto.Adapters.SQL.Sandbox.mode(Rumbl.Repo, :manual)
```
