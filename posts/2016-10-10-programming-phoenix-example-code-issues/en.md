---
title: List of Outdated Example Code in Programming Phoenix
---

If you follow the code examples in Programming Phoenix book now, there are several broken ones that will cause error, fail to pass test, or show deprecation warning. I documented such codes from the book and how to resolve them.

<!--more-->

Those codes break because your installed Phoenix version and the one used in the book is different. The book uses 1.1.x, but if you follow the installation instruction, it will install the most recent version of Phoenix like 1.2.x.  

If you do not want to deal with this issue at all, install Phoenix 1.1.6 with this command.
{% highlight bash %}
$ mix archive.install https://github.com/phoenixframework/archives/raw//master/phoenix_new-1.1.6.ez
{% endhighlight %}

The authors of the book plan to update the book when Phoenix 1.3 is released, which will fix these broken examples for good.

Edit: At the time of writing I used Elixir 1.3.4. More recent Elixir 1.4 slightly changes the content of `mix.exs` and the directory structure of app created by running `mix new MyApp --sup`. But they are both minor changes and won't affect following the sample code.

### Building Forms, Chapter 4 (PDF page 60)
In *rumbl/web/models/user.ex*:
{% highlight elixir %}
def changeset(model, params \\ :empty) do 
  model 
  |> cast(params, ~w(name username), []) 
  |> validate_length(:username, min: 1, max: 20) 
end 
{% endhighlight %}

Replace `cast/4` with `cast/3` + `validate_required/3`. Change it to:

{% highlight elixir %}
def changeset(model, params \\ :empty) do
  model
  |> cast(params, [:name, :username, :password])
  |> validate_required([:name, :username])
  |> validate_length(:username, min: 1, max: 20)
end
{% endhighlight %}

`cast/4` is deprecated.  
[Ecto docs](https://hexdocs.pm/ecto/Ecto.Changeset.html#cast/4)  
[Phoenix docs](https://github.com/phoenixframework/phoenix/issues/1564)

### Building Forms, Chapter 4 (PDF page 60-61)
In *rumbl/web/models/user.ex*:

{% highlight elixir %}
def changeset(model, params \\ :empty) do 
  model 
  |> cast(params, ~w(name username), []) 
  |> validate_length(:username, min: 1, max: 20) 
end 
{% endhighlight %}

Change the argument of `changeset/2` to:

{% highlight elixir %}
def changeset(model, params \\ %{}) do 
  model 
  |> cast(params, ~w(name username), []) 
  |> validate_length(:username, min: 1, max: 20) 
end 
{% endhighlight %}

`:empty` is replaced by `%{}` in Ecto 2.0. It will still work with `:empty`, though. Strangely, `Video` and `Category` models that show up in later chapters use `%{}`. This one comes up a few more times in the later chapters.

### Presenting User Account Links, Chapter 5 (PDF page 86)
Your *rumbl/web/templates/layout/app.html.eex* will initially look like this:
{% highlight erb %}
<header class="header">
  <nav role="navigation">
    <ul class="nav nav-pills pull-right">
      <li><a href="http://www.phoenixframework.org/docs">Get Started</a></li>
    </ul>
  </nav>
  <span class="logo"></span>
</header>
{% endhighlight %}

This is slightly different from what is expected in the book. For example, it uses `header` tag instead of `div` tag, and `ul` instead of `ol` with different class. The authors instruct us to change it like this.

{% highlight erb %}
<div class="header">  <ol class="breadcrumb text-right">    <%= if @current_user do %>      <li><%= @current_user.username %></li>
      <li>        <%= link "Log out", to: session_path(@conn, :delete, @current_user),
         method: "delete" %>      </li>    <% else %>      <li><%= link "Register", to: user_path(@conn, :new) %></li> 
      <li><%= link "Log in", to: session_path(@conn, :new) %></li>    <% end %> </ol>  <span class="logo"></span>
</div>
{% endhighlight %}

This is what I did. The rendered page looks slightly different, but it works fine.

{% highlight erb %}
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
{% endhighlight %}

### Examining the Generated Controller and View, Chapter 6 (PDF page 97)
The book mentions that in *rumbl/web/controllers/video_controller.ex*, there should be this line:

{% highlight elixir %}
plug :scrub_params, "video" when action in [:create, :update]
{% endhighlight %}

That line is no longer automatically included in the controller because of compatibility with Ecto 2.0 update.  
[Phoenix docs](https://github.com/phoenixframework/phoenix/issues/1564)

### Testing Logged-Out Users, Chapter 8 (PDF page 135-137)
If `test "requires user authentication on all actions"` fails, it's likely that you made a similar mistake as I did. Check your *rumbl/web/router.ex* and see if it look like this.
{% highlight elixir %}
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
{% endhighlight %}

Delete the line `resources "videos", VideoController` in `scope "/", Rumbl do … end` block. You probably misread the output result of resource generation in Generating Resources, Chapter 6 (PDF page 92-93) and understood it as an instruction to add `resources "/videos", VideoController` to *rumbl/web/router.ex*.

### Testing Side Effect-Fere Model Code, Chapter 8 (PDF page 149-150) 
In *rumbl/test/models/user_test.exs*

{% highlight elixir %}
test "changeset does not accept long usernames" do
  attrs = Map.put(@valid_attrs, :username, String.duplicate("a", 30))
  assert {:username, {"should be at most %{count} character(s)", [count: 20]}} in 
  errors_on(%User{}, attrs)
end 
{% endhighlight %}

The assertion should be changed to

{% highlight elixir %}
test "changeset does not accept long usernames" do
  attrs = Map.put(@valid_attrs, :username, String.duplicate("a", 30))
  assert {:username, "should be at most 20 character(s)"} in 
  errors_on(%User{}, attrs)
end 
{% endhighlight %}

This issue is caused by changes to the definition of `errors_on/2` in *rumbl/test/support/model_case.ex*.
	
### Testing Code with Side Effects, Chapter 8 (PDF page 152)
In *rumbl/test/models/user_repo_test.exs*
{% highlight elixir %}
test "converts unique_constraint on username to error" do 
  insert_user(username: "eric")
  attrs = Map.put(@valid_attrs, :username, "eric") 
  changeset = User.changeset(%User{}, attrs) 

  assert {:error, changeset} = Repo.insert(changeset) 
  assert {:username, "has already been taken"} in changeset.errors 
end 
{% endhighlight %}

The last assertion should be changed to

{% highlight elixir %}
test "converts unique_constraint on username to error" do
  insert_user(username: "eric")
  attrs = Map.put(@valid_attrs, :username, "eric")
  changeset = User.changeset(%User{}, attrs)

  assert {:error, changeset} = Repo.insert(changeset)
  assert {:username, {"has already been taken", []}} in changeset.errors
  end
{% endhighlight %}

I think it's because of how `assert/2` works. 

### Isolating Wolfram, Chapter 13 (PDF page 247)
In *rumbrella/apps/info_sys/test/backends/http_client.exs*:
 
{% highlight elixir %}
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
{% endhighlight %}

See the first match case for `cond` block. It should be:

{% highlight elixir %}
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
{% endhighlight %}

Try `URI.encode("1 + 1")` in `iex`, just like it's called in `fetch_xml/1` in *rumbrella/apps/info_sys/lib/info_sys/wolfram.ex*. That returns `"1%20+%201"`, not `"1+%2B+1"` as in the book. This is why the `cond` match in `InfoSys.Test.HTTPClient.request/1` fails.
	
I made another mistake here when creating the stub `wolfram.xml` . I thought it was weird that the stub did not end with `</queryresult>` tag. Well, it turned out that what you can see in the book was only the first several lines of the actual stub file. Click the link on the file name to get the entire xml file that's 137 lines long.

### Isolating Wolfram, Chapter 13 (PDF page 248)
In *rumbrella/apps/rumbl/test/test_helper.exs*:
{% highlight elixir %}
Code.require_file "../../info_sys/test/backends/http_client.exs", __DIR__ 
ExUnit.start 
	
Mix.Task.run "ecto.create", ~w(-r Rumbl.Repo --quiet) 
Mix.Task.run "ecto.migrate", ~w(-r Rumbl.Repo --quiet) 
Ecto.Adapters.SQL.begin_test_transaction(Rumbl.Repo) 
{% endhighlight %}

The generated file is a bit different from what's shown in the book. You just need to add path to `http_client.exs` like below and you'll have no problem.

{% highlight elixir %}
Code.require_file "../../info_sys/test/backends/http_client.exs", __DIR__
ExUnit.start
	
Ecto.Adapters.SQL.Sandbox.mode(Rumbl.Repo, :manual)
{% endhighlight %}
