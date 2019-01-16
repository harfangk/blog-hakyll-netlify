---
title: How to Create Minimal Elixir Web App With Plug and Cowboy and Deploy to Heroku
---

This post will go through how to create a minimal web application in Elixir just using Cowboy, an HTTP server for Erlang/OTP, and Plug, a composable web middleware for Elixir, and deploy it to Heroku. 

Since you are knowledgeable enough to know about and look for a barebone approach, I assume that you have a basic familiarity with both Elixir and web development.

<!--more-->

I'm using Elixir 1.4.1 at the time of writing. The application directory structure and `mix.exs` file configuration got changed in Elixir 1.4, so I recommend you to get Elixir 1.4 or higher to minimize discrepancy. 

Complete source code of the finished sample application can be found [here](https://github.com/harfangk/timeconverter). Link to the running Heroku app is [here](https://blooming-thicket-28926.herokuapp.com/).

## Create an Elixir Application

In this post, we will create a simple application that converts datetime between iso8601 and unix formats. 

Run `mix new timeconverter --sup` from your shell to create a new Elixir app with built-in supervision tree. 

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

## Add HTTP Server

We will set up Cowboy and Plug. Open `mix.exs` and add dependencies. 

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

Then run `mix deps.get` to fetch necessary dependencies.

After that, we will create `Timeconverter.Router` module that will serve as the HTTP interface of the application. Create `router.ex` in `lib/timeconverter` directory. Copy the following code into the newly created file.

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

`Plug.Router` provides a set of macros to generate routes that respond to HTTP reqeusts. When you use that module, `match` and `dispatch` plugs are required by default.

`get "/"` will respond to GET requests made to the root page. All other requests will be routed to `match _`. 

Check the documentation for `Plug.Router` from [here](https://hexdocs.pm/plug/Plug.Router.html#content). 

`start_link/3` function is a part of `GenServer`, not `Plug.Router`. It defines what `Timeconverter.Router` module will do when it's run under a supervision tree. Here we set it to run `Cowboy` under HTTP.

Next we will include `Timeconverter.Router` under the application's supervision tree so that it will be run when the application runs. Open `lib/timeconverter/application.ex` and add `Timeconverter.Router` as a worker.

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

Now `Cowboy` HTTP server will also run when our application runs.

Run `mix run --no-halt` from the shell. If you are prompted to install `rebar` dependency, do so. When the application runs, open up `localhost:4000`. You will see the application running, greeting you with a simple text "wow!".

## Add Domain Logic for Converting Time Format

Since this post is about building a minimal web app and deploying it, we will not go into the domain logic itself. It's a pretty boring example, anyway.

Replace the contents of `lib/timeconverter.ex` with the following code. Remember that this sample code is also provided on github. 

{% highlight elixir %}
defmodule Timeconverter do
  @moduledoc """
  Documentation for Timeconverter.
  """
  
  def convert_datetime(%{"format" => format, "time" => time}) do
    case format do
      "to_unix" -> iso8601_to_unix(time)
      "to_iso8601" -> unix_to_iso8601(time)
      _ -> {:error, ~s{Format should be either "to_unix" or "to_iso8601"}}
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

Nothing interesting here - just some Regex and DateTime functions. Let's look at how we connect the HTTP routes with domain logic. Open `lib/timeconverter/router.ex` and change the route functions.

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

`Plug.Conn` could be understood as the representation of HTTP requests and responses. All useful information about a single HTTP connection is stored in one `Plug.Conn` struct and can be accessed from there. 

Query parameters can be accessed from `conn.params` only after running `Plug.Conn.fetch_query_params/1`. After getting the parameters, we pass `conn.params` to our domain logic function `Timeconverter.convert_datetime/1`. As you can see, connecting HTTP endpoint layer and domain logic is simple and straightforward.

Quit the application and run it again. Now the app will respond to different query parameters.

## Deploying to Heroku

You need Heroku account and Heroku Command Line Interface for this part. If you haven't set it up already, visit [Heroku dev center](https://devcenter.heroku.com/articles/heroku-cli) and follow the instructions.

We need buildpacks to deploy to Heroku. Although Elixir is not officially supported, there's an open source buildpack for Elixir created by HashNuke. We will use that buildpack to deploy to Heroku. Check the documentation for HashNuke's buildpack from [here](https://github.com/HashNuke/heroku-buildpack-elixir). 

But we need to make a few preparations before deploying. First create `elixir_buildpack.config` file in the application's root directory and type the following configurations. 

{% highlight bash %}
# Erlang version
erlang_version=19.2

# Elixir version
elixir_version=1.4.1
{% endhighlight %}

As of February 2017 the buildpack uses Elixir 1.3 when no Elixir version is specified. Since we're using Elixir 1.4, the build causes error if we don't specify the Elixir version we'd like to use.

Now open `lib/timeconverter/router.ex` file to set up port configuration for Heroku. 

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

Heroku assigns a port through environment variable PORT. We need to get that port number through `System.get_env("PORT")` so that our application can run on Heroku. `if` clause in `get_port/0` is there to provide port number when we would like to run our application locally.

Go to the root directory of our application. Then create a Heroku app with the buildpack by running `heroku create --buildpack "https://github.com/HashNuke/heroku-buildpack-elixir.git"`.

The script will build the Heroku application and also set a git remote repository called heroku. Run `git push heroku master` to deploy the application to Heroku. Now your application is all set and running. Run `heroku open` to open the application in your browser and check if it's running correctly.
