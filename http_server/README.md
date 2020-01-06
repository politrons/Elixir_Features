# HttpServer # ![My image](../img/server-icon.png)

You can find the code of the server **[here](lib/http_server.ex)**

## Installation

In order to create a mix project just use

```
mix new http_server
```

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `http_server` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:http_server, "~> 0.1.0"},
    { :uuid, "~> 1.1" }
  ]
end
```
And then run mix command to download dependencies.

```
mix deps.get
```

Also add the module where your server will run 

```elixir
def application do
    [mod: {HttpServer, []}]
end
```

## Run

To run the server just need to execute the next command in the same folder where you have your mi.exs

```
mix run --no-halt
```

