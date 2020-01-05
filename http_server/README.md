# HttpServer


In order to create a mix project just use

```
mix new http_server
```

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `http_server` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:http_server, "~> 0.1.0"}
  ]
end
```

Also add the module where your server will run 

```elixir
def application do
    [mod: {HttpServer, []}]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/http_server](https://hexdocs.pm/http_server).

