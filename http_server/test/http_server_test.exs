defmodule HttpServerTest do
  use ExUnit.Case
  doctest HttpServer

  test "greets the world" do
    assert HttpServer.start("type","arg") == :world
  end
end
