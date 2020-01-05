defmodule HttpServer do

  @moduledoc false

  use Application

  def start(_type, _args) do
    IO.inspect "starting application"
    children = [
      %{
        id: HttpServer,
        start: {HttpServer, :start_server, []}
      }
    ]

    opts = [strategy: :one_for_one, name: Supervisor]
    Supervisor.start_link(children, opts)
  end


  require Logger

  def start_server do
    IO.inspect "starting socket connection"

    {:ok, socket} = :gen_tcp.listen(1981, active: false, packet: :http_bin, reuseaddr: true)

    # Spawn_link run a function Module in another thread, and wait until the function return.
    # We specify the module, function name and argument to the function.
    # Run in another HttpServer the function that accept connections.
    {:ok, spawn_link(HttpServer, :accept_connection, [socket])}
  end

  # [:gen_tcp.accept] passing a socket block the execution until we receive a request, and :ok or :error
  # to check side-effect.
  def accept_connection(socket) do
    {:ok, request} = :gen_tcp.accept(socket)

    # We process the request, and the logic in another thread.
    spawn(fn ->
      body = "Hello world! The time is #{Time.to_string(Time.utc_now())}"

      response = """
      HTTP/1.1 200\r
      Content-Type: text/html\r
      Content-Length: #{byte_size(body)}\r
      \r
      #{body}
      """
      send_response(request, response)
    end)

    # Recursive call to get the next request.
    accept_connection(socket)
  end

  # Using [:gen_tcp.send] passing socket and response we can response the request.
  # Once we response we can close the connection just using [:gen_tcp.close] passing again the socket.
  def send_response(socket, response) do
    :gen_tcp.send(socket, response)
    :gen_tcp.close(socket)
  end

  # Child function to be used for a Supervisor where we specify how we start the server.
  # In case the server fails for whatever reason it will be by default restart.
  def child_spec(opts) do
    %{id: HttpServer, start: {HttpServer, :start_server, [[opts]]}}
  end



end