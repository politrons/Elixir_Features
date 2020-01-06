defmodule HttpServer do

  @moduledoc false

  use Application

  # Function to be invoked by mix.exs to run the application.
  # Here we define the execution of the server as children of a supervisor.
  # To get more info about supervisor visit [my_gen_server]
  def start(_type, _args) do
    IO.inspect "Starting application"
    children = [
      %{
        id: HttpServer,
        start: {HttpServer, :start_server, [1981]}
      }
    ]

    supervisor_options = [strategy: :one_for_one, name: Supervisor]
    # Run the supervisor passing the array of children and the supervisor configuration,
    # and then this one run the children process.
    Supervisor.start_link(children, supervisor_options)
  end

  # Function to start the http server.
  # We use [:gen_tcp] API to use all features to build the server and service layer.
  def start_server(port) do
    IO.inspect "Starting http server on port #{port}....."

    http_opts = [active: false, packet: :http_bin, reuseaddr: true]
    # Using [:gen_tcp.listen] passing the port we receive a tuple of state of action(:ok,:error) and the socket or reason of error
    # We do a pattern matching to control side-effect of creation of server.
    case :gen_tcp.listen(port, http_opts) do
      {:ok, listener} ->
        # Spawn_link run a function Module in another thread, and wait until the function return.
        # We specify the module, function name and argument to the function.
        # Run in another HttpServer the function that accept connections.
        {:ok, spawn_link(HttpServer, :listen_connection, [listener])}
      {:error, reason} ->
        IO.inspect "Error running the server caused by: #{inspect reason}"
    end
  end

  # [:gen_tcp.accept] passing a socket block the execution until we receive a request, and :ok or :error
  # to check side-effect.
  def listen_connection(listener) do
    {:ok, socket} = :gen_tcp.accept(listener)
    process_request(socket, "Hello Http Server #{UUID.uuid1()} #{Time.to_string(Time.utc_now())}")
    # Recursive call to get the next request.
    listen_connection(listener)
  end

  # We process the request, and run the logic in another thread using [spawn].
  # Using [:gen_tcp.send] passing socket and response we can response the request.
  # Once we response we can close the connection just using [:gen_tcp.close] passing again the socket.
  def process_request(socket, body) do
    spawn(
      fn ->
        :gen_tcp.send(socket, create_response(body))
        :gen_tcp.close(socket)
      end
    )
  end

  def create_response(body)  do
    """
    HTTP/1.1 200\r
    Content-Type: text/html\r
    Content-Length: #{byte_size(body)}\r
    \r
    #{body}
    """
  end

end