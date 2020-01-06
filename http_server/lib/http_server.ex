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

  # Server
  # --------

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
    process_request(socket)
    # Recursive call to get the next request.
    listen_connection(listener)
  end

  # Service
  # ---------

  # We process the request, and run the logic in another thread using [spawn].
  # Inside the green thread we do pipeline composition to invoke the rest of functions.
  def process_request(socket) do
    spawn(
      fn ->
        socket
        |> read_request
        |> response_request
        |> close_socket
      end
    )
  end

  def read_request(socket) do
    {:ok, {_server_name, verb, {_, path}, _version}} = :gen_tcp.recv(socket, 0)
    %{socket: socket, verb: verb, path: path}
  end

  # Using [:gen_tcp.send] passing socket and response we can response the request.
  def response_request(%{socket: socket, verb: verb, path: path}) do
    :gen_tcp.send(socket, handle(%{verb: verb, path: path}))
  end

  # Once we response we can close the connection just using [:gen_tcp.close] passing again the socket.
  def close_socket(socket) do
    :gen_tcp.close(socket)
  end

  # Handles
  # --------
  # Pattern matching to handle paths, we create a handle function for each [Method]+[Path] and we implement
  # the business logic for each. It return a String response

  def handle(%{verb: :GET, path: "/login"}) do
    create_response("You can login into the system #{UUID.uuid1()} #{Time.to_string(Time.utc_now())}")
  end

  def handle(%{verb: :GET, path: "/users"}) do
    create_response("We will give you users feature pretty soon")
  end
  # everything else is a 404 response
  def handle(_) do
    create_not_found_response()
  end

  defp create_response(body)  do
    """
    HTTP/1.1 200\r
    Content-Type: text/html\r
    Content-Length: #{byte_size(body)}\r
    \r
    #{body}
    """
  end

  def create_not_found_response()  do
    """
    HTTP/1.1 404\r
    Content-Type: text/html\r
    """
  end

end