defmodule MyGenServer do
  @moduledoc false

  #In order to create your own GenServer, you need to use GenServer in your own implementation
  use GenServer

  def start_child_server(state) do
    GenServer.start_link(__MODULE__, state, name: __MODULE__)
  end

  #Function to be invoked once the GenServer is initialized
  @impl true
  def init(basket) do
    IO.inspect "Init Basket"
    {:ok, basket}
  end

  # All [handle_call] functions are Synchronous from the client side.
  # [handle_call] implementation, specifying the message that we match for this function, once we receive it.
  # Synchronous call to get an element from this process. It will block the client until the message is received
  @impl true
  def handle_call(:basket, _from, basket) do
    {:reply, basket, basket}
  end

  @impl true
  def handle_call(:total_price, _from, basket) do
    total_price = List.foldr(basket, 0, fn (product, total_price) -> total_price + product.price end)
    {:reply, total_price, basket}
  end

  # [handle_cast] implementation. Asynchronous fire and forget call, the client send the message to the process, and don't wait for any response.
  @impl true
  def handle_cast({:add_product, product}, basket) do
    {:noreply, [product | basket]}
  end

  # Another [cast] function, this one to delete products from the basket
  @impl true
  def handle_cast({:delete_product, product_id}, basket) do
    filter_products = Enum.filter(basket, fn product -> product.id != product_id end)
    {:noreply, filter_products}
  end

end

defmodule Product do
  defstruct id: "id", description: "", price: 0
end

defmodule MyGenServerRunner do

  # We create a GenServer process using [start_link] passing the Module name, the init message, and also optionally
  # the name you want to use to invoke the GenServer once is created using call or cast
  {:ok, _} = GenServer.start_link(MyGenServer, [], name: Basket)
  # {:ok, pid} = GenServer.start_link(MyGenServer, ["hello_gen_server"])

  IO.inspect GenServer.call(Basket, :basket)
  GenServer.cast(Basket, {:add_product, %Product{id: "1", description: "coca-cola", price: 2.0}})
  GenServer.cast(Basket, {:add_product, %Product{id: "2", description: "pepsi", price: 2.0}})
  IO.inspect GenServer.call(Basket, :basket), label: "Basket"
  IO.inspect "Total price: #{GenServer.call(Basket, :total_price)}"
  GenServer.cast(Basket, {:delete_product, "2"})
  IO.inspect GenServer.call(Basket, :basket), label: "Basket"
  IO.inspect "Total price: #{GenServer.call(Basket, :total_price)}"

  # Supervisor
  # --------------
  # Create GenServer using Supervisor like Akka Guard/Supervisor, exactly same logic behind this error Handler implementation.
  IO.inspect "------------------------------------------------------------------------"
  IO.inspect "Supervisor:"

  # We define our GenServer as a child configuring the function to start up the server.
  children = [
    %{
      id: MyGenServer,
      start: {MyGenServer, :start_child_server, [[]]}
    }
  ]

  # We start a supervisor using [start_link] function, passing the array of children to supervise.
  # We also pick up which strategy apply on them. in this case [one_for_one] if a child process terminates, only that process is restarted
  {:ok,pid} = Supervisor.start_link(children,  strategy: :one_for_one)
  # Using [count_children] function passing the Supervisor processId we get info about all children of this supervisor
  IO.inspect Supervisor.count_children(pid), label: "Supervisor children information"

  GenServer.cast(MyGenServer, {:add_product, %Product{id: "1", description: "nachos", price: 3.0}})
  IO.inspect GenServer.call(MyGenServer, :basket), label: "Basket"
  IO.inspect "Total price: #{GenServer.call(MyGenServer, :total_price)}"
end