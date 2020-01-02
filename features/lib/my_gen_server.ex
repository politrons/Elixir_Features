defmodule MyGenServer do
  @moduledoc false

  use GenServer

  #Function to be invoked once the GenServer is initialized
  @impl true
  def init(basket) do
    IO.inspect "Init Basket"
    {:ok, basket}
  end


  # [handle_call] implementation, specifying the message that we match for this function, once we receive it.
  # Synchronous call to get an element from this process. It will block the client until the message is received
  @impl true
  def handle_call(:basket, _from, basket) do
    {:reply, basket, basket}
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
  IO.inspect GenServer.call(Basket, :basket)
  GenServer.cast(Basket, {:delete_product, "2"})
  IO.inspect GenServer.call(Basket, :basket)


end