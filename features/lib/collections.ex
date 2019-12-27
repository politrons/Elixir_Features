defmodule Collections do
  @moduledoc false

  # List
  # ----

  #  In Elixir just like in other dynamic language allow you to add different types in one collection.
  my_collection = ["un", 2, "tres", 4.0, :cinco]

  #  In Elixir merge two collections is so simple like use ++ of both
  concat_list = [1, 2, 3, 4] ++ [5, 6, 7, 8, 9, 10]
  IO.inspect concat_list

  # Recursion:
  # Just like in Haskell the most common way to iterate an array is using recursion, here we define the function
  # where we get every element of the array defining the element and the rest of the collection, without that element extracted,
  # and then we need to also define another function when the array is empty and then we wont nothing.
  def for_each([element | elements]) do
    IO.inspect element
    for_each(elements)
  end

  def for_each([]), do: nil

  # Sugar iteration:
  # Second option is less verbose and more sugar using [Enum.each] function that allow pass an array
  # and he iterate a consumer function.

  Enum.each my_collection, fn element ->
    IO.inspect element
  end

  # Map
  # ----

  # Maps in Elixir are created using %{key => value} and allow N entry arguments or 0 %{}
  my_map = %{
    "key" => "value",
    1 => "DOS",
    2.0 => 5.0,
    :my_key => "works"
  }

  # You can get elements from map just using [Map.fetch] passing the map and key. Quite similar as Golang
  IO.inspect Map.fetch(my_map, "key")
  IO.inspect Map.fetch(my_map, "bla")

  # Fetch return an :error in case there's no key in the map
  value = Map.fetch(my_map, "bla")
  IO.inspect "The key in the map return error?:" <> to_string(value == :error)

  #  Get Sugar
  IO.inspect my_map
  IO.inspect my_map["key"]
  IO.inspect my_map.my_key # In case of simbol type we can use it directly on the map

  IO.inspect my_map[1]
  IO.inspect my_map[2.0]
  IO.inspect my_map["bla"]

  # Update value
  updated_map = Map.get_and_update(my_map, "key", fn value -> {"key", String.upcase(value)}  end)
  IO.inspect updated_map

  # To append elements and creating new maps we use Map.put
  new_map = Map.put(my_map, "new_key", "new_value")
  IO.inspect new_map

  # To drop elements and creating new maps we use Map.delete
  new_map = Map.delete(my_map, 1)
  IO.inspect new_map

end

#It's seems like [def] functions only can be executed from outside of the module'
defmodule CollectionsMain do
  @moduledoc false
  Collections.for_each([1, "hello", 2.0, :pablo])

end
