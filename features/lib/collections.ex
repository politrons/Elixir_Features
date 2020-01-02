defmodule Collections do
  @moduledoc false

  #----------#
  #   List   #
  #----------#

  #  In Elixir just like in other dynamic language allow you to add different types in one collection.
  my_collection = ["un", 2, "tres", 4.0, :cinco]

  #  In Elixir merge two collections is so simple like use ++ of both
  concat_list = [1, 2, 3, 4] ++ [5, 6, 7, 8, 9, 10]
  IO.inspect concat_list

  # Recursion iteration
  # -------------------
  # Just like in Haskell the most common way to iterate an array is using recursion, here we define the function
  # where we get every element of the array defining the element and the rest of the collection, without that element extracted,
  # and then we need to also define another function when the array is empty and then we wont nothing.
  def for_each([element | elements]) do
    IO.inspect element
    for_each(elements)
  end

  def for_each([]), do: nil

  # Sugar iteration
  # ----------------
  # Second option is less verbose and more sugar using [Enum.each] function that allow pass an array
  # and he iterate a consumer function.

  Enum.each my_collection, fn element ->
    IO.inspect element
  end

  #  Filter list
  # -------------
  # for iterator allow pass a function as second argument, to filter the list before apply the transform function,
  # in the third argument it will return a new list.
  my_array = ["hello", "Elixir", 5, "functional", :another, "world"]
  is_alphanumeric? = fn (n) -> is_binary(n) end
  filter_list = for n <- my_array, is_alphanumeric?.(n), do: String.upcase(n)
  IO.inspect filter_list

  # Using [List.last] operator passing an array we extract the last element of the array, also works with [first]
  get_last_element = List.last(my_array)
  IO.inspect get_last_element

  get_first_element = List.first(my_array)
  IO.inspect get_first_element

  #  Fold list
  # -----------
  #Just like in Haskell or Scala foldLeft or foldRight allow iterate from left to right, into a type specify as second
  # argument, and as third, we pass function where every iteration, is the previous element added into the type.
  new_concat_string = List.foldr(
    ["fold", "always", "is", "cool"],
    "",
    fn old, new -> String.upcase(old <> " " <> new) end
  )
  IO.inspect new_concat_string

  # Here we iterate the list and we pass every element in upper case in the new type array.
  new_upper_list = List.foldl(["fold", "always", "is", "cool"], [], fn new, old -> old ++ [String.upcase(new)] end)
  IO.inspect new_upper_list

  #----------#
  #    Map   #
  #----------#

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

  #------------#
  #   Range    #
  #------------#
  range = 1..100

  # Get first and last element
  first..last = range
  IO.inspect first
  IO.inspect last

  # Integration with Enum module

  # Count number of elements
  IO.inspect Enum.count(range)

  #------------#
  #   Stream   #
  #------------#

  #Stream monad is not different compare with Java or Scala streams, same operators and same concept to filter,
  # transform and compose elements.
  range = 1..10

  # With [map] operator we can transform elements of the stream
  double_stream = Stream.map(range, fn element -> element * 2 end)
  IO.inspect Enum.to_list(double_stream)

  # With [flat_map] operator we can compose elements of the stream into new element type
  flat_map_stream = Stream.flat_map(range, fn element -> [element * 2] end)
  IO.inspect Enum.to_list(flat_map_stream)

  # With filter operator we can filter the elements of the stream
  filter_stream = Stream.filter(range, fn number -> number > 5 end)
  IO.inspect Enum.to_list(filter_stream)

end

#It's seems like [def] functions only can be executed from outside of the module'
defmodule CollectionsMain do
  @moduledoc false
  Collections.for_each([1, "hello", 2.0, :pablo])
end
