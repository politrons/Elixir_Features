defmodule Basics do
  @moduledoc false

  #  String features
  # ------------------

  #  You can define symbols just using : operator, concat with <> and move to String using [to_string] function
  IO.puts "Those names are alike? " <> to_string(:paul == :paul)

  # In order to just print we use [IO] with operator [puts]
  IO.puts "Hello functional Elixir world!!!"

  #  Factory class [String] provide the typical operators to work with Strings
  IO.puts String.upcase("hello string in upper case")

  IO.puts "How many characters " <> to_string(String.length("How many characters I have?"))

  # Primitive Types
  # -----------------
  # Variable assigned is just like in any other dynamic type language, you can asign whatever value, and
  # then automatically the variable is created for that type.
  # Since Elixir is not String type, compiler cannot help us here, and combine different types is possible, bringing
  # the nasty classCast exception. That's why is  good practice to use function [is_*] to know the type of a variable

  integer = 1
  float = 1.0
  string = "hello variable"
  array = ["hello", 1, "world", 5.0]
  :applee
  :bannana

  IO.puts to_string(integer) <> " is integer:" <> to_string(is_integer(integer))
  IO.puts to_string(float) <> " is float:" <> to_string(is_float(float))
  IO.puts string <> " is string:" <> to_string(is_binary(string))
  IO.inspect array
  IO.inspect :apple
  IO.inspect :bannana

  #   Tuples
  # ---------
  tuples = {"hello", :tuple, "world"}

  IO.inspect tuples
  IO.inspect tuples
             |> elem(2) # Using [elem(position)] we can extract elements from a tuple

  {first, second, third} = tuples
  IO.inspect first
  IO.inspect second
  IO.inspect third

  # Update element in tuple
  IO.inspect put_elem(tuples, 2, "tuple")
end
