defmodule PatternMatching do
  @moduledoc false

  # Pattern matching checking the value in any case
  int_val = 10
  string_val = "hello world"
  x = "hello world"

  output = case x do
    ^int_val -> "It's an integer value:" <> x
    ^string_val -> "It's a string value:" <> x
    _ -> "I dont know what this value is"
  end
  IO.inspect output

  # Pattern matching condition using [when] boolean condition to pick up one case or other.
  # This is really handy to have Strong type and avoid class cast exceptions.
  output_type = case x do
    x when is_integer(x) -> "It's an integer type:" <> x
    x when is_binary(x) -> "It's a string type:" <> x
    _ -> "I dont know what this type is"
  end
  IO.inspect output_type


  #Functions
  #----------
  #This is really awesome, how we can use pattern matching with functions to do several things depending of the types of arguments.

  function_pattern_matching = fn
    x when is_integer(x) -> x * 1000
    x when is_binary(x) -> String.upcase(x)
    x when is_function(x) -> x.("Even works with embedded functions?!, wow")
    :patata -> "Patatas with one beer please"
    _ -> "This function is not well defined"
  end

  IO.inspect function_pattern_matching.(1)
  IO.inspect function_pattern_matching.("hello functional pattern matching")
  IO.inspect function_pattern_matching.(fn x -> String.upcase(x) end)
  IO.inspect function_pattern_matching.(:patata)
  IO.inspect function_pattern_matching.(:eihn?)

end
