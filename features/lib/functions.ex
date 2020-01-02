defmodule Functions do
  @moduledoc false

  #  Anonymous functions
  #  --------------------
  #  In Elixir we can create simple functions just using [fn] before the definition of arguments
  #  Functions in Elixir is not different to another functional language as Haskell or Scala, where we apply
  #  referential transparency, expect a number of arguments as input, including void, and return always same value.
  #  and replace the function by the end value it wont alter your program
  supplier_func = fn () -> "Hello supplier function" end
  sum_func = fn a, b -> a + b end
  concat_func = fn a, b -> a <> b end
  upper_case_func = fn a -> String.upcase(a) end

  IO.puts supplier_func.()
  IO.puts "My first function Wi!!! " <> to_string(sum_func.(5, 5))
  IO.puts "Concat function " <> concat_func.("Hello", "world")
  IO.puts "Upper case function " <> upper_case_func.("hello functional world")

  #  We can check if a variable is a function using [is_function] operator
  IO.puts "This is a function?:" <> to_string(is_function(sum_func))

  #  Named functions
  #   ---------------
  #  You can create named functions just like in Scala you have def or val(anonymous) functions.
  #  As a limitation that I found, named functions cannot being invoked within the same module, not idea why!
  def my_named_function(a, b) do
    "after sum " <> to_string(a + b) <> " number"
  end

  #  Higher order functions
  #  -----------------------
  #  All functions in elixir are first class citizen so it can receive and return new functions
  higher_order_func0 = fn value ->
    value <> " Functional"
  end

  higher_order_func1 = fn value ->
    value <> " world!"
  end

  higher_order_func2 = fn value ->
    String.upcase(value)
  end

  IO.inspect higher_order_func2.(higher_order_func1.(higher_order_func0.("hello")))

  function_as_argument = fn (value, upper_function) ->
    upper_function.(value) <> "!!!!!"
  end

  IO.inspect function_as_argument.("hello elixir functional world", fn value -> String.upcase(value) end)

  #  Pipeline
  # -------------
  #  We can make this functions composition more sugar using [|>] pipeline operator, that allow
  #  compose one function with the next. Here the order of functions are more natural, and easy to reason.
  response = "hello"
             |> higher_order_func0.()
             |> higher_order_func1.()
             |> higher_order_func2.()
  IO.inspect response

  #  Currying functions
  #  -----------------------
  #  Currying function like in Haskell or Scala allow you to resolve a function partially, and return the rest of the function
  #  to be evaluated once we pass the rest of the arguments.
  #  Here we pass a function to sum two numbers, and internally we create a currying function that receive a number, and return
  #  a function that expect to receive a string to concat with the number, once we transform into string

  function_receive_and_return_function = fn sum_function ->
    to_string_function = fn sum_value -> fn concat_value -> to_string(sum_value) <> concat_value end end
    to_string_function.(sum_function.(10, 5))
  end

  currying_function = function_receive_and_return_function.(fn (a, b) -> a + b end)

  IO.inspect currying_function
  IO.inspect currying_function.(" Numbers")

  # Here we have a function that return a function which return another function.
  currying_function = fn a -> fn b -> fn () -> String.upcase(a <> " currying function " <> b) end end end
  IO.inspect currying_function.("hello").("world").()

end

defmodule FunctionsRunner do

  IO.inspect Functions.my_named_function(5, 5)

end