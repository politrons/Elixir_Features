# Modules are the closest thing we have in Functional language as Elixir or Haskell to classes.
# Here we define a data type structure with some default values, and now is possible to access from everywhere
defmodule User do
  defstruct name: "", age: 0
end

#  In Elixir as a functional language it does not contain any class/instance, everything is stateless and static.
#  You can consider the closest thing as a class a module as we have here, which is basically a collection of functions.
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

  # We can also use #{} to use interpolations like in Scala, and add some functions to be used inside string
  IO.puts "How many characters #{String.length("How many characters I have?")}"

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
  :applee # Atoms are constant types whose name is also his value. Start with [:] and the value is what follow the :
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

  #  Structs types
  # ---------------
  # Structs type use the module data type created previously in the top of this module.

  user = %User{name: "politrons", age: 38}

  IO.inspect "Name: #{user.name}"
  IO.inspect "Age: #{user.age}"

  # Agents
  # ------------
  #Agents are a simple abstraction around state.
  #Often in Elixir there is a need to share or store state

  # For this example and in order to reduce the agent value in a loop, we use recursion functions
  def reduce_and_print(x, pid) when x > 0 do
    # Then we can use functions like [get_and_update] passing the processId to update the state of the agent,
    # passing a new function where the function argument is the current state of the [Agent]
    fun = fn -> Agent.get_and_update(pid, fn current_state -> {current_state, current_state - 1} end) end
    value = fun.()
    IO.inspect value
    reduce_and_print(value, pid)
  end

  # Like in Haskell is very common create the output function to be matched by the invocation, once the value is 0
  def reduce_and_print(0, _) do
    nil end

end

defmodule AgentRunner do
  # Here we create the agent with a initial function that set the state.
  {:ok, pid} = Agent.start_link(fn -> 5  end)
  Basics.reduce_and_print(1000, pid)
end
