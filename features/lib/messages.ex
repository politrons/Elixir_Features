defmodule Messages do
  @moduledoc false

  #  Function to run async using spawn
  def simple_process(a, b, c) do
    String.upcase(a <> " " <> b <> " " <> c)
  end

end

#  Spawn
# --------
# In order to run async process in another modules we can also use [spawn] specifying Module, name_of_function,
# and array of the arguments we want to pass.
# as a result it will return a process_id of the thread execution.
defmodule MessagesRunner do

  pid = spawn(Async, :simple_process, ["Hello", "async", "world"])

  IO.inspect pid

end