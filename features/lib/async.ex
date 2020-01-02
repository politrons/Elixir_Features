defmodule Async do
  @moduledoc false

  # ------ #
  #  Task  #
  # ------ #
  # Task are, like in any other language, computation in another lightweight thread.

  #  Async
  # -------
  # Basic async  Task contains a function with no arguments.
  task = Task.async(
    fn ->
      :timer.sleep(1000)
      "Hello async world"
    end
  )

  sync_response = fn input -> String.upcase(input) <> "!!!" end.("Hello Sync world")
  # To move the thread local result into the main one, we use [Task.await]. The await it has a default timeout of 5 seconds.
  async_response = Task.await(task)

  IO.inspect sync_response
  IO.inspect async_response

  #Thanks to [map] and [flat_map] of Enum module we can create several array entry in a Task, run in parallel,
  # and thanks to flat_map compose results doing [Task.await]
  result = [1, 2, 3, 4, 5]
           |> Enum.map(
                fn number -> Task.async(
                               fn -> :timer.sleep(1000)
                                     IO.inspect "Running in parallel"
                                     [number * 10]
                               end
                             )
                end
              )
           |> Enum.flat_map(fn task -> Task.await(task) end)

  IO.inspect result

  # Using zip operator we can zip several async operations in one unique result.
  zip_result = Enum.zip(
    [
      Task.await(Task.async(fn -> :timer.sleep(1000);[" hello "] end)),
      Task.await(Task.async(fn -> :timer.sleep(1000);[" async "] end)),
      Task.await(Task.async(fn -> :timer.sleep(1000);[" world "] end))
    ]
  )

  IO.inspect zip_result

  #  Yield
  # --------
  #  [Task.yield] function is actually how we should always wait for the result running in another thread.
  #  Here we can match the result since it return a tuple of {:ok, result}, {:exit, :noproc} in case the task
  #  end already, or nil in case the task dont end before the timeout.
  timeout_task = Task.async(
    fn ->
      :timer.sleep(1000)
      "This result it might never being on time"
    end
  )

  timeout_result = case Task.yield(timeout_task)  do
    {:ok, result} -> result
    {:exit, :noproc} -> "This task end already"
    nil -> "Timeout error"
  end
  IO.inspect timeout_result

  # In case we plan to end the task after some timeout, we need also to control the shutdown of the task
  timeout_result = case Task.yield(timeout_task, 500) || Task.shutdown(task) do
    {:ok, result} -> result
    {:exit, :noproc} -> "This task end already"
    nil -> "Timeout error"
  end
  IO.inspect timeout_result

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
# if you just decide to ignore this Pid for further communication, it will be consider a fire & forget communication.
defmodule AsyncRunner do

  pid = spawn(Async, :simple_process, ["Hello", "async", "world"])
  IO.inspect pid

end