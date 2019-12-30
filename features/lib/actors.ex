# In Erlang/Elixir the Akka actor, is known as process, and works pretty much the same.
# Stateless entity that not share state and communicate between each other by message
defmodule Actors do
  @moduledoc false

  #Just like in Akka we use some message to communicate between process
  :init
  :marco
  :polo

  # An stateless computation, the communication with him is asynchronously, and by message.
  # The Process just like Akka must implement receive function, where using pattern matching we
  #  check if the message that we receive match with any that we have, otherwise make a recursive call
  #  and wait for the next message.
  defmodule Process1 do

    def listener() do
      #Pattern matching function waiting for the message.
      receive do
        {:marco, number_of_interactions, pid_from} ->
          IO.puts "Marco"
          # Using send follow by the processId reference and the message we communicate between process.
          # Just like in Akka actor, we can refer ro self() as the processId address of our process.
          send pid_from, {:polo, number_of_interactions, self()}
        _ -> IO.puts "This message was not for me"
      end
      listener()
    end
  end

  # Second process that it will use to communicate by message to Process1
  defmodule Process2 do
    def listener do
      receive do
        {:init, number_of_interactions} ->
          pid_to = spawn(Actors.Process1, :listener, [])
          send pid_to, {:marco, number_of_interactions, self()}
        {:polo, number_of_interactions, pid_to} ->
          IO.puts "Polo"
          send pid_to, {:marco, number_of_interactions, self()}
        _ -> IO.puts "This message was not for me"
      end
      listener()
    end
  end

  pid_process1 = spawn(Actors.Process2, :listener, [])
  send pid_process1, {:init, 1000}

end
