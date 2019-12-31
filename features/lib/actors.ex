# In Erlang/Elixir the Akka actor, is known as processes, and works pretty much the same.
# Stateless entity that not share state and communicate between each other by message
defmodule Actors do
  @moduledoc false

  #Just like in Akka we use some message to communicate between process
  :init
  :marco
  :polo

  defmodule UtilsFunctions do

    def update_state(agent_pid) do
      Agent.get_and_update(agent_pid, fn current_state -> {current_state, current_state - 1} end)
    end

  end

  # An stateless computation, the communication with him is asynchronously, and by message.
  # The Process just like Akka must implement receive function, where using pattern matching we
  #  check if the message that we receive match with any that we have, otherwise make a recursive call
  #  and wait for the next message.
  defmodule Process1 do

    def listener() do
      #Pattern matching function waiting for the message.
      receive do
        {:marco, agent_pid, pid_from} ->
          IO.puts "Marco"
          cond do
            # In this condition we update the counter of the agent and we check if we reach 0, otherwise we communicate
            UtilsFunctions.update_state(agent_pid) > 0 ->
              # Using send follow by the processId reference and the message we communicate between process.
              # Just like in Akka actor, we can refer ro self() as the processId address of our process.
              send pid_from, {:polo, agent_pid, self()}
          end
        _ -> IO.puts "This message was not for me"
      end
      listener()
    end
  end

  # Second process that it will use to communicate by message to Process1
  defmodule Process2 do
    def listener do
      receive do
        {:init, agent_pid} ->
          IO.puts "Init"
          pid_to = spawn(Actors.Process1, :listener, [])
          send pid_to, {:marco, agent_pid, self()}
        {:polo, agent_pid, pid_to} ->
          IO.puts "Polo"
          cond do
            UtilsFunctions.update_state(agent_pid) > 0 -> send pid_to, {:marco, agent_pid, self()}
          end
        _ -> IO.puts "This message was not for me"
      end
      listener()
    end
  end

  # In order to have maximum number of interactions between the two process we create an Agent which contains the
  # counter state that it will be share between all process.
  # Once the counter reach 0 we stop communications.
  # If you want to see more information about [Agent] go to basics.ex file to see some examples.
  {:ok, agent_pid} = Agent.start_link(fn -> 10  end)
  # In order to create a reference of the process or [pid] we use [spawn] function passing the module, function and
  # number of arguments of that process.
  pid_process1 = spawn(Actors.Process2, :listener, [])
  send pid_process1, {:init, agent_pid}

end
