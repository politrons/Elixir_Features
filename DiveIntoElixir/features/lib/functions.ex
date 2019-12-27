defmodule Functions do
  @moduledoc false
  


  use Application

  def start(_type, _args) do
    Functions.Supervisor.start_link()
  end
end