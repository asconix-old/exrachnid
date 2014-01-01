defmodule Exrachnid.Supervisor do
  use Supervisor.Behaviour

  #######
  # API #
  #######

  def start_link do
    :supervisor.start_link({:local, __MODULE__}, __MODULE__, [])
  end

  ########################
  # Supervisor Callbacks #
  ########################

  def init([]) do
    # NOTE: We can see a nice visualization by :appmon.start
    children = [
      # NOTE: If we leave out the restart strategy, it defaults to 
      #       :permanent.
      worker(Exrachnid.DbServer, []),
      worker(Exrachnid.WorkerSupervisor, [])
    ]

    # NOTE: Previously made a stupid mistake. If we changed this to
    #       strategy: :simple_one_for_one, this would crash with a
    #       bad_start_spec error. Why? Because :simple_one_for_one
    #       means we cannot have the restart strategy to be permanent.
    supervise(children, strategy: :one_for_one, max_restarts: 1000000, max_seconds: 1)
  end

end
