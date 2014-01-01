defmodule Exrachnid.WorkerSupervisor do
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

    pool_options = [
      name: {:local, :worker_pool},
      worker_module: Exrachnid.Worker,
      size: 5,
      max_overflow: 0
    ]

    children = [
      :poolboy.child_spec(:worker_pool, pool_options, [])
    ] 

    supervise(children, strategy: :one_for_one)

  end
end
