defmodule Exrachnid.WorkerSupervisor do
  use Supervisor.Behaviour

  #######
  # API #
  #######

  def start_link do
    :supervisor.start_link({:local, __MODULE__}, __MODULE__, [])
  end

  # def start_child(url) do
  #   :supervisor.start_child(__MODULE__, [url])
  # end

  ########################
  # Supervisor Callbacks #
  ########################

  # NOTE: This is the only callback we need to implement for supervisors.
  def init([]) do

    pool_options = [
      name: {:local, :worker_pool},
      worker_module: Exrachnid.Worker,
      size: 100,
      max_overflow: 0
    ]

    children = [
      :poolboy.child_spec(:worker_pool, pool_options, [])
    ] 

    supervise(children, strategy: :one_for_one)

    # children = [
    #   # NOTE: :temporary means that a child process is never restarted. 
    #   #       Might change this to transient. I'm not too sure yet.
    # 
    #   # NOTE: By default, Exrachnid.Worker.start_link would be called. But, 
    #   #       since we are using :simple_one_for_one, no child is started.
    #   worker(Exrachnid.Worker, [], restart: :temporary)
    # ]

    # supervise(children, strategy: :simple_one_for_one)
  end
end
