defmodule Exrachnid.Worker do
  use GenServer.Behaviour

  alias HTTPotion.Response
  @user_agent  [ "User-agent": "Elixir benjamintanweihao@gmail.com"]
  
  ####### 
  # API #
  ####### 

  def start_link(url) do
    # NOTE: We do not need a singleton server. Hence, we leave out the
    #       {:local, name} bit.
    :gen_server.start_link(__MODULE__, [url], [{:trace}])
  end

  def crawl(url) do
    # NOTE: This is slightly round-about. Here is what is happening:
    # 
    #       1. We call Exrachnid.Worker.crawl(url).
    #       2. Next, Exrachnid.Supervisor.start_child(url) is called.
    #       3. Then, Exrachnid.Worker.start_link(url) is called.
    #       4. The process is then attached to the supervision tree.
    { :ok, pid } = Exrachnid.WorkerSupervisor.start_child(url)

    :gen_server.cast(pid, { :crawl, url })
  end

  #######################
  # GenServer callbacks #
  #######################

  def init(_url) do
    { :ok, [] }
  end

  def handle_cast({ :crawl, url }, _state) do
    case HTTPotion.get(url, @user_agent, []) do
      Response[body: body, status_code: status, headers: _headers] when status in 200..299 ->
        
        Exrachnid.add_fetched_url(url)

        host = URI.parse(url).host
        
        # Add extracted links
        body          
          |> extract_links(host)
          |> Exrachnid.add_new_urls

      _ -> 
        # TODO: Do nothing yet.
    end
    { :stop, :normal, [] }
  end

  def handle_info(_msg, _state) do
  end

  def code_change(_old_vsn, _state, _extra) do
    :ok
  end

  def terminate(_reason, _state) do
    :ok
  end

  #####################
  # Private functions #
  #####################

  def extract_links(page, host) do
    result = %r/<a[^>]* href="([^"]*)"/ |> Regex.scan(page) 
    links = case is_list(result) do
      true  -> result |> Enum.map(fn [_,x] -> x end)
      false -> []
    end
    
    links |> Enum.map(fn(url) -> normalize_link(host, url) end)
  end

  # If URL is fully qualified, the ignore. Else attach host
  def normalize_link(host, url) do
    uri_info = URI.parse(url)
    result = case uri_info.host do
      nil -> 
        "#{host}#{uri_info.path}" 
      _ -> 
        url
    end
    result
  end
end
