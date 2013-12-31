defmodule Exrachnid.DbServer do
  require Lager
  use GenServer.Behaviour

  defrecord State, new_urls: HashSet.new, fetched_urls: HashSet.new

  #######
  # API #
  #######

  def start_link do
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  def add_new_urls(urls) when is_list(urls) do
    :gen_server.call(__MODULE__, {:add_new_urls, urls})
  end

  def add_fetched_url(url) do
    :gen_server.cast(__MODULE__, {:remove_new_url, url})
    :gen_server.cast(__MODULE__, {:add_fetched_url, url})
  end

  def request_new_url do
    :gen_server.call(__MODULE__, :request_new_url)
  end

  def statistics do
    :gen_server.call(__MODULE__, :statistics)
  end

  #######################
  # GenServer callbacks #
  #######################

  def init([]) do
    { :ok, State.new(new_urls: HashSet.new, fetched_urls: HashSet.new) }
  end

  def handle_call({:add_new_urls, urls}, _from, state) do
    new_urls = Enum.reject(urls, fn(url) -> HashSet.member?(state.fetched_urls, url) end)
    
    case new_urls do
      [] -> 
        new_state = state
      _ ->
        new_state = State.new(new_urls: HashSet.union(HashSet.new(new_urls), 
                                                      state.new_urls), 
                              fetched_urls: state.fetched_urls)
    end
    {:reply, new_urls, new_state} 
  end

  def handle_call(:request_new_url, _from, state) do
    [h|t] = state.new_urls |> HashSet.to_list 
    new_state = State.new(new_urls: HashSet.new(t), 
                          fetched_urls: state.fetched_urls)
    {:reply, h, new_state}
  end

  def handle_call(:statistics, _from, state) do
    reply = "Fetched: #{state.fetched_urls.size}\nNew: #{state.new_urls.size}"
    {:reply, reply, state}
  end

  def handle_cast({:add_fetched_url, url}, state) do
    new_state = State.new(new_urls: state.new_urls, 
                          fetched_urls: HashSet.put(state.fetched_urls, url))
    Lager.info "Fetched: #{new_state.fetched_urls.size}"
    {:noreply, new_state} 
  end

  def handle_cast({:remove_new_url, url}, state) do
    new_state = State.new(new_urls: HashSet.delete(state.new_urls, url),
                          fetched_urls: state.fetched_urls)
    Lager.info "Remaining: #{new_state.new_urls.size}"
    {:noreply, new_state} 
  end

end
