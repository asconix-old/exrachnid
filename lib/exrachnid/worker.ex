defmodule Exrachnid.Worker do
  require Lager
  use GenServer.Behaviour
  
  ####### 
  # API #
  ####### 

  def start_link(state) do
    :gen_server.start_link(__MODULE__, state, [{:trace}])
  end

  def crawl(url) do
    :poolboy.transaction(:worker_pool, fn(worker)-> 
                                         :gen_server.cast(worker, {:crawl, url}) 
                                       end)
  end

  #######################
  # GenServer callbacks #
  #######################

  def init(state) do
    { :ok, state }
  end

  def handle_cast({:crawl, url}, state) do
    try do 
      body = fetch_page(url) 
      Lager.info url

      Exrachnid.add_fetched_url(url)

      host = URI.parse(url).host
          
      body          
        |> extract_links(host)
        |> Exrachnid.add_new_urls
    rescue
      error ->
        Lager.error error
    end
      
    { :noreply, state }
  end

  #####################
  # Private functions #
  #####################

  def fetch_page(url) do
    {:ok, status_code, _headers, client} = :hackney.get(url) 
    
    if status_code in 200..299 do
      {:ok, body} = :hackney.body(client)
      body
    else
      ""
    end
  end

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
