defmodule Exrachnid do
  require Lager
  use Application.Behaviour

  def start(_type, _args) do
    Exrachnid.Supervisor.start_link
  end

  #######
  # API #
  #######

  def crawl do
    Exrachnid.crawl("http://www.zalora.sg")
  end

  def crawl(url) do
    Exrachnid.Worker.crawl(url)
    {:ready, available, _, _} = :poolboy.status(:worker_pool)
    Enum.each(1..available, fn(_x) -> request_new_url end)
  end

  def add_new_urls(urls) do
    urls |> Exrachnid.DbServer.add_new_urls 
  end

  def add_fetched_url(url) do
    Exrachnid.DbServer.add_fetched_url(url)
  end

  def remove_new_url(url) do
    Exrachnid.DbServer.remove_new_url(url)
  end

  def request_new_url do
    Exrachnid.DbServer.request_new_url |> crawl
  end

  def statistics do
    Exrachnid.DbServer.statistics
  end
end
