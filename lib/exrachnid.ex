defmodule Exrachnid do
  use Application.Behaviour

  def start(_type, _args) do
    Exrachnid.Supervisor.start_link
  end

  #######
  # API #
  #######
  def crawl do
    Exrachnid.Worker.crawl("http://www.zalora.sg")
  end

  def crawl(url) do
    Exrachnid.Worker.crawl(url)
  end

  def add_new_urls(urls) do
    urls |> Exrachnid.DbServer.add_new_urls 
      # |> Enum.each(fn(url) -> crawl(url) end)
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
end
