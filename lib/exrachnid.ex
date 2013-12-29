defmodule Exrachnid do
  use Application.Behaviour

  # See http://elixir-lang.org/docs/stable/Application.Behaviour.html
  # for more information on OTP Applications
  def start(_type, _args) do
    Exrachnid.Supervisor.start_link
  end

  #######
  # API #
  #######
  def crawl(url) do
    Exrachnid.Worker.crawl(url)
  end

  # Returns added urls
  def add_new_urls(urls) do
    urls |>  
    Exrachnid.DbServer.add_new_urls |>
    Enum.each(fn(url) -> crawl(url) end)
  end

  def add_fetched_url(url) do
    Exrachnid.DbServer.add_fetched_url(url)
  end

  def remove_new_url(url) do
    Exrachnid.DbServer.remove_new_url(url)
  end

end
