defmodule Exrachnid.Mixfile do
  use Mix.Project

  def project do
    [ app: :exrachnid,
      version: "0.0.1",
      elixir: "~> 0.12.1-dev",
      elixirc_options: options(Mix.env),
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [
      mod: { Exrachnid, [] },
      applications: [ 
                      :inets,
                      :ibrowse,
                      :hackney,
                      :exlager 
                    ]
    ]
  end

  def options(env) when env in [:dev, :test] do
    [ exlager_level: :debug ]
  end

  def options(env) when env in [:prod] do
    [ exlager_level: :warning ]
  end

  defp deps do
    [ 
      { :ibrowse, github: "cmullaparthi/ibrowse" },
      { :hackney, github: "benoitc/hackney" },
      { :exlager, github: "khia/exlager" },
      { :poolboy, github: "devinus/poolboy", tag: "1.0.0" }
    ]
  end
end
