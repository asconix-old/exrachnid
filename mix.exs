defmodule Exrachnid.Mixfile do
  use Mix.Project

  def project do
    [ app: :exrachnid,
      version: "0.0.1",
      elixir: "~> 0.12.1-dev",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [
      mod: { Exrachnid, [] },
      applications: [ :httpotion ]
    ]
  end

  defp deps do
    [ 
      { :httpotion, github: "myfreeweb/httpotion" }
    ]
  end
end
