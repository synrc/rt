defmodule RT.Mixfile do
  use Mix.Project

  def project() do
    [
      app: :rt,
      version: "0.10.0",
      elixir: "~> 1.7",
      description: "RT Erlang Runtime Library",
      package: package(),
      deps: deps()
    ]
  end

  def package do
    [
      files: ~w(doc src mix.exs LICENSE),
      licenses: ["ISC"],
      maintainers: ["Namdak Tonpa"],
      name: :rt,
      links: %{"GitHub" => "https://github.com/voxoz/rt"}
    ]
  end

  def application() do
    [ mod: {:rt, []} ]
  end

  def deps() do
    [
      {:ex_doc, "~> 0.11", only: :dev}
    ]
  end
end
