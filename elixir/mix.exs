defmodule NodeTown.MixProject do
  use Mix.Project

  def project do
    [
      app: :nodetown,
      version: "0.1.0",
      elixir: "~> 1.14",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      aliases: aliases(),
      deps: deps()
    ]
  end

  # Configuration for the OTP application.
  #
  # Type `mix help compile.app` for more information.
  def application do
    [
      mod: {NodeTown.Application, []},
      extra_applications: [:logger, :runtime_tools]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Specifies your project dependencies.
  #
  # Type `mix help deps` for examples and options.
  defp deps do
    [
      {:phoenix, "~> 1.7.0-rc.2", override: true},
      {:phoenix_ecto, "~> 4.4"},
      {:ecto_sql, "~> 3.6"},
      {:ecto_sqlite3, ">= 0.0.0"},
      {:phoenix_html, "~> 3.0"},
      {:phoenix_live_reload, "~> 1.2", only: :dev},
      {:phoenix_live_view, "~> 0.18.3"},
      {:heroicons, "~> 0.5"},
      {:phoenix_live_dashboard, "~> 0.7.2"},
      {:esbuild, "~> 0.5", runtime: Mix.env() == :dev},
      {:tailwind, "~> 0.1.8", runtime: Mix.env() == :dev},
      {:swoosh, "~> 1.3"},
      {:finch, "~> 0.13"},
      {:telemetry_metrics, "~> 0.6"},
      {:telemetry_poller, "~> 1.0"},
      {:gettext, "~> 0.20"},
      {:jason, "~> 1.2"},
      {:plug_cowboy, "~> 2.5"},
      {:crawly, "~> 0.14.0"},
      {:floki, "~> 0.33.0"},
      {:kino, "~> 0.8.0"},
      {:openai, "~> 0.2.3"},
      {:telegram, github: "visciang/telegram", tag: "0.22.4"},
      {:hackney, "~> 1.18"},
      {:oban, github: "sorentwo/oban", rev: "4e1e6cf45b205fe2383fab72b2dc176d68bedb77"},
      {:rdf, "~> 1.1"},
      {:rdf_xml, "~> 1.0"},
      {:json_ld, "~> 0.3"},
      {:sparql, "~> 0.3"},
      {:grax, "~> 0.3"},
      {:req, "~> 0.3"},
      {:req_easyhtml, "~> 0.1.0"},
      {:waffle_gcs, "~> 0.2"},
      {:earmark, "~> 1.4"},
      {:temp, "~> 0.4"},
      {:tz, "~> 0.24.0"},
      {:ex_faiss, github: "elixir-nx/ex_faiss"},
      {:nx, "~> 0.4"},
      {:exla, "~> 0.4"},
      {:tokenizers, "~> 0.2"},
      {:retry, "~> 0.17"},
      {:phoenix_inline_svg, "~> 1.4"},
      {:terminusdb_client, path: "vendor/terminusdb-client-elixir"},
      {:nostrum, github: "Kraigie/nostrum"},
      {:gun, "== 2.0.1", [env: :prod, repo: "hexpm", hex: "remedy_gun", override: true]},
      {:cowlib, "~> 2.11.1",
       [env: :prod, hex: "remedy_cowlib", repo: "hexpm", optional: false, override: true]},
      {:exmoji, "~> 0.3.0"},
      {:exile, "~> 0.1.0"},
      {:fifo, "~> 0.1.0"},
      {:redix, "~> 1.1"}
    ]
  end

  # Aliases are shortcuts or tasks specific to the current project.
  # For example, to install project dependencies and perform other setup tasks, run:
  #
  #     $ mix setup
  #
  # See the documentation for `Mix` for more info on aliases.
  defp aliases do
    [
      setup: ["deps.get", "ecto.setup", "assets.setup"],
      "ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],
      "ecto.reset": ["ecto.drop", "ecto.setup"],
      test: ["ecto.create --quiet", "ecto.migrate --quiet", "test"],
      "assets.setup": ["tailwind.install --if-missing", "esbuild.install --if-missing"],
      "assets.deploy": ["tailwind default --minify", "esbuild default --minify", "phx.digest"]
    ]
  end
end