# This file is responsible for configuring your application
# and its dependencies with the aid of the Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
import Config

config :nodetown,
  namespace: NodeTown,
  ecto_repos: [NodeTown.Repo],
  generators: [binary_id: true]

config :nodetown, Oban,
  engine: Oban.Engines.Lite,
  queues: [
    default: 10,
    openai: 1
  ],
  repo: NodeTown.Repo

# Configures the endpoint
config :nodetown, NodeTownWeb.Endpoint,
  url: [host: "localhost"],
  render_errors: [
    formats: [html: NodeTownWeb.ErrorHTML, json: NodeTownWeb.ErrorJSON],
    layout: false
  ],
  pubsub_server: NodeTown.PubSub,
  live_view: [signing_salt: "8/x20B5U"]

# Configures the mailer
#
# By default it uses the "Local" adapter which stores the emails
# locally. You can see the emails in your browser, at "/dev/mailbox".
#
# For production it's recommended to configure a different adapter
# at the `config/runtime.exs`.
config :nodetown, NodeTown.Mailer, adapter: Swoosh.Adapters.Local

# Configure esbuild (the version is required)
config :esbuild,
  version: "0.14.41",
  default: [
    args:
      ~w(js/app.js --bundle --target=es2017 --outdir=../priv/static/assets --external:/fonts/* --external:/images/*),
    cd: Path.expand("../assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ]

# Configure tailwind (the version is required)
config :tailwind,
  version: "3.2.4",
  default: [
    args: ~w(
      --config=tailwind.config.js
      --input=css/app.css
      --output=../priv/static/assets/app.css
    ),
    cd: Path.expand("../assets", __DIR__)
  ]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

config :elixir, :time_zone_database, Tz.TimeZoneDatabase

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

config :crawly,
  middlewares: [
    Crawly.Middlewares.DomainFilter,
    Crawly.Middlewares.UniqueRequest
  ],
  pipelines: [
    NodeTown.CrawlEcto,
    Crawly.Pipelines.JSONEncoder
  ]

config :tesla, adapter: {Tesla.Adapter.Hackney, [recv_timeout: 40_000]}

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{config_env()}.exs"
