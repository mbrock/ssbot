defmodule NodeTown.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children =
      [
        {Redix, name: :redis},
        NodeTownWeb.Telemetry,
        NodeTown.Repo,
        {Phoenix.PubSub, name: NodeTown.PubSub},
        {Finch, name: NodeTown.Finch},
        {Oban, Application.fetch_env!(:nodetown, Oban)},
        NodeTownWeb.Endpoint,
        NodeTown.Graph,
        GPT3,
        NodeTown.Discord
      ] ++
        case Application.fetch_env!(:nodetown, :ssbot)[:telegram_token] do
          nil ->
            []

          token ->
            [
              {Telegram.Poller,
               bots: [
                 {NodeTown.TelegramBot, [token: token, max_bot_concurrency: 32]}
               ]}
            ]
        end

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: NodeTown.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    NodeTownWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
