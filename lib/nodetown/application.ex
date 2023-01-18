defmodule NodeTown.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Start the Telemetry supervisor
      NodeTownWeb.Telemetry,
      # Start the Ecto repository
      NodeTown.Repo,
      # Start the PubSub system
      {Phoenix.PubSub, name: NodeTown.PubSub},
      # Start Finch
      {Finch, name: NodeTown.Finch},
      # Start the Endpoint (http/https)
      NodeTownWeb.Endpoint
      # Start a worker by calling: NodeTown.Worker.start_link(arg)
      # {NodeTown.Worker, arg}
    ]

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
