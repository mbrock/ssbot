defmodule NodeTown.Repo do
  use Ecto.Repo,
    otp_app: :nodetown,
    adapter: Ecto.Adapters.SQLite3
end
