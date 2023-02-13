defmodule NodeTown.Repo.Migrations.CreateScrapeItems do
  use Ecto.Migration

  def change do
    create table(:scrape_items, primary_key: false) do
      add :id, :binary_id, primary_key: true
      add :url, :string
      add :html, :string

      timestamps()
    end
  end
end
