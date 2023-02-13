defmodule NodeTown.Repo.Migrations.AddDataToScrapeItems do
  use Ecto.Migration

  def change do
    alter table(:scrape_items) do
      add :data, :map
    end
  end
end
