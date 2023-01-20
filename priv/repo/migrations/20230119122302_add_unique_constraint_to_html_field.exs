defmodule NodeTown.Repo.Migrations.AddUniqueConstraintToHtmlField do
  use Ecto.Migration

  def change do
    create unique_index(:scrape_items, :html)
  end
end
