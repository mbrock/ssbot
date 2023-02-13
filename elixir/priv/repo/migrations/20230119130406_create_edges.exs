defmodule NodeTown.Repo.Migrations.CreateEdges do
  use Ecto.Migration

  def change do
    create table(:edges, primary_key: false) do
      add :id, :binary_id, primary_key: true
      add :data, :map
      add :src, references(:nodes, on_delete: :nothing, type: :binary_id)
      add :dst, references(:nodes, on_delete: :nothing, type: :binary_id)

      timestamps()
    end

    create index(:edges, [:src])
    create index(:edges, [:dst])

    create unique_index(:edges, [:src, :dst, :data])
  end
end
