defmodule NodeTown.Graph.Edge do
  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id
  schema "edges" do
    field :data, :map
    field :src, :binary_id
    field :dst, :binary_id

    timestamps()
  end

  @doc false
  def changeset(edge, attrs) do
    edge
    |> cast(attrs, [:data])
    |> validate_required([:data])
  end
end
