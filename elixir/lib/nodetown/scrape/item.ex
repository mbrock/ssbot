defmodule NodeTown.Scrape.Item do
  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :binary_id, autogenerate: false}
  @foreign_key_type :binary_id
  schema "scrape_items" do
    field :html, :string
    field :url, :string
    field :data, :map, default: %{}

    timestamps()
  end

  @doc false
  def changeset(item, attrs) do
    item
    |> cast(attrs, [:url, :html, :data, :id])
    |> validate_required([:url, :html, :id])
  end
end