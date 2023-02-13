defmodule NodeTown.ScrapeFixtures do
  @moduledoc """
  This module defines test helpers for creating
  entities via the `NodeTown.Scrape` context.
  """

  @doc """
  Generate a item.
  """
  def item_fixture(attrs \\ %{}) do
    {:ok, item} =
      attrs
      |> Enum.into(%{
        html: "some html",
        url: "some url"
      })
      |> NodeTown.Scrape.create_item()

    item
  end
end
