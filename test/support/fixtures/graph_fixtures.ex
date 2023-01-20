defmodule NodeTown.GraphFixtures do
  @moduledoc """
  This module defines test helpers for creating
  entities via the `NodeTown.Graph` context.
  """

  @doc """
  Generate a node.
  """
  def node_fixture(attrs \\ %{}) do
    {:ok, node} =
      attrs
      |> Enum.into(%{
        data: %{}
      })
      |> NodeTown.Graph.create_node()

    node
  end

  @doc """
  Generate a edge.
  """
  def edge_fixture(attrs \\ %{}) do
    {:ok, edge} =
      attrs
      |> Enum.into(%{
        data: %{}
      })
      |> NodeTown.Graph.create_edge()

    edge
  end
end
