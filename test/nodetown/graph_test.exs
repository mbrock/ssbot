defmodule NodeTown.GraphTest do
  use NodeTown.DataCase

  alias NodeTown.Graph

  describe "nodes" do
    alias NodeTown.Graph.Node

    import NodeTown.GraphFixtures

    @invalid_attrs %{data: nil}

    test "list_nodes/0 returns all nodes" do
      node = node_fixture()
      assert Graph.list_nodes() == [node]
    end

    test "get_node!/1 returns the node with given id" do
      node = node_fixture()
      assert Graph.get_node!(node.id) == node
    end

    test "create_node/1 with valid data creates a node" do
      valid_attrs = %{data: %{}}

      assert {:ok, %Node{} = node} = Graph.create_node(valid_attrs)
      assert node.data == %{}
    end

    test "create_node/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Graph.create_node(@invalid_attrs)
    end

    test "update_node/2 with valid data updates the node" do
      node = node_fixture()
      update_attrs = %{data: %{}}

      assert {:ok, %Node{} = node} = Graph.update_node(node, update_attrs)
      assert node.data == %{}
    end

    test "update_node/2 with invalid data returns error changeset" do
      node = node_fixture()
      assert {:error, %Ecto.Changeset{}} = Graph.update_node(node, @invalid_attrs)
      assert node == Graph.get_node!(node.id)
    end

    test "delete_node/1 deletes the node" do
      node = node_fixture()
      assert {:ok, %Node{}} = Graph.delete_node(node)
      assert_raise Ecto.NoResultsError, fn -> Graph.get_node!(node.id) end
    end

    test "change_node/1 returns a node changeset" do
      node = node_fixture()
      assert %Ecto.Changeset{} = Graph.change_node(node)
    end
  end

  describe "edges" do
    alias NodeTown.Graph.Edge

    import NodeTown.GraphFixtures

    @invalid_attrs %{data: nil}

    test "list_edges/0 returns all edges" do
      edge = edge_fixture()
      assert Graph.list_edges() == [edge]
    end

    test "get_edge!/1 returns the edge with given id" do
      edge = edge_fixture()
      assert Graph.get_edge!(edge.id) == edge
    end

    test "create_edge/1 with valid data creates a edge" do
      valid_attrs = %{data: %{}}

      assert {:ok, %Edge{} = edge} = Graph.create_edge(valid_attrs)
      assert edge.data == %{}
    end

    test "create_edge/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Graph.create_edge(@invalid_attrs)
    end

    test "update_edge/2 with valid data updates the edge" do
      edge = edge_fixture()
      update_attrs = %{data: %{}}

      assert {:ok, %Edge{} = edge} = Graph.update_edge(edge, update_attrs)
      assert edge.data == %{}
    end

    test "update_edge/2 with invalid data returns error changeset" do
      edge = edge_fixture()
      assert {:error, %Ecto.Changeset{}} = Graph.update_edge(edge, @invalid_attrs)
      assert edge == Graph.get_edge!(edge.id)
    end

    test "delete_edge/1 deletes the edge" do
      edge = edge_fixture()
      assert {:ok, %Edge{}} = Graph.delete_edge(edge)
      assert_raise Ecto.NoResultsError, fn -> Graph.get_edge!(edge.id) end
    end

    test "change_edge/1 returns a edge changeset" do
      edge = edge_fixture()
      assert %Ecto.Changeset{} = Graph.change_edge(edge)
    end
  end
end
