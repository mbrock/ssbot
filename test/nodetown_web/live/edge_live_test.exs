defmodule NodeTownWeb.EdgeLiveTest do
  use NodeTownWeb.ConnCase

  import Phoenix.LiveViewTest
  import NodeTown.GraphFixtures

  @create_attrs %{data: %{}}
  @update_attrs %{data: %{}}
  @invalid_attrs %{data: nil}

  defp create_edge(_) do
    edge = edge_fixture()
    %{edge: edge}
  end

  describe "Index" do
    setup [:create_edge]

    test "lists all edges", %{conn: conn} do
      {:ok, _index_live, html} = live(conn, ~p"/edges")

      assert html =~ "Listing Edges"
    end

    test "saves new edge", %{conn: conn} do
      {:ok, index_live, _html} = live(conn, ~p"/edges")

      assert index_live |> element("a", "New Edge") |> render_click() =~
               "New Edge"

      assert_patch(index_live, ~p"/edges/new")

      assert index_live
             |> form("#edge-form", edge: @invalid_attrs)
             |> render_change() =~ "can&#39;t be blank"

      {:ok, _, html} =
        index_live
        |> form("#edge-form", edge: @create_attrs)
        |> render_submit()
        |> follow_redirect(conn, ~p"/edges")

      assert html =~ "Edge created successfully"
    end

    test "updates edge in listing", %{conn: conn, edge: edge} do
      {:ok, index_live, _html} = live(conn, ~p"/edges")

      assert index_live |> element("#edges-#{edge.id} a", "Edit") |> render_click() =~
               "Edit Edge"

      assert_patch(index_live, ~p"/edges/#{edge}/edit")

      assert index_live
             |> form("#edge-form", edge: @invalid_attrs)
             |> render_change() =~ "can&#39;t be blank"

      {:ok, _, html} =
        index_live
        |> form("#edge-form", edge: @update_attrs)
        |> render_submit()
        |> follow_redirect(conn, ~p"/edges")

      assert html =~ "Edge updated successfully"
    end

    test "deletes edge in listing", %{conn: conn, edge: edge} do
      {:ok, index_live, _html} = live(conn, ~p"/edges")

      assert index_live |> element("#edges-#{edge.id} a", "Delete") |> render_click()
      refute has_element?(index_live, "#edge-#{edge.id}")
    end
  end

  describe "Show" do
    setup [:create_edge]

    test "displays edge", %{conn: conn, edge: edge} do
      {:ok, _show_live, html} = live(conn, ~p"/edges/#{edge}")

      assert html =~ "Show Edge"
    end

    test "updates edge within modal", %{conn: conn, edge: edge} do
      {:ok, show_live, _html} = live(conn, ~p"/edges/#{edge}")

      assert show_live |> element("a", "Edit") |> render_click() =~
               "Edit Edge"

      assert_patch(show_live, ~p"/edges/#{edge}/show/edit")

      assert show_live
             |> form("#edge-form", edge: @invalid_attrs)
             |> render_change() =~ "can&#39;t be blank"

      {:ok, _, html} =
        show_live
        |> form("#edge-form", edge: @update_attrs)
        |> render_submit()
        |> follow_redirect(conn, ~p"/edges/#{edge}")

      assert html =~ "Edge updated successfully"
    end
  end
end
