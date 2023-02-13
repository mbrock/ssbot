defmodule NodeTownWeb.ItemLiveTest do
  use NodeTownWeb.ConnCase

  import Phoenix.LiveViewTest
  import NodeTown.ScrapeFixtures

  @create_attrs %{html: "some html", url: "some url"}
  @update_attrs %{html: "some updated html", url: "some updated url"}
  @invalid_attrs %{html: nil, url: nil}

  defp create_item(_) do
    item = item_fixture()
    %{item: item}
  end

  describe "Index" do
    setup [:create_item]

    test "lists all scrape_items", %{conn: conn, item: item} do
      {:ok, _index_live, html} = live(conn, ~p"/scrape_items")

      assert html =~ "Listing Scrape items"
      assert html =~ item.html
    end

    test "saves new item", %{conn: conn} do
      {:ok, index_live, _html} = live(conn, ~p"/scrape_items")

      assert index_live |> element("a", "New Item") |> render_click() =~
               "New Item"

      assert_patch(index_live, ~p"/scrape_items/new")

      assert index_live
             |> form("#item-form", item: @invalid_attrs)
             |> render_change() =~ "can&#39;t be blank"

      {:ok, _, html} =
        index_live
        |> form("#item-form", item: @create_attrs)
        |> render_submit()
        |> follow_redirect(conn, ~p"/scrape_items")

      assert html =~ "Item created successfully"
      assert html =~ "some html"
    end

    test "updates item in listing", %{conn: conn, item: item} do
      {:ok, index_live, _html} = live(conn, ~p"/scrape_items")

      assert index_live |> element("#scrape_items-#{item.id} a", "Edit") |> render_click() =~
               "Edit Item"

      assert_patch(index_live, ~p"/scrape_items/#{item}/edit")

      assert index_live
             |> form("#item-form", item: @invalid_attrs)
             |> render_change() =~ "can&#39;t be blank"

      {:ok, _, html} =
        index_live
        |> form("#item-form", item: @update_attrs)
        |> render_submit()
        |> follow_redirect(conn, ~p"/scrape_items")

      assert html =~ "Item updated successfully"
      assert html =~ "some updated html"
    end

    test "deletes item in listing", %{conn: conn, item: item} do
      {:ok, index_live, _html} = live(conn, ~p"/scrape_items")

      assert index_live |> element("#scrape_items-#{item.id} a", "Delete") |> render_click()
      refute has_element?(index_live, "#item-#{item.id}")
    end
  end

  describe "Show" do
    setup [:create_item]

    test "displays item", %{conn: conn, item: item} do
      {:ok, _show_live, html} = live(conn, ~p"/scrape_items/#{item}")

      assert html =~ "Show Item"
      assert html =~ item.html
    end

    test "updates item within modal", %{conn: conn, item: item} do
      {:ok, show_live, _html} = live(conn, ~p"/scrape_items/#{item}")

      assert show_live |> element("a", "Edit") |> render_click() =~
               "Edit Item"

      assert_patch(show_live, ~p"/scrape_items/#{item}/show/edit")

      assert show_live
             |> form("#item-form", item: @invalid_attrs)
             |> render_change() =~ "can&#39;t be blank"

      {:ok, _, html} =
        show_live
        |> form("#item-form", item: @update_attrs)
        |> render_submit()
        |> follow_redirect(conn, ~p"/scrape_items/#{item}")

      assert html =~ "Item updated successfully"
      assert html =~ "some updated html"
    end
  end
end
