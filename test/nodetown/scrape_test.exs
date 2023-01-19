defmodule NodeTown.ScrapeTest do
  use NodeTown.DataCase

  alias NodeTown.Scrape

  describe "scrape_items" do
    alias NodeTown.Scrape.Item

    import NodeTown.ScrapeFixtures

    @invalid_attrs %{html: nil, url: nil}

    test "list_scrape_items/0 returns all scrape_items" do
      item = item_fixture()
      assert Scrape.list_scrape_items() == [item]
    end

    test "get_item!/1 returns the item with given id" do
      item = item_fixture()
      assert Scrape.get_item!(item.id) == item
    end

    test "create_item/1 with valid data creates a item" do
      valid_attrs = %{html: "some html", url: "some url"}

      assert {:ok, %Item{} = item} = Scrape.create_item(valid_attrs)
      assert item.html == "some html"
      assert item.url == "some url"
    end

    test "create_item/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Scrape.create_item(@invalid_attrs)
    end

    test "update_item/2 with valid data updates the item" do
      item = item_fixture()
      update_attrs = %{html: "some updated html", url: "some updated url"}

      assert {:ok, %Item{} = item} = Scrape.update_item(item, update_attrs)
      assert item.html == "some updated html"
      assert item.url == "some updated url"
    end

    test "update_item/2 with invalid data returns error changeset" do
      item = item_fixture()
      assert {:error, %Ecto.Changeset{}} = Scrape.update_item(item, @invalid_attrs)
      assert item == Scrape.get_item!(item.id)
    end

    test "delete_item/1 deletes the item" do
      item = item_fixture()
      assert {:ok, %Item{}} = Scrape.delete_item(item)
      assert_raise Ecto.NoResultsError, fn -> Scrape.get_item!(item.id) end
    end

    test "change_item/1 returns a item changeset" do
      item = item_fixture()
      assert %Ecto.Changeset{} = Scrape.change_item(item)
    end
  end
end
