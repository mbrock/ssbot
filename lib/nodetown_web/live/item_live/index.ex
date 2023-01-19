defmodule NodeTownWeb.ItemLive.Index do
  use NodeTownWeb, :live_view

  alias NodeTown.Scrape
  alias NodeTown.Scrape.Item

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, :scrape_items, list_scrape_items())}
  end

  @impl true
  def handle_params(params, _url, socket) do
    {:noreply, apply_action(socket, socket.assigns.live_action, params)}
  end

  defp apply_action(socket, :edit, %{"id" => id}) do
    socket
    |> assign(:page_title, "Edit Item")
    |> assign(:item, Scrape.get_item!(id))
  end

  defp apply_action(socket, :new, _params) do
    socket
    |> assign(:page_title, "New Item")
    |> assign(:item, %Item{})
  end

  defp apply_action(socket, :index, _params) do
    socket
    |> assign(:page_title, "Listing Scrape items")
    |> assign(:item, nil)
  end

  @impl true
  def handle_event("delete", %{"id" => id}, socket) do
    item = Scrape.get_item!(id)
    {:ok, _} = Scrape.delete_item(item)

    {:noreply, assign(socket, :scrape_items, list_scrape_items())}
  end

  defp list_scrape_items do
    Scrape.list_scrape_items()
  end
end
