defmodule NodeTownWeb.EdgeLive.Index do
  use NodeTownWeb, :live_view

  alias NodeTown.Graph
  alias NodeTown.Graph.Edge

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, :edges, list_edges())}
  end

  @impl true
  def handle_params(params, _url, socket) do
    {:noreply, apply_action(socket, socket.assigns.live_action, params)}
  end

  defp apply_action(socket, :edit, %{"id" => id}) do
    socket
    |> assign(:page_title, "Edit Edge")
    |> assign(:edge, Graph.get_edge!(id))
  end

  defp apply_action(socket, :new, _params) do
    socket
    |> assign(:page_title, "New Edge")
    |> assign(:edge, %Edge{})
  end

  defp apply_action(socket, :index, _params) do
    socket
    |> assign(:page_title, "Listing Edges")
    |> assign(:edge, nil)
  end

  @impl true
  def handle_event("delete", %{"id" => id}, socket) do
    edge = Graph.get_edge!(id)
    {:ok, _} = Graph.delete_edge(edge)

    {:noreply, assign(socket, :edges, list_edges())}
  end

  defp list_edges do
    Graph.list_edges()
  end
end
