defmodule NodeTownWeb.EdgeLive.Show do
  use NodeTownWeb, :live_view

  alias NodeTown.Graph

  @impl true
  def mount(_params, _session, socket) do
    {:ok, socket}
  end

  @impl true
  def handle_params(%{"id" => id}, _, socket) do
    {:noreply,
     socket
     |> assign(:page_title, page_title(socket.assigns.live_action))
     |> assign(:edge, Graph.get_edge!(id))}
  end

  defp page_title(:show), do: "Show Edge"
  defp page_title(:edit), do: "Edit Edge"
end