defmodule NodeTownWeb.EdgeLive.FormComponent do
  use NodeTownWeb, :live_component

  alias NodeTown.Graph

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.header>
        <%= @title %>
        <:subtitle>Use this form to manage edge records in your database.</:subtitle>
      </.header>

      <.simple_form
        :let={f}
        for={@changeset}
        id="edge-form"
        phx-target={@myself}
        phx-change="validate"
        phx-submit="save"
      >
        <.input field={{f, :data}} type="text" label="Data" />
        <:actions>
          <.button phx-disable-with="Saving...">Save Edge</.button>
        </:actions>
      </.simple_form>
    </div>
    """
  end

  @impl true
  def update(%{edge: edge} = assigns, socket) do
    changeset = Graph.change_edge(edge)

    {:ok,
     socket
     |> assign(assigns)
     |> assign(:changeset, changeset)}
  end

  @impl true
  def handle_event("validate", %{"edge" => edge_params}, socket) do
    changeset =
      socket.assigns.edge
      |> Graph.change_edge(edge_params)
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, :changeset, changeset)}
  end

  def handle_event("save", %{"edge" => edge_params}, socket) do
    save_edge(socket, socket.assigns.action, edge_params)
  end

  defp save_edge(socket, :edit, edge_params) do
    case Graph.update_edge(socket.assigns.edge, edge_params) do
      {:ok, _edge} ->
        {:noreply,
         socket
         |> put_flash(:info, "Edge updated successfully")
         |> push_navigate(to: socket.assigns.navigate)}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign(socket, :changeset, changeset)}
    end
  end

  defp save_edge(socket, :new, edge_params) do
    case Graph.create_edge(edge_params) do
      {:ok, _edge} ->
        {:noreply,
         socket
         |> put_flash(:info, "Edge created successfully")
         |> push_navigate(to: socket.assigns.navigate)}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign(socket, changeset: changeset)}
    end
  end
end
