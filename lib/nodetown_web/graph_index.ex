defmodule NodeTownWeb.GraphLive.Index do
  use NodeTownWeb, :live_view

  alias NodeTown.NS.ActivityStreams

  @impl true
  def mount(_params, _session, socket) do
    subjects =
      NodeTown.Graph.get()
      |> RDF.Graph.values()
      |> Map.to_list()
      |> Enum.filter(fn {_id, fields} -> fields[ActivityStreams.published().value] end)
      |> Enum.sort_by(
        fn {_id, fields} -> fields[ActivityStreams.published().value] end,
        :desc
      )
      |> Enum.map(fn {id, fields} -> %{id: id, fields: Map.to_list(fields)} end)

    {:ok, assign(socket, :subjects, subjects)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <article :for={subject <- @subjects} class="border m-2 p-2">
      <%= render_subject(subject) %>
    </article>
    """
  end

  @impl true
  def handle_params(params, _url, socket) do
    {:noreply, apply_action(socket, socket.assigns.live_action, params)}
  end

  defp apply_action(socket, :index, _params) do
    socket
    |> assign(:page_title, "Listing Scrape items")
    |> assign(:item, nil)
  end

  def render_subject(assigns) do
    ~H"""
    <table>
      <tbody>
        <tr :for={{key, value} <- @fields}>
          <td class="font-mono"><%= key %></td>
          <td><%= inspect(value) %></td>
        </tr>
      </tbody>
    </table>
    """
  end
end
