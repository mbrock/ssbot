defmodule NodeTownWeb.GraphLive.Index do
  use NodeTownWeb, :live_view

  alias NodeTown.NS.ActivityStreams, as: AS
  alias NodeTown.NS.AI

  use RDF

  require Logger

  @impl true
  def mount(_params, _session, socket) do
    descriptions =
      NodeTown.Graph.get()
      |> RDF.Data.descriptions()
      |> Enum.filter(& &1[AS.published()])
      |> Enum.sort_by(
        &List.first(&1[AS.published()]),
        {:desc, RDF.XSD.DateTime}
      )
      |> Enum.take(50)

    {:ok, assign(socket, :descriptions, descriptions)}
  end

  @impl true
  def handle_params(params, _url, socket) do
    {:noreply, apply_action(socket, socket.assigns.live_action, params)}
  end

  defp apply_action(socket, :index, _params) do
    socket
    |> assign(:page_title, "Node.Town :: Graph")
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flow-root w-96">
      <ul :for={description <- @descriptions} role="list" class="-mb-8">
        <li>
          <div class="relative pb-8">
            <span class="absolute top-5 left-5 -ml-px h-full w-0.5 bg-gray-200" />
            <div class="relative flex items-start space-x-3 text-sm">
              <%= render_description(description) %>
            </div>
          </div>
        </li>
      </ul>
    </div>
    """
  end

  def render_description(description) do
    [type] = description[RDF.type()]

    assigns = %{
      type: type,
      item: description,
      subject: RDF.Description.subject(description),
      fields: RDF.Description.values(description)
    }

    render_one(assigns)
  end

  slot :icon, required: true
  slot :data, required: true

  def feed_item(assigns) do
    ~H"""
    <div class="relative flex items-start space-x-3">
      <div>
        <div class="relative px-1">
          <div class="flex h-8 w-8 items-center justify-center rounded-full bg-gray-100 ring-8 ring-white">
            <%= render_slot(@icon) %>
          </div>
        </div>
      </div>
      <div class="min-w-0 flex-1 py-1.5">
        <%= render_slot(@data) %>
      </div>
    </div>
    """
  end

  def render_one(%{type: term_to_iri(AI.TextCompletion)} = assigns) do
    ~H"""
    <.feed_item>
      <:icon>
        <svg
          class="h-5 w-5 text-gray-500"
          fill="currentColor"
          viewBox="0 0 24 24"
          role="img"
          xmlns="http://www.w3.org/2000/svg"
        >
          <title>OpenAI icon</title>
          <path d="M22.2819 9.8211a5.9847 5.9847 0 0 0-.5157-4.9108 6.0462 6.0462 0 0 0-6.5098-2.9A6.0651 6.0651 0 0 0 4.9807 4.1818a5.9847 5.9847 0 0 0-3.9977 2.9 6.0462 6.0462 0 0 0 .7427 7.0966 5.98 5.98 0 0 0 .511 4.9107 6.051 6.051 0 0 0 6.5146 2.9001A5.9847 5.9847 0 0 0 13.2599 24a6.0557 6.0557 0 0 0 5.7718-4.2058 5.9894 5.9894 0 0 0 3.9977-2.9001 6.0557 6.0557 0 0 0-.7475-7.0729zm-9.022 12.6081a4.4755 4.4755 0 0 1-2.8764-1.0408l.1419-.0804 4.7783-2.7582a.7948.7948 0 0 0 .3927-.6813v-6.7369l2.02 1.1686a.071.071 0 0 1 .038.052v5.5826a4.504 4.504 0 0 1-4.4945 4.4944zm-9.6607-4.1254a4.4708 4.4708 0 0 1-.5346-3.0137l.142.0852 4.783 2.7582a.7712.7712 0 0 0 .7806 0l5.8428-3.3685v2.3324a.0804.0804 0 0 1-.0332.0615L9.74 19.9502a4.4992 4.4992 0 0 1-6.1408-1.6464zM2.3408 7.8956a4.485 4.485 0 0 1 2.3655-1.9728V11.6a.7664.7664 0 0 0 .3879.6765l5.8144 3.3543-2.0201 1.1685a.0757.0757 0 0 1-.071 0l-4.8303-2.7865A4.504 4.504 0 0 1 2.3408 7.872zm16.5963 3.8558L13.1038 8.364 15.1192 7.2a.0757.0757 0 0 1 .071 0l4.8303 2.7913a4.4944 4.4944 0 0 1-.6765 8.1042v-5.6772a.79.79 0 0 0-.407-.667zm2.0107-3.0231l-.142-.0852-4.7735-2.7818a.7759.7759 0 0 0-.7854 0L9.409 9.2297V6.8974a.0662.0662 0 0 1 .0284-.0615l4.8303-2.7866a4.4992 4.4992 0 0 1 6.6802 4.66zM8.3065 12.863l-2.02-1.1638a.0804.0804 0 0 1-.038-.0567V6.0742a4.4992 4.4992 0 0 1 7.3757-3.4537l-.142.0805L8.704 5.459a.7948.7948 0 0 0-.3927.6813zm1.0976-2.3654l2.602-1.4998 2.6069 1.4998v2.9994l-2.5974 1.4997-2.6067-1.4997Z" />
        </svg>
      </:icon>
      <:data>
        <article class="flex flex-col">
          <.item_header item={@item} title="GPT-3 completion" />
          <div class="p-2 text-sm text-slate-400">
            Context: <%= texts(@item, AS.context()) %>
          </div>
          <div class="p-2">
            <p class="whitespace-pre-line italic text-slate-600"><%= texts(@item, AI.input()) %></p>
            <p class="whitespace-pre-line text-gray-700"><%= texts(@item, AS.content()) %></p>
          </div>
        </article>
      </:data>
    </.feed_item>
    """
  end

  def render_one(%{type: term_to_iri(AS.Note)} = assigns) do
    ~H"""
    <.feed_item>
      <:icon>
        <svg
          xmlns="http://www.w3.org/2000/svg"
          fill="none"
          viewBox="0 0 24 24"
          stroke-width="1.5"
          stroke="currentColor"
          class="w-6 h-6"
        >
          <path
            stroke-linecap="round"
            stroke-linejoin="round"
            d="M7.5 8.25h9m-9 3H12m-9.75 1.51c0 1.6 1.123 2.994 2.707 3.227 1.129.166 2.27.293 3.423.379.35.026.67.21.865.501L12 21l2.755-4.133a1.14 1.14 0 01.865-.501 48.172 48.172 0 003.423-.379c1.584-.233 2.707-1.626 2.707-3.228V6.741c0-1.602-1.123-2.995-2.707-3.228A48.394 48.394 0 0012 3c-2.392 0-4.744.175-7.043.513C3.373 3.746 2.25 5.14 2.25 6.741v6.018z"
          />
        </svg>
      </:icon>
      <:data>
        <article class="flex flex-col">
          <.item_header item={@item} title="Text" />
          <div class="p-2 flex flex-col gap-4 text-slate-600">
            <p class="whitespace-pre-line text-sm"><%= texts(@item, AI.chatContext()) %></p>
            <p class="text-gray-700"><%= texts(@item, AS.content()) %></p>
          </div>
        </article>
      </:data>
    </.feed_item>
    """
  end

  def render_one(assigns) do
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

  def item_header(assigns) do
    ~H"""
    <header class="text-sm flex justify-between">
      <h2 class="text-sm px-2"><%= @title %></h2>
      <time class="text-slate-500 font-mono">
        <%= Calendar.strftime(date(@item), "%c") %>
      </time>
    </header>
    """
  end

  def texts(item, iri) do
    (item[iri] || []) |> Enum.map(&to_string/1) |> Enum.join("\n")
  end

  def date(item) do
    case item[AS.published()] do
      [x | _] -> RDF.Literal.value(x) |> DateTime.shift_zone!("Europe/Riga")
      _ -> nil
    end
  end
end
