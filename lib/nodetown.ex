defmodule NodeTown do
  defmodule SSLV do
    require Logger

    def do_judge(item) do
      relevant = NodeTown.SS.gpt3_judge(item)
      data = Map.put(item.data, :relevant, relevant)
      {:ok, _} = NodeTown.Scrape.update_item(item, %{data: data})

      relevant
    end

    def notify(item) do
      NodeTown.SS.notify(item)
    end

    def row_uuid(url, text) do
      UUID.uuid5(:dns, "node.town")
      |> UUID.uuid5("#{url}: #{text}")
      |> Ecto.UUID.cast!()
    end

    def scrape do
      req = Req.new(http_errors: :raise)

      roots =
        Enum.map(
          [
            "https://www.ss.com/en/real-estate/flats/riga/centre/",
            "https://www.ss.com/en/real-estate/flats/riga/maskavas-priekshpilseta/",
            "https://www.ss.com/en/real-estate/flats/riga/grizinkalns/"
          ],
          &URI.parse/1
        )

      for root <- roots,
          body =
            Req.get!(req, url: root).body
            |> Floki.parse_document!(),
          rows =
            body
            |> Floki.find("#head_line ~ tr")
            |> Enum.take(100)
            |> Enum.map(fn tr ->
              path =
                tr
                |> Floki.find(".msga2 a")
                |> Floki.attribute("href")
                |> List.first()

              text = tr |> Floki.text(sep: "; ")

              %{
                path: path,
                text: text
              }
            end)
            |> Enum.filter(& &1.path)
            |> Enum.map(fn %{path: path, text: text} ->
              url = root |> URI.merge(path) |> URI.to_string()

              %{
                id: row_uuid(url, text),
                url: url,
                text: text
              }
            end),
          %{url: url, id: id} <- rows,
          nil == NodeTown.Repo.get(NodeTown.Scrape.Item, id, log: false),
          item = SSLV.scrape_item(req, url, id),
          {:ok, x} = NodeTown.Scrape.create_item(item) do
        x
      end
    end

    def scrape_item(req, url, id) do
      body =
        Req.get!(req, url: url).body
        |> Floki.parse_document!()

      body
      |> Floki.find("#content_main_div")
      |> List.first()
      |> then(fn root ->
        %{
          id: id,
          url: url,
          html: root |> Floki.raw_html(pretty: false),
          data: %{}
        }
      end)
      |> then(fn item ->
        %{item | data: NodeTown.SS.grok(item)}
      end)
    end
  end

  def scrape do
    SSLV.scrape()
  end

  def summarize do
    for item <- NodeTown.Scrape.list_scrape_items(),
        item.data["summary"] == nil do
      start_ss_gpt_job(item.id)
    end
  end

  def summarize_foo do
    for item <- NodeTown.Scrape.list_scrape_items() |> Enum.take(2) do
      {:ok, summary} = NodeTown.SS.gpt3_describe(NodeTown.SS.grok(item))
      IO.puts(summary)
      IO.puts("")
    end
  end

  def start_scrape_job do
    NodeTown.Scrape.Worker.new(%{})
    |> Oban.insert()
  end

  def start_ss_gpt_job(id) do
    NodeTown.SS.GPTWorker.new(%{"id" => id})
    |> Oban.insert()
  end

  def update_search_context(context, command) do
    prompt = """
      [Constructing a complex search filter incrementally.]

      Search filter before:
      #{context}

      Filter manipulation command:
      #{command}

      Search filter after:
    """

    text =
      gpt3(
        prompt: prompt,
        temperature: 0,
        max_tokens: 400,
        top_p: 1.0
      )

    IO.puts(text)
    IO.puts("")

    text
  end

  def complete(model, options) do
    {:ok, result} = OpenAI.completions(model, options)
    %{choices: [%{"text" => text}]} = result
    String.trim(text)
  end

  def gpt3(options) do
    complete("text-davinci-003", options)
  end

  def codex(options) do
    complete("code-davinci-002", options)
  end
end
