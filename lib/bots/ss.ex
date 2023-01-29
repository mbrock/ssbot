defmodule Bots do
  def html_to_text(url) do
    {dump, 0} = System.cmd("links", ["-dump", "-width", "300", url])
    dump
  end

  def grok(text) do
    GPT3.complete(
      "text-davinci-003",
      prompt: """
      >>> Summarize this web page.

      #{text}

      >>> Web page summary in English:
      """,
      temperature: 0,
      max_tokens: 500
    )
  end
end

defmodule Bots.SSLV do
  require Logger

  def urls do
    [
      "https://www.ss.com/en/real-estate/flats/riga/centre/",
      "https://www.ss.com/en/real-estate/flats/riga/maskavas-priekshpilseta/",
      "https://www.ss.com/en/real-estate/flats/riga/grizinkalns/"
    ]
  end

  def to_rdf do
    urls()
    |> Enum.map(&Bots.html_to_text/1)
    |> Enum.map(&Bots.grok/1)
  end

  def do_judge(item) do
    relevant = gpt3_judge(item)
    data = Map.put(item.data, :relevant, relevant)
    {:ok, _} = NodeTown.Scrape.update_item(item, %{data: data})

    relevant
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
        urls(),
        &URI.parse/1
      )

    for root <- roots,
        rows =
          Req.get!(req, url: root).body
          |> Floki.parse_document!()
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
        item = scrape_item(req, url, id),
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
      %{item | data: grok(item)}
    end)
  end

  def extract_coordinates(attr) do
    attr
    |> String.split("=")
    |> List.last("")
    |> String.split(", ")
    |> Enum.take(2)
  end

  def grok(x) do
    doc = Floki.parse_fragment!(x.html)

    table =
      doc
      |> Floki.find(".options_list")
      |> then(fn list ->
        Enum.zip(
          list
          |> Floki.find(".ads_opt_name")
          |> Enum.map(&Floki.text/1)
          |> Enum.map(&String.trim/1)
          |> Enum.map(&String.replace_suffix(&1, ":", "")),
          list
          |> Floki.find(".ads_opt")
          |> Enum.map(&Floki.text/1)
          |> Enum.map(&String.trim/1)
          |> Enum.map(&String.replace_suffix(&1, "[Map]", ""))
        )
      end)
      |> Map.new()

    message =
      doc
      |> Floki.find("#msg_div_msg")
      |> Floki.filter_out("table")
      |> Floki.text(deep: true)
      |> String.trim()

    price =
      doc
      |> Floki.find(".ads_price#tdo_8")
      |> Floki.text()
      |> String.trim()

    coords =
      doc
      |> Floki.find("#mnu_map")
      |> Floki.attribute("onclick")
      |> List.first("")
      |> extract_coordinates()

    %{
      url: x.url,
      table: table,
      message: message,
      price: price,
      coords: coords
    }
  end

  def gpt3_prompt(item) do
    """
    Context: Ad on a marketplace.
    Data: #{item.table |> Jason.encode!(pretty: true)}
    Price: #{item.price}

    Description (Latvian/Russian/English):
    #{item.message}

    [end of description]

    Summarize in eight English bullet points, appropriate for a brief chat message to alert us about a potentially interesting ad to look at. Be objective and terse.

    Always start with "• [Type], [Size], [Location], [Price]"; then proceed in order of importance.

    Output (in English):
    """
  end

  def gpt3_describe(item) do
    prompt = gpt3_prompt(item)

    with {:ok, %{choices: [%{"text" => text}]}} <-
           OpenAI.completions(
             "text-davinci-003",
             prompt: prompt,
             max_tokens: 200,
             temperature: 0
           ) do
      {:ok, text |> String.trim()}
    end
  end

  def gpt3_judge(item) do
    prompt = """
      Task: Determine whether an item matches the user's wishes.

      Item description:
      #{item.data["summary"]}

      User wishes: flat bigger than 70 m^2

      Output: {"matched":
    """

    NodeTown.gpt3(prompt: prompt, temperature: 0, max_tokens: 100)
    |> IO.inspect(label: "GPT-3 judgment")
    |> String.starts_with?("true")
  end

  def notify(item) do
    token = Application.fetch_env!(:nodetown, :ssbot)[:telegram_token]
    chat_id = -753_420_060

    text = """
    #{item.data["summary"]}

    #{item.url}
    """

    Telegram.Api.request(token, "sendMessage",
      chat_id: chat_id,
      text: text,
      disable_web_page_preview: true
    )
  end
end
