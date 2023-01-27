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
    IO.inspect(options)
    {:ok, result} = OpenAI.completions(model, options)
    %{choices: [%{"text" => text}]} = result
    IO.inspect(text)
    String.trim(text)
  end

  def gpt3_embedding(text) do
    key = System.get_env("OPENAI_API_KEY")

    response =
      Req.post!(
        "https://api.openai.com/v1/embeddings",
        headers: [authorization: "Bearer #{key}"],
        json: %{input: text, model: "text-embedding-ada-002"}
      )

    %{"data" => [%{"embedding" => embedding}]} = response.body

    {:ok, embedding |> Nx.tensor()}
  end

  def gpt3(options) do
    complete("text-davinci-003", options)
  end

  def codex(options) do
    complete("code-davinci-002", options)
  end

  def eink_latex!(latex) do
    Req.new(
      http_errors: :raise,
      method: :post,
      body: latex,
      headers: [content_type: "text/plain"]
    )
    |> Req.request!(url: "http://urbion/epap/DISPLAY-LATEX")
  end

  def eink!(img) do
    Req.new(
      http_errors: :raise,
      method: :post,
      body: img,
      headers: [content_type: "image/png"]
    )
    |> Req.request!(url: "http://urbion/epap/DISPLAY-IMAGE")
  end

  def latex_preamble do
    ~S"""
    \documentclass[12pt,twocolumn]{extarticle}
    \usepackage[
      paperwidth=209.66mm,paperheight=157.25mm,
      margin=0.8cm,includefoot]{geometry}
    \usepackage[
      width=209.66mm,height=157.25mm,center,frame,noinfo]{crop}
    \usepackage{parskip}
    \usepackage{ebgaramond}
    \usepackage[sc]{titlesec}
    \begin{document}
    """
  end

  def latex_postamble do
    ~S"""
    \end{document}
    """
  end

  def latex(src) do
    """
    #{latex_preamble()}
    #{src}
    #{latex_postamble()}
    """
  end

  def latex_to_png_file(src) do
    Temp.track!()

    dir_path = Temp.mkdir!("nodetown-latex")
    tex_path = Path.join(dir_path, "doc.tex") |> IO.inspect(label: :tex_path)
    File.write!(tex_path, src)

    System.cmd(
      "latex",
      [tex_path],
      cd: dir_path,
      into: IO.stream()
    )

    System.cmd(
      "dvipng",
      ["-D", "226.785", "doc"],
      cd: dir_path,
      into: IO.stream()
    )

    File.read!(Path.join(dir_path, "doc1.png"))
  end

  def org_to_latex(src) do
    Temp.track!()

    org_path = Temp.path!("nodetown.org")
    File.write!(org_path, src)

    case System.cmd("pandoc", ["-f", "markdown", "-t", "latex", org_path]) do
      {org, 0} ->
        {:ok, org}

      {_, status} ->
        {:error, {:pandoc, status}}
    end
  end

  def weather do
    {lat, lon} = {56.9475072, 24.1401856}
    key = Application.fetch_env!(:nodetown, :weather_api_key)
    url = "https://api.pirateweather.net/forecast/#{key}/#{lat},#{lon}?units=si"
    Req.get!(url).body
  end

  def weather_gpt3(lang) do
    weather =
      NodeTown.weather()
      |> Access.get("currently")
      |> Map.take(
        ~w"apparentTemperature humidity icon precipIntensity precipProbability precipType summary temperature"
      )
      |> Map.put("time", to_string(DateTime.now!("Europe/Riga")))

    NodeTown.gpt3(
      max_tokens: 300,
      temperature: 0,
      prompt: """
      Given weather data, write a brief, informative summary,
      in #{lang}.
      Write in the present tense.
      Use casual language. Don't use excessive decimals.
      Omit needless words like "currently".
      Mention the current date and time, simplified.

      Weather data:
      #{inspect(weather, pretty: true)}

      Summary (#{lang}, Markdown):
      """
    )
  end

  def language_gpt3(lang) do
    NodeTown.gpt3(
      max_tokens: 500,
      temperature: 0.5,
      prompt: """
      Write a short dialogue in #{lang} about some everyday topic,
      between two named characters, one male and female,
      for someone who is learning #{lang}.

      Level: moderate

      Output (Markdown blockquote, names **bold**):
      """
    )
  end

  def daily_eink() do
    text = """
    #{weather_gpt3("Latvian")}

    #{language_gpt3("Latvian")}

    #{weather_gpt3("Svenska")}

    #{language_gpt3("Svenska")}
    """

    {:ok, src} = NodeTown.org_to_latex(text)

    src
  end

  defmodule EinkWorker do
    use Oban.Worker, queue: :openai

    @impl Oban.Worker
    def perform(%Oban.Job{}) do
      NodeTown.daily_eink()
      |> IO.inspect()
      |> NodeTown.eink_latex!()

      :ok
    end
  end

  defmodule TelegramBot do
    use Telegram.ChatBot

    @impl Telegram.ChatBot
    def init(_chat) do
      {:ok, %{threads: %{}}}
    end

    @impl Telegram.ChatBot
    def handle_update(
          %{"message" => %{"text" => text, "chat" => %{"id" => chat_id}} = message},
          token,
          state
        ) do
      case message["reply_to_message"] do
        %{"message_id" => reply_to_message_id} ->
          case state.threads[reply_to_message_id] do
            nil ->
              {:ok, state}

            continuation ->
              handle_reply(continuation, text, chat_id, token, state)
          end

        _ ->
          handle_text(text, chat_id, token, state)
      end
    end

    def handle_update(_update, _token, state) do
      {:ok, state}
    end

    def say(text, chat_id, token) do
      IO.inspect(text, label: "text")

      {:ok, message} =
        Telegram.Api.request(
          token,
          "sendMessage",
          chat_id: chat_id,
          text: text
        )

      message
    end

    def handle_text("/gpt", chat_id, token, state) do
      "Give me your prompt."
      |> say(chat_id, token)
      |> then(fn %{"message_id" => message_id} ->
        {:ok, put_in(state, [:threads, message_id], :await_prompt)}
      end)
    end

    def handle_text("/chat", chat_id, token, state) do
      synopsis = ~S"""
      I am an intelligent contextual conversation agent.

      I always remember a "synopsis" of my identity and the conversation thus far.
      I know what we're talking about, and I remember important details.

      I am a good listener, and I ask good questions with a coaching spirit,
      yet I'm humble and kind of funny.

      I'm always trying to find the next concrete step.
      """

      response =
        NodeTown.gpt3(
          max_tokens: 1000,
          temperature: 0.7,
          prompt: """
          Date: #{DateTime.now!("Europe/Riga")}

          # Initial synopsis

          #{synopsis}

          # Initial witty greeting
          """
        )

      say(response, chat_id, token)
      |> then(fn %{"message_id" => message_id} ->
        {:ok, put_in(state, [:threads, message_id], {:chat, synopsis})}
      end)
    end

    def handle_text(_text, _id, _token, state) do
      {:ok, state}
    end

    def handle_reply(
          :await_prompt,
          prompt,
          chat_id,
          token,
          state
        ) do
      NodeTown.gpt3(
        max_tokens: 400,
        prompt: prompt,
        temperature: 0.0
      )
      |> say(chat_id, token)

      {:ok, state}
    end

    def handle_reply({:chat, synopsis}, text, chat_id, token, state) do
      response =
        NodeTown.gpt3(
          max_tokens: 1000,
          temperature: 0.6,
          prompt: """
          Date: #{DateTime.now!("Europe/Riga")}

          # Current synopsis

          #{synopsis}

          # They said

          #{text}

          # I say
          """
        )

      %{"message_id" => message_id} = say(response, chat_id, token)

      new_synopsis =
        NodeTown.gpt3(
          max_tokens: 1000,
          temperature: 0.2,
          prompt: """
          # Current synopsis

          #{synopsis}

          # They said

          #{text}

          # I said

          #{response}

          # Updated synopsis
          """
        )

      # say("Synopsis: #{new_synopsis}", chat_id, token)

      {:ok, put_in(state, [:threads, message_id], {:chat, new_synopsis})}
    end

    def handle_reply(_continuation, _text, _id, _token, state) do
      {:ok, state}
    end
  end
end
