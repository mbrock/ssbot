defmodule GPT3 do
  use RDF

  alias NodeTown.NS.{ActivityStreams, Net, AI}

  def choose_model(purpose) do
    case purpose do
      :translate_text_to_code ->
        "code-davinci-002"

      _ ->
        "text-davinci-003"
    end
  end

  def estimate_tokens(txt) do
    String.length(txt) / 4.0
  end

  def complete(model, options) do
    {:ok, %{choices: [%{"text" => text}]}} =
      model
      |> OpenAI.completions(options)

    text = String.trim(text)

    inference =
      NodeTown.gensym()
      |> RDF.type(AI.Inference)
      |> ActivityStreams.content(text)
      |> ActivityStreams.attributedTo(model)

    text
  end
end

defmodule NodeTown do
  def scrape do
    Bots.SSLV.scrape()
  end

  def summarize do
    for item <- NodeTown.Scrape.list_scrape_items(),
        item.data["summary"] == nil do
      start_ss_gpt_job(item.id)
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
      Write a short paragraph in #{lang} about some interesting topic,
      for someone who is learning #{lang}.

      Make it something that could be fun to talk to a little kid about.

      Level: easy to moderate

      Output (Markdown blockquote, names **bold**):
      """
    )
  end

  def daily_eink() do
    text = """
    #{weather_gpt3("Latvian")}

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

  def gensym() do
    RDF.Resource.Generator.generate(
      generator: RDF.IRI.UUID.Generator,
      prefix: "https://node.town/"
    )
  end

  defmodule TelegramBot do
    use Telegram.ChatBot
    use RDF

    alias NodeTown.NS.{ActivityStreams, Net}

    @impl Telegram.ChatBot
    def init(_chat) do
      {:ok, %{threads: %{}}}
    end

    def find_message_by_id(id) do
      result =
        NodeTown.Graph.query([
          {:x?, Net.telegramId(), id}
        ])

      case result do
        [%{x: x}] ->
          x

        [] ->
          NodeTown.gensym()
          |> RDF.type(ActivityStreams.Note)
          |> Net.telegramId(id)
          |> NodeTown.Graph.remember()
          |> RDF.Description.subject()
      end
    end

    def find_chat_by_id(chat_id) do
      result =
        NodeTown.Graph.query([
          {:x?, Net.telegramId(), chat_id}
        ])

      case result do
        [%{x: chat}] ->
          chat

        [] ->
          NodeTown.gensym()
          |> RDF.type(ActivityStreams.Group)
          |> Net.telegramId(chat_id)
          |> NodeTown.Graph.remember()
          |> RDF.Description.subject()
      end
    end

    def describe_new_message(
          _token,
          %{
            "message_id" => telegram_message_id,
            "chat" => %{
              "id" => telegram_chat_id
            },
            "text" => text
          } = message
        ) do
      audience = find_chat_by_id(telegram_chat_id)

      NodeTown.gensym()
      |> RDF.type(ActivityStreams.Note)
      |> Net.telegramData(Jason.encode!(message))
      |> ActivityStreams.content(text)
      |> ActivityStreams.audience(audience)
      |> Net.telegramId(telegram_message_id)
      |> then(fn x ->
        case message["reply_to_message"]["message_id"] do
          nil ->
            x

          parent_id ->
            parent = find_message_by_id(parent_id)
            x |> ActivityStreams.inReplyTo(parent)
        end
      end)
      |> NodeTown.Graph.remember()
    end

    @impl Telegram.ChatBot
    def handle_update(
          %{"message" => %{"text" => text, "chat" => %{"id" => chat_id}} = message},
          token,
          state
        ) do
      describe_new_message(token, message)

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

    def start_narrative_agent(
          {chat_id, token},
          %{core_identity: core_identity} = memory,
          state
        ) do
      response =
        NodeTown.gpt3(
          max_tokens: 50,
          temperature: 0.8,
          prompt: """
          # Contextual Narrative Agent Transcript
          # Date: #{DateTime.now!("Europe/Riga")}

          # Core identity statement

          #{core_identity}

          # My greeting
          """
        )

      say(response, chat_id, token)
      |> then(fn %{"message_id" => message_id} ->
        {:ok, put_in(state, [:threads, message_id], {:narrative, memory})}
      end)
    end

    def handle_text("/gpt", chat_id, token, state) do
      "Give me your prompt."
      |> say(chat_id, token)
      |> then(fn %{"message_id" => message_id} ->
        {:ok, put_in(state, [:threads, message_id], :await_prompt)}
      end)
    end

    def handle_text("/help", chat_id, token, state) do
      start_narrative_agent(
        {chat_id, token},
        %{
          core_identity: ~S"""
          I'm your fixer, your buddy, your coach.  I'm here to help you.

          I know a lot and I'm happy to provide concrete advice.
          I'll ask you questions whenever it seems useful.
          What I don't know myself, I'll guide you through figuring out.

          As a conversational bot, I can only talk and understand.
          I cannot make anything happen in the world.
          """,
          synopsis: ~S"""
          We just started talking.
          """
        },
        state
      )
    end

    def handle_text("/chat", chat_id, token, state) do
      start_narrative_agent(
        {chat_id, token},
        %{
          core_identity: ~S"""
          I like talking about ideas, projects, philosophical speculations, etc.
          I'll often just ask simple questions from you, to help you work through questions, etc.
          If you ask me to come up with something I'll do it right away unless there's something I don't understand.
          As a chat bot, I'm only here to talk. I can't actually do anything in the world.

          I remember what we've talked about, and what I know about you and your life-world,
          in the form of a "synopsis" with three parts.
          """,
          synopsis: ~S"""
          Context:
          Our conversation just started.

          Beliefs about you:
          You are fundamentally good.

          Knowledge about your life:
          n/a
          """
        },
        state
      )
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

    # def handle_reply(
    #       {:eval, {_x, bs}},
    #       src,
    #       id,
    #       tok,
    #       s
    #     ) do
    #   {x, bs} = Code.eval_string(src, bs)
    #   r = say(inspect(x, pretty: true), id, tok)
    #   {:ok, put_in(s, [:threads, r["message_id"]], {:eval, {x, bs}})}
    # end

    def handle_reply(
          {:narrative, %{core_identity: core_identity, synopsis: synopsis} = memory},
          text,
          chat_id,
          token,
          state
        ) do
      response =
        NodeTown.gpt3(
          max_tokens: 1000,
          temperature: 0.6,
          prompt: """
          # Narrative Agent Transcript
          # Date: #{DateTime.now!("Europe/Riga")}

          # Agent's core identity statement
          #{core_identity}

          # Agent's synopsis of the conversation so far
          #{synopsis}

          # User input
          #{text}

          # Agent output
          """
        )

      %{"message_id" => message_id} = say(response, chat_id, token)

      new_synopsis =
        NodeTown.gpt3(
          max_tokens: 1000,
          temperature: 0,
          prompt: """
          # Contextual Intelligent Narrative Agent
          # Date: #{DateTime.now!("Europe/Riga")}

          # Agent description
          #{core_identity}

          # Agent's synopsis
          #{synopsis}

          # User input
          #{text}

          # Agent output
          #{response}

          # Agent's updated synopsis
          """
        )

      # say("Synopsis: #{new_synopsis}", chat_id, token)

      new_memory = %{memory | synopsis: new_synopsis}

      {:ok, put_in(state, [:threads, message_id], {:narrative, new_memory})}
    end

    def handle_reply(_continuation, _text, _id, _token, state) do
      {:ok, state}
    end
  end

  def remember(x) do
    NodeTown.Graph.remember(x)
  end
end
