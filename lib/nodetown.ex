defmodule NodeTown do
  def gensym() do
    RDF.Resource.Generator.generate(
      generator: RDF.IRI.UUID.Generator,
      prefix: "https://node.town/"
    )
  end

  defmodule TelegramBot do
    use Telegram.ChatBot
    use RDF

    require Logger

    alias NodeTown.NS.{ActivityStreams, Net}

    @impl Telegram.ChatBot
    def init(_chat) do
      state = %{
        threads: %{},
        my_latest_message: nil
      }

      {:ok, state}
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

      timestamp = DateTime.now!("Etc/UTC")

      NodeTown.gensym()
      |> RDF.type(ActivityStreams.Note)
      |> Net.telegramData(Jason.encode!(message))
      |> ActivityStreams.content(text)
      |> ActivityStreams.audience(audience)
      |> ActivityStreams.published(timestamp)
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

      IO.inspect(%{message: message, state: state}, pretty: true)

      case message["reply_to_message"] do
        %{"message_id" => id} ->
          reply_to(id, text, chat_id, token, state)

        _ ->
          handle_text(text, chat_id, token, state)
      end
    end

    def handle_update(_update, _token, state) do
      {:ok, state}
    end

    def reply_to(id, text, chat_id, token, state) do
      case state.threads[id] do
        nil ->
          Logger.debug("no thread for #{id} in #{chat_id}")
          {:ok, put_in(state, [:my_latest_message], nil)}

        continuation ->
          Logger.debug("handling continuation for #{id} in #{chat_id}")
          IO.inspect(continuation, pretty: true)
          handle_reply(continuation, text, chat_id, token, state)
      end
    end

    def say(text, chat_id, token, state) do
      IO.inspect(text, label: "text")

      {:ok, %{"message_id" => id}} =
        Telegram.Api.request(
          token,
          "sendMessage",
          chat_id: chat_id,
          text: text
        )

      {id, %{state | my_latest_message: id}}
    end

    def start_narrative_agent(
          {chat_id, token},
          synopsis,
          state
        ) do
      response =
        GPT3.complete!(
          model: "text-davinci-003",
          max_tokens: 50,
          temperature: 0.8,
          prompt: """
          # Date: #{DateTime.now!("Europe/Riga")}

          # Context
          #{synopsis}

          # My greeting
          """
        )

      {id, state} = say(response, chat_id, token, state)

      {:ok, put_in(state, [:threads, id], {:narrative, synopsis})}
    end

    def handle_text("/gpt", chat_id, token, state) do
      {id, state} = say("Give me your prompt.", chat_id, token, state)
      {:ok, put_in(state, [:threads, id], :await_prompt)}
    end

    def handle_text("/chat", chat_id, token, state) do
      start_narrative_agent(
        {chat_id, token},
        ~S"""
        ## About me
        I like talking about ideas, projects, philosophical speculations, etc.
        I can ask questions when that seems useful.
        I remember our conversation by noting everything in my chronicle.
        I also have a buffer, which is a file that I can edit when we're working on something.

        ## My action capabilities
        I don't have any capabilities for performing actions in the world.

        ## Conversation chronicle
        You messaged me; I greeted you.

        ## Buffer contents
        (empty)
        """,
        state
      )
    end

    def handle_text(
          text,
          chat_id,
          token,
          %{my_latest_message: my_latest_message} = state
        )
        when my_latest_message != nil do
      Logger.debug("autoreplying to #{my_latest_message}")
      reply_to(my_latest_message, text, chat_id, token, state)
    end

    def handle_text(_text, _id, _token, state) do
      {:ok, put_in(state, [:my_latest_message], nil)}
    end

    def handle_reply(
          :await_prompt,
          prompt,
          chat_id,
          token,
          state
        ) do
      text =
        GPT3.complete!(
          model: "text-davinci-003",
          max_tokens: 400,
          prompt: prompt,
          temperature: 0.0
        )

      {_, state} = say(text, chat_id, token, state)

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
          {:narrative, synopsis},
          text,
          chat_id,
          token,
          state
        ) do
      response =
        GPT3.complete!(
          model: "text-davinci-003",
          max_tokens: 1000,
          temperature: 0.3,
          prompt: """
          # Date: #{DateTime.now!("Europe/Riga")}

          # Context
          #{synopsis}

          # User message
          #{text}

          # Agent response
          """
        )

      {id, state} = say(response, chat_id, token, state)

      new_synopsis =
        GPT3.complete!(
          model: "text-davinci-003",
          max_tokens: 1500,
          temperature: 0.3,
          prompt: """
          # Date: #{DateTime.now!("Europe/Riga")}

          # Previous context
          #{synopsis}

          # User message
          #{text}

          # Agent response
          #{response}

          # Updated context
          """
        )

      IO.puts(new_synopsis)
      IO.puts("")

      {:ok, put_in(state, [:threads, id], {:narrative, new_synopsis})}
    end

    def handle_reply(_continuation, _text, _id, _token, state) do
      {:ok, state}
    end
  end

  def remember(x) do
    NodeTown.Graph.remember(x)
  end
end
