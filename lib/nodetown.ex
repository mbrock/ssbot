defmodule NodeTown do
  defmodule Prompts do
    def estimate_tokens(x) do
      x
      |> Map.values()
      |> Enum.map(&GPT3.estimate_tokens/1)
      |> Enum.sum()
    end

    defmodule Text do
      def context do
        """
        ## bot traits
        yeah I'm a bot... kinda like a rubber duck with memory

        I like talking about whatever you're thinking about

        when needed I can assist with planning, deciding, etc

        my specific knowledge about the world is a bit unreliable

        I like writing informally without capitalizing

        ## knowledge about user
        - they're on Telegram

        ## open questions
        - what's on their mind?

        ## conversation log
        <user> [starts a chat]
        """
      end

      def initial() do
        %{
          context: context(),
          prefix: """
          # state
          #{context()}

          # my initial greeting
          """,
          suffix: "\n#"
        }
      end

      def reply(context, text) do
        %{
          prefix: """
          # state
          #{context}

          # message from user
          #{text}

          # my response
          """,
          suffix: "\n#"
        }
      end

      def update(context, text, response) do
        %{
          prefix: """
          # state (current)
          #{context}

          # message from user
          #{text}

          # my response
          #{response}

          # state (updated)
          """,
          suffix: "\n# "
        }
      end
    end

    defmodule HTML do
      def context do
        """
          <ol id="about-me">
            <li>I'm a bot assistant.
          <ol>
          <ol id="history">
            <li>The user started a new conversation.
          </ol>
          <p id="current-goal">
            (n/a)
          </p>
        """
      end

      def initial() do
        %{
          context: """
          <section class=context>
          #{context()}
          </section>
          """,
          prefix: """
          <section class=interaction>
          <p>Bot says:
          """,
          suffix: "</p>"
        }
      end

      def reply(context, text) do
        %{
          prefix: """
          #{context}
          <section class=interaction>
          <p>User says: #{text}</p>
          <p>Bot says:
          """,
          suffix: "</p>"
        }
      end

      def update(context, text, response) do
        %{
          prefix: """
          #{context}
          <section class=interaction>
          <p>
            User said: #{text}
          </p>
          <p>
            I replied: #{response}
          </p>
          </section>
          <section class="updated context">
          """,
          suffix: "</section>"
        }
      end
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

    require Logger

    alias NodeTown.NS.{ActivityStreams, Net, AI}

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
      subject =
        describe_new_message(token, message)
        |> RDF.Description.subject()

      IO.inspect(%{message: message, state: state}, pretty: true)

      case message["reply_to_message"] do
        %{"message_id" => id} ->
          reply_to(subject, id, text, chat_id, token, state)

        _ ->
          handle_text(subject, text, chat_id, token, state)
      end
    end

    def handle_update(_update, _token, state) do
      {:ok, state}
    end

    def reply_to(subject, id, text, chat_id, token, state) do
      case state.threads[id] do
        nil ->
          Logger.debug("no thread for #{id} in #{chat_id}")
          {:ok, put_in(state, [:my_latest_message], nil)}

        continuation ->
          Logger.debug("handling continuation for #{id} in #{chat_id}")
          IO.inspect(continuation, pretty: true)
          handle_reply(continuation, subject, text, chat_id, token, state)
      end
    end

    def say(text, chat_id, token, state) do
      IO.inspect(text, label: "text")

      {:ok, %{"message_id" => id} = message} =
        Telegram.Api.request(
          token,
          "sendMessage",
          chat_id: chat_id,
          text: text
        )

      describe_new_message(token, message)
      |> ActivityStreams.author(AI.NodeTown)
      |> NodeTown.Graph.remember()

      {id, %{state | my_latest_message: id}}
    end

    def start_narrative_agent(
          subject,
          {chat_id, token},
          %{context: context, prefix: prefix, suffix: suffix} = x,
          state
        ) do
      tokens = Prompts.estimate_tokens(x)

      response =
        GPT3.complete!(
          context: subject,
          model: "text-davinci-003",
          max_tokens: tokens + 256,
          temperature: 0.6,
          prompt: """
          #{context}
          #{prefix}
          """,
          stop: suffix
        )

      {id, state} = say(response, chat_id, token, state)

      response_entity =
        find_message_by_id(id)
        |> AI.chatContext(context)
        |> NodeTown.Graph.remember()

      prompt2 = Prompts.Text.update(context, "(n/a)", response)

      new_synopsis =
        GPT3.complete!(
          context: RDF.Description.subject(response_entity),
          model: "text-davinci-003",
          max_tokens: Prompts.estimate_tokens(prompt2) + 100,
          temperature: 0,
          prompt: prompt2.prefix,
          stop: prompt2.suffix
        )

      {:ok, put_in(state, [:threads, id], {:narrative, new_synopsis})}
    end

    def handle_text(_subject, "/gpt", chat_id, token, state) do
      {id, state} = say("Give me your prompt.", chat_id, token, state)
      {:ok, put_in(state, [:threads, id], :await_prompt)}
    end

    def handle_text(subject, "/chat", chat_id, token, state) do
      start_narrative_agent(
        subject,
        {chat_id, token},
        Prompts.Text.initial(),
        state
      )
    end

    def handle_text(
          subject,
          text,
          chat_id,
          token,
          %{my_latest_message: my_latest_message} = state
        )
        when my_latest_message != nil do
      Logger.debug("autoreplying to #{my_latest_message}")
      reply_to(subject, my_latest_message, text, chat_id, token, state)
    end

    def handle_text(_subject, _text, _id, _token, state) do
      {:ok, put_in(state, [:my_latest_message], nil)}
    end

    def handle_reply(
          :await_prompt,
          subject,
          prompt,
          chat_id,
          token,
          state
        ) do
      text =
        GPT3.complete!(
          context: subject,
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
          subject,
          text,
          chat_id,
          token,
          state
        ) do
      prompt = Prompts.Text.reply(synopsis, text)

      response =
        GPT3.complete!(
          context: subject,
          model: "text-davinci-003",
          max_tokens: Prompts.estimate_tokens(prompt) + 512,
          temperature: 0.3,
          prompt: prompt.prefix,
          stop: prompt.suffix
        )

      {id, state} = say(response, chat_id, token, state)

      response_entity =
        find_message_by_id(id)
        |> AI.chatContext(synopsis)
        |> NodeTown.Graph.remember()

      prompt2 = Prompts.Text.update(synopsis, text, response)

      new_synopsis =
        GPT3.complete!(
          context: RDF.Description.subject(response_entity),
          model: "text-davinci-003",
          max_tokens: Prompts.estimate_tokens(prompt2) + 512,
          temperature: 0,
          prompt: prompt2.prefix,
          stop: prompt2.suffix
        )

      IO.puts(new_synopsis)
      IO.puts("")

      {:ok, put_in(state, [:threads, id], {:narrative, new_synopsis})}
    end

    def handle_reply(_continuation, _subject, _text, _id, _token, state) do
      {:ok, state}
    end
  end

  def remember(x) do
    NodeTown.Graph.remember(x)
  end
end
