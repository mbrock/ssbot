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
        When needed I can assist with planning, deciding, etc.

        ## style examples
         - bad: Hello, sir, how may I help you today?
         - good: hey, how's it going?

        ## capabilities
        I can perform actions by just mentioning the following tags in my output:

        - to make a picture, drawing, photo, etc, I can do e.g.
          - <action:show "drawing of a happy dog, very beautiful, realistic">
        - to schedule a reminder, I can do e.g.
          - <remind "+7m" "hey, how about that egg?">

        ## knowledge
        - user is on Telegram

        ## open questions
        - what's on their mind?

        ## conversation log (never deleted, only pruned and summarized)
        user: [starts a chat]
        bot:
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

  #  defmodule TelegramChat do
  #    use Telegram.ChatBot
  #    use RDF
  #
  #    require Logger
  #
  #    alias NodeTown.NS.{ActivityStreams as AS, Net, AI}
  #
  #    @impl Telegram.ChatBot
  #    def init(%{"chat_id" => chat_id}) do
  #      {:ok, %{chat_iri: TelegramBot.find_chat_by_id(chat_id)}}
  #    end
  #
  #    @impl Telegram.ChatBot
  #    def handle_update(update, token, state) do
  #      NodeTown.gensym()
  #        |> RDF.type(AS.Update)
  #        |> Net.originPlatform(Net.Telegram)
  #        |> AS.audience(state.chat_iri)
  #    end
  #  end

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
          %{
            "message_id" => telegram_message_id,
            "chat" => %{
              "id" => telegram_chat_id
            }
          } = message
        ) do
      audience = find_chat_by_id(telegram_chat_id)

      timestamp = DateTime.now!("Etc/UTC")

      NodeTown.gensym()
      |> Net.telegramData(Jason.encode!(message))
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
      |> then(fn x ->
        case message do
          %{"text" => text} ->
            x
            |> RDF.type(ActivityStreams.Note)
            |> ActivityStreams.content(text)

          %{"caption" => caption} ->
            x
            |> RDF.type(ActivityStreams.Image)
            |> AI.input(caption)
            |> ActivityStreams.name(caption)
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
        describe_new_message(message)
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

    def spinner(action, chat_id, token) do
      {:ok, _} =
        Telegram.Api.request(
          token,
          "sendChatAction",
          chat_id: chat_id,
          action: action
        )
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

      describe_new_message(message)
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
      spinner("typing", chat_id, token)
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

      spinner("typing", chat_id, token)

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

    def handle_text(_subject, "/show " <> prompt, chat_id, token, state) do
      show_picture(prompt, chat_id, token)
      {:ok, state}
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
      spinner("typing", chat_id, token)

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

    def handle_reply(
          {:narrative, synopsis},
          subject,
          text,
          chat_id,
          token,
          state
        ) do
      prompt = Prompts.Text.reply(synopsis, text)

      spinner("typing", chat_id, token)

      response =
        GPT3.complete!(
          context: subject,
          model: "text-davinci-003",
          max_tokens: Prompts.estimate_tokens(prompt) + 512,
          temperature: 0.3,
          prompt: prompt.prefix,
          stop: prompt.suffix
        )

      {response, id, description, state} =
        case Regex.run(~r"<action:show \"(.*?)\">", response) do
          [_, imgprompt] ->
            {id, description} = show_picture(imgprompt, chat_id, token)

            {"<img alt=\"{imgprompt}\">", id, description, state}

          nil ->
            {id, state} = say(response, chat_id, token, state)

            description =
              find_message_by_id(id)
              |> AI.chatContext(synopsis)
              |> NodeTown.Graph.remember()

            {response, id, description, state}
        end

      prompt2 = Prompts.Text.update(synopsis, text, response)

      new_synopsis =
        GPT3.complete!(
          context: RDF.Description.subject(description),
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

    def show_picture(prompt, chat_id, token) do
      spinner("upload_photo", chat_id, token)
      imgsrc = GPT3.image!(prompt)

      inference =
        NodeTown.gensym()
        |> RDF.type(AI.Inference)
        |> ActivityStreams.published(DateTime.now!("Etc/UTC"))
        |> AI.input(prompt)
        |> ActivityStreams.image(imgsrc)
        |> NodeTown.remember()

      {:ok, %{"message_id" => id} = message} =
        Telegram.Api.request(
          token,
          "sendPhoto",
          chat_id: chat_id,
          photo: imgsrc,
          caption: prompt
        )

      description =
        describe_new_message(message)
        |> ActivityStreams.author(AI.NodeTown)
        |> ActivityStreams.image(imgsrc)
        |> ActivityStreams.generator(inference |> RDF.Description.subject())
        |> NodeTown.Graph.remember()

      {id, description}
    end
  end

  def remember(x) do
    NodeTown.Graph.remember(x)
  end
end
