defmodule NodeTown.Discord do
  require Logger

  use Nostrum.Consumer
  import Bitwise

  alias Nostrum.Api
  alias Nostrum.Struct.Interaction

  def start_link do
    Consumer.start_link(__MODULE__)
  end

  def register(guild_id) do
    command = %{
      name: "bash",
      type: 1,
      description: "start a bash shell",
      options: []
    }

    Api.create_guild_application_command(guild_id, command)
  end

  def reply_code(text, msg) do
    Api.create_message!(
      msg.channel_id,
      content: """
      ```elixir
      #{text}
      ```
      """,
      message_reference: %{message_id: msg.id}
    )
  end

  def emoji(name) do
    Exmoji.from_short_name(name) |> Exmoji.EmojiChar.render()
  end

  def handle_event({:MESSAGE_CREATE, %{author: %{bot: nil}} = msg, _ws_state} = event) do
    Logger.debug(inspect(event))

    case msg.content do
      "!ping" ->
        Api.create_message(msg.channel_id, "pyongyang!")

      "!eval " <> code ->
        {result, _} = Code.eval_string(code, msg: msg, event: event)
        inspect(result, pretty: true) |> reply_code(msg)

      "!r" ->
        reaction =
          case IEx.Helpers.recompile() do
            :ok -> emoji("+1")
            :noop -> emoji("sleeping")
          end

        Api.create_reaction!(msg.channel_id, msg.id, reaction)

      _ ->
        :ignore
    end
  end

  def handle_event(
        {:INTERACTION_CREATE, %Interaction{data: %{name: name}} = interaction, _ws_state}
      ) do
    handle_interaction(name, interaction)
  end

  # Default event handler, if you don't include this, your consumer WILL crash if
  # you don't have a method definition for each event type.
  def handle_event(event) do
    Logger.debug(inspect(event, label: "Discord event"))
    :noop
  end

  def handle_interaction("bash", interaction) do
    Logger.debug(inspect(interaction, label: "Interaction"))

    Api.create_interaction_response!(
      interaction,
      %{
        type: 4,
        data: %{
          content: "Starting shell..."
        }
      }
    )

    fifo = FIFO.new()

    Task.start(fn ->
      ["bash", "--noediting", "-i"]
      |> Exile.stream!(input: fifo, use_stderr: true)
      |> Stream.each(fn data ->
        content =
          case data do
            {:stdout, text} -> text
            {:stderr, text} -> text
          end

        Api.create_followup_message!(
          interaction.token,
          %{
            content: "```\n#{content}\n```"
          }
        )
      end)
      |> Stream.run()
    end)

    :ok
  end
end
