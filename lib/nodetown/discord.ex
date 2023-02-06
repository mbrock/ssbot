defmodule NodeTown.Discord do
  require Logger
  use Nostrum.Consumer

  alias Nostrum.Api

  def start_link do
    Consumer.start_link(__MODULE__)
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

  def handle_event({:MESSAGE_CREATE, msg, _ws_state} = event) do
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

  # Default event handler, if you don't include this, your consumer WILL crash if
  # you don't have a method definition for each event type.
  def handle_event(event) do
    Logger.debug(inspect(event))
    :noop
  end
end
