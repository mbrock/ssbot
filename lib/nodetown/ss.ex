defmodule NodeTown.SS do
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
