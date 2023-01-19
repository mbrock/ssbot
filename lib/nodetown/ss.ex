defmodule NodeTown.SS do
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
          |> Enum.map(&String.replace_suffix(&1, ":", "")),
          list
          |> Floki.find(".ads_opt")
          |> Enum.map(&Floki.text/1)
          |> Enum.map(&String.replace_suffix(&1, "[Map]", ""))
        )
      end)

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

    %{
      url: x.url,
      table: table,
      message: message,
      price: price,
      coords: coords,
      seen: x.inserted_at
    }
  end

  def gpt3_prompt(item) do
    """
    Context: Ad on a marketplace in Riga, Latvia.

    Task: Summarize the text description, taking context from the structured data.

    Style: Be as concise as possible, use bullet points.

    Structured data: #{item.table |> Map.new() |> Jason.encode!(pretty: true)}

    Price: #{item.price}

    Text description (Latvian/Russian/English):

    #{item.message}

    Output (with Unicode bullet points):
    """
  end

  def gpt3_describe(item) do
    prompt = gpt3_prompt(item)
    with {:ok, %{choices: [%{"text" => text}]}} <- OpenAI.completions("text-davinci-003", prompt: prompt, max_tokens: 800, temperature: 0) do
      {:ok, text |> String.trim()}
    end
  end


  def notify(item) do
    token = Application.fetch_env!(:nodetown, :ssbot)[:telegram_token]
    chat_id = -753420060

    with {:ok, gpt3} <- gpt3_describe(item) do
      text = """
      #{gpt3}

      #{item.url}
      """
      Telegram.Api.request(token, "sendMessage", chat_id: chat_id, text: text, disable_web_page_preview: true)
    end  
  end
end

