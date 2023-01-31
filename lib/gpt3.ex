defmodule GPT3 do
  use GenServer

  use RDF
  use Retry

  alias NodeTown.NS.{ActivityStreams, AI}

  def start_link(opts) do
    GenServer.start_link(GPT3, opts, name: GPT3)
  end

  def init(state) do
    {:ok, state}
  end

  def handle_call({:complete, params}, _from, state) do
    {:ok, completion} = complete(params)
    {:reply, completion, state}
  end

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

  def request(path, body) do
    url = "https://api.openai.com#{path}"
    key = System.get_env("OPENAI_API_KEY")
    headers = [authorization: "Bearer #{key}"]

    json = Enum.into(body, %{})

    with {:ok, response} <-
           Req.post(
             url,
             headers: headers,
             json: json,
             retry: fn result ->
               case result do
                 %{status: 200} -> false
                 _ -> true
               end
             end,
             max_retries: 6
           ) do
      {:ok, response.body}
    else
      {:error, e} -> {:error, e}
    end
  end

  def complete!(options) do
    timeout = 60_000
    GenServer.call(GPT3, {:complete, options}, timeout)
  end

  def complete(options) do
    model = options[:model]
    prompt = options[:prompt]

    with {:ok, %{"choices" => [%{"text" => text}]}} <-
           request(
             "/v1/completions",
             options
           ) do
      text = String.trim(text)

      inference =
        NodeTown.gensym()
        |> RDF.type(AI.TextCompletion)
        |> ActivityStreams.attributedTo(model)
        |> AI.model(model)
        |> AI.input(prompt)
        |> AI.output(text)
        |> ActivityStreams.content(text)
        |> ActivityStreams.published(DateTime.now!("Etc/UTC"))

      NodeTown.remember(inference)

      {:ok, text}
    else
      {:error, e} -> {:error, e}
    end
  end

  def embedding!(text) do
    {:ok, %{"data" => [%{"embedding" => embedding}]}} =
      request("/v1/embeddings", %{
        input: text,
        model: "text-embedding-ada-002"
      })

    inference =
      NodeTown.gensym()
      |> RDF.type(AI.Embedding)
      |> AI.input(text)
      |> AI.output(Jason.encode!(embedding))

    embedding |> Nx.tensor()
  end
end
