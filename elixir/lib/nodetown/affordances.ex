defmodule NodeTown.Affordances do
  @affordances [
    {:remind, "remind me"},
    {:show, "show a picture"},
    {:search, "search for"},
    {:search, "google"},
    {:search, "look up"},
    {:remember, "remember"}
  ]

  @dimensions 1536

  def new do
    # FAISS is Facebook's fast in-memory vector similarity search
    index = ExFaiss.Index.new(@dimensions, "Flat")

    # Get embeddings for all the affordances
    docs =
      @affordances
      |> Enum.map(fn {tag, text} ->
        {{tag, text}, GPT3.embedding!(text)}
      end)

    # Add them to the FAISS index in order
    {_, index} =
      Enum.reduce(docs, {0, index}, fn {_, tensor}, {i, idx} ->
        {i + 1, ExFaiss.Index.add(idx, tensor)}
      end)

    %{index: index, docs: docs}
  end

  def make_prompt(message),
    do: """
    Determine the relevant capability to use for helping the user.
    Only use the available capabilities.
    If no capabilities seem relevant, return nil.

    Available capabilities:
      remind - set reminders, etc
      make_art - generate pictures, etc
      search - search the web for info

    Message: \"hey can you search for good gloves?\"
    Result: search

    Message: \"please generate a picture of a beautiful dog\"
    Result: make_art

    Message: \"could you remind me to paint a tree?\"
    Result: remind

    Message: \"hey what's up?\"
    Result: nil

    Message: #{inspect(message)}
    Result:
    """

  def guess_with_llm!(message) do
    prompt = make_prompt(message)

    GPT3.complete!(
      context: NodeTown.NS.AI.Classify,
      model: "text-babbage-001",
      max_tokens: GPT3.estimate_tokens(prompt) + 20,
      temperature: 0.5,
      #      top_p: 0.5,
      stop: "]",
      prompt: prompt
    )
  end

  @doc """
    Find the top k relevant affordances with percentage similarity.
  """
  def guess(%{index: index, docs: docs}, message, k) do
    embedding = GPT3.embedding!(message)
    result = ExFaiss.Index.search(index, embedding, k)

    tags =
      result.labels
      |> Nx.to_flat_list()
      |> Enum.map(fn i -> Enum.at(docs, i) end)
      |> Enum.map(fn {{tag, _text}, _} -> tag end)

    distances =
      result.distances
      |> Nx.to_flat_list()
      |> Enum.map(&round(100 - &1 * 100))

    Enum.zip(tags, distances)
  end
end
