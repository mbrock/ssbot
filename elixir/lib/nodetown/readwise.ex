defmodule NodeTown.Readwise do
  @moduledoc """
  Readwise is a service for collects highlights from your Kindle
  and other sources.

  This module provides a simple interface to the Readwise API.
  """

  @base_url "https://readwise.io/api/v2"

  require Logger

  use RDF

  alias NodeTown.NS.{Schema, Net}

  def foo() do
    stream_to_file(System.get_env("READWISE_TOKEN"), "readwise.jsonl")
  end

  def file_stream() do
    File.stream!("readwise.jsonl")
    |> Stream.map(&Jason.decode!/1)
  end

  def rdf_stream() do
    file_stream()
    |> Stream.map(&item_to_rdf/1)
  end

  def item_to_rdf(%{} = item) do
    {type, uri} =
      case item["category"] do
        "articles" ->
          {Schema.Article, item["unique_url"]}

        "books" ->
          {Schema.Book, "https://amazon.com/dp/#{item["asin"]}"}
      end

    iri = RDF.IRI.new(uri)

    highlights =
      item
      |> Map.get("highlights")
      |> Enum.map(&highlight_to_rdf(&1, iri))

    iri
    |> RDF.type(type)
    |> Schema.name(item["readable_title"])
    |> Schema.url(item["source_url"])
    |> Schema.author(item["author"])
    |> Schema.isbn(item["asin"])

    #    |> Net.highlights(highlights)
  end

  def highlight_to_rdf(x, source) do
    # %{
    #   "book_id" => 23343400,
    #   "color" => "yellow",
    #   "created_at" => "2023-01-16T19:20:10.876Z",
    #   "end_location" => nil,
    #   "external_id" => nil,
    #   "highlighted_at" => "2023-01-16T04:56:00Z",
    #   "id" => 457613742,
    #   "is_discard" => false,
    #   "is_favorite" => false,
    #   "location" => 204,
    #   "location_type" => "location",
    #   "note" => "",
    #   "readwise_url" => "https://readwise.io/open/457613742",
    #   "tags" => [],
    #   "text" => "Thomas arrived at the university of the Western world first as a student [...]",
    #   "updated_at" => "2023-01-16T19:20:10.876Z",
    #   "url" => nil
    # },

    {:ok, highlighted_at, 0} = DateTime.from_iso8601(x["highlighted_at"])

    RDF.IRI.new(x["readwise_url"])
    |> RDF.type(Schema.Quotation)
    |> Schema.text(x["text"])
    |> Schema.dateCreated(highlighted_at)
    |> Schema.isBasedOn(source)
  end

  def embeddings() do
    File.stream!("readwise.jsonl")
    |> Stream.map(&Jason.decode!/1)
    |> Stream.flat_map(&Map.get(&1, "highlights"))
    |> Stream.map(&Map.get(&1, "text"))
    |> Stream.each(&IO.puts/1)
    |> Stream.map(&GPT3.embedding!/1)
    |> Stream.run()
  end

  def export(token) do
    IO.inspect(token, label: "token")

    response =
      Req.get!(
        "#{@base_url}/export/",
        headers: [authorization: "Token #{token}"]
      )

    {response.body["results"], response.body["nextPageCursor"]}
  end

  def export(token, cursor) do
    response =
      Req.get!(
        "#{@base_url}/export/",
        headers: [authorization: "Token #{token}"],
        params: %{"pageCursor" => cursor}
      )

    {response.body["results"], response.body["nextPageCursor"]}
  end

  def stream(token) do
    cursor = :start

    Stream.unfold(cursor, fn cursor ->
      Logger.info("cursor: #{IO.inspect(cursor)}")

      case cursor do
        :start ->
          export(token)

        nil ->
          nil

        _ ->
          export(token, cursor)
      end
    end)
    |> Stream.flat_map(& &1)
  end

  def stream_to_file(token, path) do
    stream(token)
    #    |> Stream.each(&IO.inspect(&1["title"], label: "readwise"))
    |> Stream.map(&Jason.encode!/1)
    |> Stream.map(&(&1 <> "\n"))
    |> Stream.into(File.stream!(path))
    |> Stream.run()
  end
end
