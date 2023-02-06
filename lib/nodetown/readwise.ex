defmodule NodeTown.Readwise do
  @moduledoc """
  Readwise is a service for collects highlights from your Kindle
  and other sources.

  This module provides a simple interface to the Readwise API.
  """

  @base_url "https://readwise.io/api/v2"

  require Logger

  def foo() do
    stream_to_file(System.get_env("READWISE_TOKEN"), "readwise.jsonl")
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
