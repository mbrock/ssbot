defmodule Scrapers.SS do
  def base_url(), do: "https://www.ss.com/en/"

  def init() do
    [
      start_urls: [
        "https://www.ss.com/en/real-estate/flats/riga/centre/"
      ]
    ]
  end

  def parse_item(response) do
    {:ok, document} = Floki.parse_document(response.body)

    items =
      document
      |> Floki.find("#content_main_div")
      |> Enum.map(fn root ->
        %{
          id: response.request_url,
          url: response.request_url,
          html: root |> Floki.raw_html()
        }
      end)

    # How do we identify unique ads?
    # The URLs are not unique; they get reused after some time.
    # So we use the URL plus the inner text of the ad's row.
    # Hash that into a UUID...
    rows =
      document
      |> Floki.find("#head_line ~ tr")
      |> Enum.map(fn tr ->
        %{
          url: tr |> Floki.find(".msga2 a") |> Floki.attribute("href"),
          text: tr |> Floki.text()
        }
      end)
      |> IO.inspect(label: "ss rows")

    requests =
      rows
      |> Enum.map(fn row ->
        uuid =
          UUID.uuid5(:dns, "node.town")
          |> UUID.uuid5("#{row.url}: #{row.text}")
          |> IO.inspect(label: "row uuid")

        %{url: row.url, uuid: uuid}
      end)
      |> Enum.filter(fn row ->
        not NodeTown.Repo.exists?(NodeTown.Scrape.Item, row.uuid)
      end)
      |> Crawly.Utils.build_absolute_urls(base_url())
      |> Crawly.Utils.requests_from_urls()

    %Crawly.ParsedItem{
      :items => items,
      :requests => requests
    }
  end
end
