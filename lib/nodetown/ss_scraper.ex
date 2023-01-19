defmodule Scrapers.SS do
  use Crawly.Spider

  @impl Crawly.Spider
  def base_url(), do: "https://www.ss.com/en/"

  @impl Crawly.Spider
  def init() do
    [
      start_urls: [
        "https://www.ss.com/en/real-estate/flats/riga/centre/"
      ]
    ]
  end

  @impl Crawly.Spider
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

    requests =
      document
      |> Floki.find(".msga2 a[href$=\".html\"]")
      |> Floki.attribute("href")
      |> Enum.uniq()
      |> Crawly.Utils.build_absolute_urls(base_url())
      |> Crawly.Utils.requests_from_urls()

    %Crawly.ParsedItem{
      :items => items,
      :requests => requests
    }
  end
end
