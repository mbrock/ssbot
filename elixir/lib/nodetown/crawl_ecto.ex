defmodule NodeTown.CrawlEcto do
  @behaviour Crawly.Pipeline

  @impl Crawly.Pipeline
  def run(item, state) do
    NodeTown.Scrape.create_item(%{
      url: item.url,
      html: item.html
    })

    {false, state}
  end
end
