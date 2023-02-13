defmodule NodeTown.Scrape.Worker do
  use Oban.Worker, queue: :default

  def summarize do
    for item <- NodeTown.Scrape.list_scrape_items(),
        item.data["summary"] == nil do
      start_ss_gpt_job(item.id)
    end
  end

  def start_ss_gpt_job(id) do
    NodeTown.SS.GPTWorker.new(%{"id" => id})
    |> Oban.insert()
  end

  @impl Oban.Worker
  def perform(%Oban.Job{}) do
    Bots.SSLV.scrape()
    summarize()
    :ok
  end
end

defmodule NodeTown.SS.GPTWorker do
  use Oban.Worker, queue: :openai, unique: [period: :infinity]

  def work(id) do
    item = NodeTown.Scrape.get_item!(id)
    data = Bots.SSLV.grok(item)

    if false do
      {:ok, summary} = Bots.SSLV.gpt3_describe(data)
      IO.puts(summary)
      IO.puts("")

      data = Map.put(data, :summary, summary)
      {:ok, item} = NodeTown.Scrape.update_item(item, %{data: data})

      if Bots.SSLV.do_judge(id) do
        Bots.SSLV.notify(item)
      end
    end
  end

  @impl Oban.Worker
  def perform(%Oban.Job{args: %{"id" => id}}) do
    work(id)
    :ok
  end
end
