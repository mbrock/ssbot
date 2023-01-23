defmodule NodeTown.Scrape.Worker do
  use Oban.Worker, queue: :default

  @impl Oban.Worker
  def perform(%Oban.Job{}) do
    NodeTown.scrape()
    NodeTown.summarize()
  end
end

defmodule NodeTown.SS.GPTWorker do
  use Oban.Worker, queue: :openai, unique: [period: :infinity]

  def work(id) do
    item = NodeTown.Scrape.get_item!(id)
    data = NodeTown.SS.grok(item)

    {:ok, summary} = NodeTown.SS.gpt3_describe(data)
    IO.puts(summary)
    IO.puts("")

    data = Map.put(data, :summary, summary)
    {:ok, item} = NodeTown.Scrape.update_item(item, %{data: data})

    if NodeTown.SSLV.do_judge(id) do
      NodeTown.SSLV.notify(item)
    end
  end

  @impl Oban.Worker
  def perform(%Oban.Job{args: %{"id" => id}}) do
    work(id)
  end
end
