defmodule NodeTown.NS do
  use RDF.Vocabulary.Namespace

  defvocab(Town,
    base_iri: "https://node.town/",
    terms: [
      :Graph
    ]
  )

  defvocab(Schema,
    base_iri: "https://schema.org/",
    file: "schemaorg-current-https.ttl",
    terms: [
      :Apartment,
      :City,
      :Conversation,
      :sameAs,
      :subjectOf,
      :url,
      :identifier,
      :text,
      :mentions,
      :keywords,
      :isPartOf,
      :hasPart
    ]
  )

  defvocab(ActivityStreams,
    base_iri: "http://www.w3.org/ns/activitystreams#",
    file: "activitystreams-owl.ttl"
  )
end

defmodule NodeTown.Graph do
  @data_path "nodetown.ttl"

  require RDF.Graph

  use Agent

  def start_link(_) do
    Agent.start_link(fn -> load!() end, name: __MODULE__)
  end

  def start() do
    start_link(load!())
  end

  def get do
    Agent.get(__MODULE__, fn x -> x end)
  end

  def update(f) do
    Agent.update(__MODULE__, fn x ->
      x |> f.() |> save()
    end)
  end

  def base do
    RDF.Graph.build do
      @base NodeTown.NS.Town
      @prefix as: NodeTown.NS.ActivityStreams

      ~I<foo#ssbot>
      |> a(ActivityStreams.Service)
    end
  end

  def load!() do
    with {:ok, graph} <- RDF.read_file(@data_path) do
      base() |> RDF.Graph.add(graph)
    else
      _ ->
        base() |> save()
    end
  end

  def save(graph) do
    RDF.write_file!(graph, @data_path, force: true)
    graph
  end
end
