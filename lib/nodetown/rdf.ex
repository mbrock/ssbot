defmodule NodeTown.NS do
  use RDF.Vocabulary.Namespace

  defvocab(BFO,
    base_iri: "http://purl.obolibrary.org/obo/bfo.owl#",
    file: "bfo.rdf"
  )

  defvocab(Net,
    base_iri: "https://node.town/net/",
    file: "internet-ontology.ttl"
  )

  defvocab(AI,
    base_iri: "https://node.town/ai/",
    file: "internet-ontology.ttl"
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

defmodule NodeTown.Schema do
  defmodule Discourse do
    use Grax.Schema

    schema do
      property(:platform, NodeTown.NS.ActivityStreams.Application)
    end
  end
end

defmodule NodeTown.Graph do
  @data_path "nodetown.xml"

  require RDF.Graph
  require Logger

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

  def remember(x) do
    Logger.debug("remembering #{inspect(x, pretty: true)}")
    :ok = update(&RDF.Graph.add(&1, x))
    x
  end

  def query(q) do
    graph = get()
    RDF.Graph.query(graph, q)
  end

  def base do
    RDF.Graph.build do
      @base NodeTown.NS.Net
      @prefix as: NodeTown.NS.ActivityStreams
      @prefix ai: NodeTown.NS.AI
      @prefix net: NodeTown.NS.Net

      ~I<foo#ssbot>
      |> a(ActivityStreams.Service)
    end
  end

  def load!() do
    with {:ok, graph} <- RDF.XML.read_file(@data_path) do
      base() |> RDF.Graph.add(graph)
    else
      e ->
        Logger.error("#{inspect(e)}")
        base()
    end
  end

  def save(graph) do
    RDF.XML.write_file!(graph, @data_path, force: true)
    graph
  end
end
