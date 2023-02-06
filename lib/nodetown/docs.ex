defmodule NodeTown.Docs do
  @moduledoc """
  This module indexes the documentation for Elixir modules and functions.

  It generates RDF descriptions for the documentation.

  It can also use OpenAI's API to generate vector embeddings for each module and function.

  Finally, it can use GPT-3 to summarize and answer questions.
  """

  use RDF

  alias NodeTown.NS.Code, as: CodeNS
  alias RDF.NS.RDFS

  def rdf_graph(mods) do
    mods
    |> Enum.map(&module_rdf_description/1)
    |> RDF.Graph.new()
  end

  def huge_rdf_graph() do
    :code.all_loaded()
    |> Enum.filter(fn {mod, _} -> "#{mod}" =~ ~r{^[A-Z]} end)
    |> Enum.map(fn {mod, _} -> mod end)
    |> rdf_graph()
  end

  def module_rdf_description(mod) do
    {
      :docs_v1,
      _,
      :elixir,
      _,
      %{"en" => moddoc},
      _metadata,
      docs
    } = Code.fetch_docs(mod)

    mod_iri = RDF.IRI.new("https://node.town/docs/elixir/#{mod}")

    mod_doc =
      mod_iri
      |> RDF.type(CodeNS.Module)
      |> RDFS.label("#{mod}")
      |> RDFS.comment(moddoc)

    docs
    |> Enum.map(&function_rdf(mod_iri, &1))
    |> Enum.reduce(mod_doc, &CodeNS.child/2)
  end

  def function_rdf(mod, {{:function, name, arity}, _, _, info}) do
    function_iri = RDF.IRI.new("https://node.town/docs/elixir/#{mod}/#{name}/#{arity}")

    function_doc =
      function_iri
      |> RDF.type(CodeNS.Function)
      |> RDFS.label("#{mod}.#{name}/#{arity}")
      |> CodeNS.parent(mod)
      |> add_function_info(arity, info)

    function_doc
  end

  def add_function_info(doc, arity, info) do
    description =
      case info do
        %{"en" => s} -> s
        %{"deprecated" => s} -> "Deprecated: #{s}"
      end

    doc
    |> CodeNS.arity(arity)
    |> CodeNS.documentation(description)
  end
end
