defmodule NodeTown.NS do
  use RDF.Vocabulary.Namespace

  defvocab(Schema,
    base_iri: "http://schema.org/",
    terms: [
      :Thing,
      :Product,
      :Accomodation,
      :category,
      :description,
      :image,
      :url,
      :keywords,
      :model,
      :size
    ]
  )
end
