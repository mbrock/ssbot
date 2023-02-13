defmodule NodeTown.Schemas do
  defmodule Telegram do
    def custom do
      File.read!("vendor/tg-bot-api/custom.json")
      |> Jason.decode!()
    end

    def translate_type(t, x, required) do
      tt =
        case t do
          "integer" ->
            "xsd:integer"

          "reference" ->
            x["reference"]

          "string" ->
            "xsd:string"

          "bool" ->
            "xsd:boolean"

          "array" ->
            case x["array"]["type"] do
              "array" ->
                foo = %{
                  "@type" => "Array",
                  "@dimensions" => 2,
                  "@class" =>
                    translate_type(x["array"]["array"]["type"], x["array"]["array"], true)
                }

                IO.inspect(foo, pretty: true)

                foo

              subtype ->
                %{
                  "@type" => "Array",
                  "@dimensions" => 1,
                  "@class" => translate_type(subtype, x["array"], true)
                }
            end

          "float" ->
            "xsd:decimal"

          "any_of" ->
            case x["any_of"] do
              [%{"type" => "integer"}, %{"type" => "string"}] ->
                "xsd:string"

              [%{"type" => "reference"}, %{"type" => "string"}] ->
                "xsd:anyURI"
            end

          unknown ->
            IO.inspect(x, pretty: true)
            throw({:unknown_type, unknown, x})
        end

      case required do
        true ->
          tt

        _ ->
          case tt do
            %{} ->
              tt

            _ ->
              %{
                "@type" => "Optional",
                "@class" => tt
              }
          end
      end
    end

    def context do
      %{
        "@type": "@context",
        "@schema": "https://node.town/telegram#",
        "@base": "https://node.town/telegram/"
      }
    end

    def objects do
      custom()
      |> then(& &1["objects"])
      |> Enum.map(&{&1["name"], &1})
      |> Enum.map(fn {k, v} ->
        base = %{
          "@type" => "Class",
          "@id" => k
        }

        properties =
          (v["properties"] || [])
          |> Enum.map(fn x ->
            type = translate_type(x["type"], x, x["required"])

            {x["name"], type}
          end)
          |> Map.new()

        Map.merge(base, properties)
      end)
    end

    def schema do
      [context() | objects()]
    end

    def base_req() do
      Req.new(
        base_url: "http://localhost:6363/api",
        auth: {"admin", "nodetown"}
      )
    end

    def insert!(_update) do
      base_req()
      |> Req.post!(
        url: "/document/nt/nt",
        params: %{
          author: "mbrock",
          message: "insert update"
        }
      )
    end

    def create_schema! do
      req = base_req()

      schema = NodeTown.Schemas.Telegram.schema()

      req
      |> Req.post!(
        url: "/document/nt/nt",
        params: %{author: "mbrock", message: "schema", graph_type: "schema", full_replace: "true"},
        json: schema
      )
    end
  end
end
