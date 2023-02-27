:- module(qdrant, [setup/0,
                   save_old_embeddings/0,
                   save_points/2,
                   search/3
                  ]).

:- use_module(apis).
:- use_module(know).
:- use_module(openai, [embedding/2]).

:- use_module(library(http/json), [json_read/2]).

%%% api_post(Service, PathComponents, Body, Result) :-
%%%     atomics_to_string(PathComponents, "/", Path),
%%%     once(api_auth(Service, Auth)),
%%%     api_url(Service, Path, URL),
%%%     user_agent(Service, UserAgent),
%%%     once(http_post(URL, Body, Result,
%%%                    [request_header('Authorization'=Auth),
%%%                     request_header('User-Agent'=UserAgent),
%%%                     json_object(dict)])),
%%%     hear(post(Service, PathComponents, Body)).

%%% api_post(Service, PathComponents, Result) :-
%%%     api_post(Service, PathComponents, json(_{}), Result).

%%% % api_put similarly

%% curl -X PUT 'http://localhost:6333/collections/test_collection' \
%%     -H 'Content-Type: application/json' \
%%     --data-raw '{
%%         "vectors": {
%%             "size": 4,
%%             "distance": "Dot"
%%         }
%%     }'

dimensions(openai, 1536).
metric(openai, 'Cosine').

new_collection(Collection, Result) :-
    dimensions(openai, Dimensions),
    metric(openai, Metric),
    Body = json(_{vectors: _{size: Dimensions, distance: Metric}}),
    api_put(qdrant, [collections, Collection], Body, Result).

save_points(Points, Result) :-
    Body = json(_{points: Points}),
    api_put(qdrant, [collections, e, points], Body, Result).

search(Vector, Limit, Result) :-
    is_list(Vector),
    Body = json(_{vector: Vector, limit: Limit, with_payload: true}),
    api_post(qdrant, [collections, e, points, search], Body, Result),
    !.

search(Text, Limit, Result) :-
    string(Text),
    embedding(Text, Vector),
    search(Vector, Limit, Result).

search(Text, Limit, Result) :-
    atom(Text),
    atom_string(Text, String),
    search(String, Limit, Result).

setup :-
    new_collection(e, Result),
    debug(qdrant, 'new collection: ~w', [Result]).

save_old_embeddings :-
    rdf(URL, rdf:type, nt:'ai/Embedding'),
    rdf(URL, nt:'ai/input', Text^^xsd:string),
    rdf(URL, nt:'ai/output', EmbeddingJSON^^xsd:string),
    open_string(EmbeddingJSON, Stream),
    json_read(Stream, Embedding),
    string_concat("https://node.town/", UUID, URL),
    Payload = _{text: Text},
    Point = _{id: UUID, payload: Payload, vector: Embedding},
    save_points([Point], Result),
    debug(qdrant, 'save point: ~w', [Result]).
