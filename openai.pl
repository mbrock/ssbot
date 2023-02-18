:- module(openai,
          [ completion/3
          , completion/4
          , embeddings/2
          , embedding/2
          , known_embedding/2
          , save_many_embeddings/1
          , vector_similarity/3
          , similarity/3
          , known_similarity/3
          , similarity_search/3
          , ask/2
          , ask/1
          ]).

:- use_module(base).
:- use_module(apis).

:- use_module(library(persistency)).

:- persistent
    known_embedding(input:text, vector:list).

:- db_attach("openai.db", []).

:- debug(embeddings).

completion(P, Options, Completion) :-
    completion(P, "text-davinci-003", Options, Completion).

completion(Prompt, Engine, Options, Completion) :-
    Path = ["v1", "engines", Engine, "completions"],
    put_dict(_{prompt: Prompt}, Options, Options2),
    api_post(openai, Path, json(Options2), Result),
    member(Completion, Result.choices).

embedding_model("text-embedding-ada-002").

embeddings(Inputs, Vectors) :-
    debug(embeddings, "Embedding...", []),
    embedding_model(Model),
    Body = json(_{input: Inputs, model: Model}),
    api_post(openai, ["v1", "embeddings"], Body, Result),
    findall(V, (member(X, Result.data), V = X.embedding), Vectors),
    debug(embedding, "embeddings(~q)", [Inputs]),
    maplist(assert_known_embedding, Inputs, Vectors).

save_many_embeddings(Inputs) :-
    ChunkSize = 25,
    findnsols(
        ChunkSize, X,
        (member(X, Inputs), \+ known_embedding(X, _)),
        Chunk
    ),
    embeddings(Chunk, _).

embedding(Input, Vector) :-
    known_embedding(Input, Vector),
    !.

embedding(Input, Vector) :-
    embeddings([Input], [Vector]).

vector_dot(V1, V2, Dot) :-
    maplist({}/[X,Y,Z]>>(Z is X*Y), V1, V2, Products),
    sum_list(Products, Dot).

vector_similarity(V1, V2, Similarity) :-
    % OpenAI embeddings are normalized, so we can just take the dot product.
    vector_dot(V1, V2, Similarity).

similarity(I1, I2, Similarity) :-
    embedding(I1, V1),
    embedding(I2, V2),
    vector_similarity(V1, V2, Similarity).

known_similarity(I1, I2, Similarity) :-
    embedding(I1, V1),
    known_embedding(I2, V2),
    vector_similarity(V1, V2, Similarity).

similarity_search(I1, I2, Similarity) :-
    findall(S-I2, known_similarity(I1, I2, S), Result),
    keysort(Result, Sorted),
    reverse(Sorted, Result2),
    member(Similarity-I2, Result2).

ask(Question, Answer) :-
    Options = _{max_tokens: 200, temperature: 0.1},
    once(completion(Question, Options, Completion)),
    Answer = Completion.text.

ask(Question) :-
    ask(Question, Answer),
    format("~s~n", [Answer]).
