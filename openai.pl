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
          , ask/1,
            edit/3,
            chat/3,
            chat/2,
            chat/4
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

json_chat([]) --> [].
json_chat([(Role - Content)|T]) -->
    [json{role: Role, content: Content}],
    json_chat(T).

chat(Lines, Message) :-
    chat(Lines, _{}, Message).

chat(Lines, Options, Message) :-
    chat(Lines, "gpt-3.5-turbo", Options, Message).

chat(Chat, Model, Options, Message) :-
    Path = ["v1", "chat", "completions"],
    phrase(json_chat(Chat), ChatJSON),
    put_dict(_{model: Model, messages: ChatJSON}, Options, Options2),
    api_post(openai, Path, json(Options2), Result),
    member(Message, Result.choices).

edit(Input, Instruction, Options, Output) :-
    % Use the /edits endpoint.
    % https://platform.openai.com/docs/api-reference/edits/create
    Path = ["v1", "edits"],
    put_dict(_{input: Input, instruction: Instruction}, Options, Options2),
    api_post(openai, Path, json(Options2), Result),
    debug(apis, "Result: ~p", [Result]),
    member(Output, Result.choices).

edit(Input, Instruction, Answer) :-
    Options = _{temperature: 0.2, model: "code-davinci-edit-001"},
    once(edit(Input, Instruction, Options, Answer)).

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
    string_length(Question, L),
    % estimate GPT-3 tokens of question
    Tokens is L // 4,
    MaxTokens is 800 + Tokens,
    Options = _{max_tokens: MaxTokens, temperature: 0.7},
    once(completion(Question, Options, Completion)),
    Answer = Completion.text.

ask(Question) :-
    ask(Question, Answer),
    format("~s~n", [Answer]).


