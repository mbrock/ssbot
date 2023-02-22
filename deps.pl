need(sweet).
need(spawn).
need(markdown).
need(mqtt).
need(openapi).

:- forall(need(N), pack_install(N)).
