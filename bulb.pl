:- module(bulb, [start/0
                ]).

:- use_module(base, []).
:- use_module(otp, [spin/2]).

:- use_module(library(mqtt)).

start :-
  spin(mqtt, work).

work :-
  mqtt_connect(Socket, bilbo, 1883, [client_id('https://node.town'),
                                     is_async(false)]),
  mqtt_sub(Socket, '#'),
  mqtt_loop(Socket).


