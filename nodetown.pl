% -*- prolog -*-
%
% This file is part of the Node.Town system.
% It is licensed under the GNU AGPL v3.0.
% See the COPYING file for more information.

:- module(nodetown, []).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_db), [(rdf_meta)/1, rdf_load/2]).
:- use_module(library(semweb/rdf_persistency)).
:- use_module(library(semweb/rdf_library)).
:- use_module(library(semweb/rdf_portray), []).
:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdfa), []).
:- use_module(library(semweb/rdf_ntriples), []).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_zlib_plugin)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(sweet)).

:- op(920, fy, *).
* _.

:- op(921, fy, >-).
>- X --> [X].

:- rdf_meta >-(t, ?, ?).

:- rdf_register_prefix(nt, 'https://node.town/').
:- rdf_register_prefix(id, 'https://id.node.town/').
:- rdf_register_prefix(as, 'http://www.w3.org/ns/activitystreams#').
:- rdf_register_prefix(schema, 'https://schema.org/').
:- rdf_register_prefix(eth, 'http://ethon.consensys.net/').
:- rdf_register_prefix(erc20, 'http://erc20.consensys.net/ERC20/').
:- rdf_register_prefix(vcard, 'http://www.w3.org/2006/vcard/ns#').

info(X) :- print_message(informational, X).
oops(X) :- print_message(error, X).
