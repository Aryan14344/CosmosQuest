:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_cors)).  % Add this at the top

% Enable CORS
:- set_setting(http:cors, [*]).

:- http_handler(root(fact), send_fact, [method(get), cors(*)]).

start_server(Port) :-
    http_server(http_dispatch, [port(Port)]).
% Define some facts
fact("Venus spins backward compared to most planets!").
fact("Bananas are berries, but strawberries aren't.").
fact("Octopuses have three hearts.").

send_random_fact(Request) :-
    cors_enable(Request, [methods([get])]),
    findall(F, fact(F), Facts),
    random_member(Fact, Facts),
    reply_json(_{fact: Fact}).




send_fact(_Request) :-
    Fact = "Venus spins backward compared to most planets!",
    reply_json(_{fact: Fact}).
