%%%%%%%%%%%% webserver.pl %%%%%%%%%%%%

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_client)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).   % to read Request parameters
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_json)).         % for json
:- use_module(library(http/json_convert)).

:- consult(configuration).

%%%%%%%%%%%%%%%%
%%  Routing   %%
%%%%%%%%%%%%%%%%

:- http_handler('/', helloAction, []).
:- http_handler('/css', http_reply_from_files(css(.), []), [prefix]).
:- http_handler('/js', http_reply_from_files(js(.), []), [prefix]).
:- http_handler('/game', http_reply_from_files(web(.), []), [prefix]).
:- http_handler('/transfert', transfertAction, []).

%%%%%%%%%%%%%%%%
%%  Actions   %%
%%%%%%%%%%%%%%%%

server(Port) :-
        http_server(http_dispatch, [port(Port)]),
        cors_enable(_, [method([get, post])]).

% say hello, to test if server is running
helloAction(_) :-
    format('Content-type: text/plain~n~n'),
    format('Hello world ! Server is running').

transfertAction(Request) :-
    http_parameters(Request, [ x(X, []),
          y(Y, []), tab(T, [])
        ]),
    reply_json(json{x:X, y:Y, tab:T}).