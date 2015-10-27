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

% Chargement de la configuration serveur
:- consult(configuration).

%%%%%%%%%%%%%%%%
%%  Routing   %%
%%%%%%%%%%%%%%%%

:- http_handler('/', helloAction, []).
:- http_handler('/css', http_reply_from_files(css(.), []), [prefix]).
:- http_handler('/js', http_reply_from_files(js(.), []), [prefix]).
:- http_handler('/game', http_reply_from_files(web(.), []), [prefix]).
:- http_handler('/get/init/board', getInitBoardAction, []).

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

initBoard(
[
[1,1,0,0,0,-1,-1,-1,-1],
[1,1,0,0,0,0,-1,-1,-1],
[1,1,1,0,0,0,0,-1,-1],
[1,1,1,0,0,0,0,2,-1],
[1,1,1,0,0,0,2,2,2],
[-1,1,0,0,0,0,2,2,2],
[-1,-1,0,0,0,0,2,2,2],
[-1,-1,-1,0,0,0,0,2,2],
[-1,-1,-1,-1,0,0,0,2,2]
]).

getInitBoardAction(Request) :-
      initBoard(PrologOut),
      prolog_to_json(PrologOut, JSONOut),
      reply_json(JSONOut).