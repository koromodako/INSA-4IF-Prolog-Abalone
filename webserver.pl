%% -----------------------------------------------------------------------------
%% Module permettant de présenter une interface web pour le jeu abalone
:- module(webserver, [server/1]).
%% -----------------------------------------------------------------------------

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_client)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).

%%%%%%%%%%%%%%%%%%%
%% Configuration %%
%%%%%%%%%%%%%%%%%%%

% Chargement de la configuration serveur
:- consult(configuration).

% Chemins ouverts dans le serveur web
user:file_search_path(web, projectRoot('web')).
user:file_search_path(css, projectRoot('web/css')).
user:file_search_path(js, projectRoot('web/js')).

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