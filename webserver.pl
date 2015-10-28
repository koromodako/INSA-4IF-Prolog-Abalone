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
:- use_module(library(http/http_mime_plugin)).

:- use_module(board).

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
:- http_handler('/get/init/board', getInitBoardAction, [prefix]).
:- http_handler('/get/player/movements', getPlayerMovementsAction, [prefix]).

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

getInitBoardAction(_) :-
      initBoard(Board),
      prolog_to_json(Board, BoardJSON),
      reply_json(BoardJSON).

getPlayerMovementsAction(Request) :-
    member(method(post), Request), !,
    http_read_json(Request, JSONIn),
    json_to_prolog(JSONIn, PrologIn),
    %findall(
    %    [NextLine, NextCol],
    %    (
    %       movable:playerMovements(Board, Player, Line, Col, NextLine, NextCol)
    %    ),
    %    NewMovements
    %),
    prolog_to_json(PrologIn, JSONOut),
    reply_json(JSONOut).