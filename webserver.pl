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
:- use_module(movable).
:- use_module(move).
:- use_module(ia).
:- use_module(gameOver).

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
:- http_handler('/make/player/movement', makePlayerMovementAction, [prefix]).
:- http_handler('/make/ia/play', makeIAPlayAction, [prefix]).
:- http_handler('/check/game/is/over', checkGameIsOver, [prefix]).

%%%%%%%%%%%%%%%%
%%  Actions   %%
%%%%%%%%%%%%%%%%

server(Port) :-
    http_server(http_dispatch, [port(Port)]),
    cors_enable(_, [method([get, post])]).

% say hello, to test if server is running
helloAction(_) :-
    format('Content-type: text/html~n~n'),
    format('Pour jouer, il faut aller  <a href="/game"> ici ! </a>').

getInitBoardAction(_) :-
      initBoard(Board),
      prolog_to_json(Board, BoardJSON),
      reply_json(BoardJSON).

getPlayerMovementsAction(Request) :-
    member(method(post), Request), !,
    http_read_json(Request, JSONIn),
    json_to_prolog(JSONIn, DataStruct),
    DataStruct=json(['Board'=Board,'Player'=Player,'Line'=Line,'Col'=Col]),
    findall(
        [NextLine, NextCol],
        (
           movable:playerMovements(Board, Player, Line, Col, NextLine, NextCol)
        ),
        NewMovements
    ),
    prolog_to_json(NewMovements, JSONOut),
    reply_json(JSONOut).

makePlayerMovementAction(Request) :-
    member(method(post), Request), !,
    http_read_json(Request, JSONIn),
    json_to_prolog(JSONIn, DataStruct),
    DataStruct=json(['Board'=Board,'Line'=Line,'Col'=Col,'NextLine'=NextLine,'NextCol'=NextCol]),
    move:moveMarbles(Board, Col, Line, NextCol, NextLine, NewBoard),
    prolog_to_json(NewBoard, JSONOut),
    reply_json(JSONOut).

makeIAPlayAction(Request) :-
    member(method(post), Request), !,
    http_read_json(Request, JSONIn),
    json_to_prolog(JSONIn, DataStruct),
    DataStruct=json(['Board'=Board,'Player'=Player]),
    ia:playTurn(Board, Player, NewBoard),
    prolog_to_json(NewBoard, JSONOut),
    reply_json(JSONOut).

checkGameIsOver(Request) :-
    member(method(post), Request), !,
    http_read_json(Request, JSONIn),
    json_to_prolog(JSONIn, DataStruct),
    DataStruct=json(['Board'=Board,'Player'=Player]),
    (
       (
           gameOver:gameOver(Player, Board),
           JSONOut=json([ 'isOver' = @true ])
       )
       ;
       (
           JSONOut=json([ 'isOver' = @false ])
       )
    ),
    reply_json(JSONOut).

%%%%%%%%%%%%%%%%%%
%% JSON Objects %%
%%%%%%%%%%%%%%%%%%

%json_object row(a, b, c, d, e, f, g, h, i).
%json_object matrix(ra:row/9,
%                   rb:row/9,
%                   rc:row/9,
%                   rd:row/9,
%                   re:row/9,
%                   rf:row/9,
%                   rg:row/9,
%                   rh:row/9,
%                   ri:row/9).

%json_object data_struct(board:matrix/9, player:int, line:int, col:int).