#!/bin/python
#-!- encoding:utf8 -!-

import requests, json

#---------------------- CONSTANTS ------------------------#

##
#   URL de base pour le serveur PROLOG
##
BASE_URL = "http://localhost:8080";
##
#   Variable globale contenant le plateau de jeu à tout moment
##
BOARD = "";
##
#   Variable globale contenant le numero du joueur qui joue à tout moment
##
PLAYER = 1;
##
#   Variable globale de stockage des gagnants 
##
WINNER_SEQ = [];
##
#   Variable gloable de stockage du total d'itérations pour chaque partie
##
ITER_COUNT_SEQ = [];
##
#   Variable globale donnant le nombre maximal de coups pour chaque partie
##
MAX_IT = 250;
##
#   Variable globale donnant le nombre de parties à jouer pour réaliser les statisques 
#       (idéalement au moins 100)
##
MAX_GAMES = 2;

#---------------------- FUNCTIONS ------------------------#

def initLocalGameVars():
    global BOARD; # sale mais c'est plus simple
    global PLAYER; # sale mais c'est plus simple
    BOARD = "";
    PLAYER = 1;

##
##  Returns standardized payload headers
##
def getHeaders():
    headers = {'Content-Type':'application/json; charset=UTF-8'};
    return headers;

##
##  Returns standardized payload content
##
def getPayload():
    payload = json.dumps({"Player":PLAYER,"Board":BOARD}, sort_keys=True);
    return payload;

##
## Update BOARD with init value
##
def getInitBoardRequest():
    global BOARD; # sale mais c'est plus simple
    # Get response from post request
    response = requests.post(BASE_URL + "/get/init/board");
    # Update BOARD with the initBoard
    BOARD = response.json();
    # return void
    return;

##
##  Return true if game is over
##
def checkGameIsOverRequest():
    # Get response from post request
    response = requests.post(BASE_URL + "/check/game/is/over", data=getPayload(), headers=getHeaders());
    # return isOver value
    return response.json()['isOver'];

## 
##  Update BOARD with the next move played by the IA
##
def makeIAPlayRequest():
    global BOARD; # sale mais c'est plus simple
    # Get response from post request
    response = requests.post(BASE_URL + "/make/ia/play", data=getPayload(), headers=getHeaders());
    # Update board
    BOARD = response.json();
    # return void
    return;

##
##  Plays next turn of a game
##
def playTurn():
    global PLAYER; # sale mais c'est plus simple
    # If game is over
    if checkGameIsOverRequest():
        # On retourne faux lorsque la partie est finie
        return False; 
    else:
        # On demande à l'IA de jouer
        makeIAPlayRequest();
        # On passe au prochain joueur
        if PLAYER == 1:
            PLAYER = 2;
        else:
            PLAYER = 1;
        # On retourne vrai si la partie n'est pas finie
        return True; 

BOARD_NM1 = ""
BOARD_NM2 = ""
BOARD_NM3 = ""
BOARD_NM4 = ""

## Detects one kind of infinite loop
def detectInfiniteLoop():
    global BOARD_NM1;
    global BOARD_NM2;
    global BOARD_NM3;
    global BOARD_NM4;
    #print(BOARD);
    #print(BOARD_NM4);
    if(BOARD == BOARD_NM4):
        return True; # Infinite loop detected
    else:
        BOARD_NM4 = BOARD_NM3;
        BOARD_NM3 = BOARD_NM2;
        BOARD_NM2 = BOARD_NM1;
        BOARD_NM1 = BOARD;
        return False;

##
##  Plays a complete game
##
def playGame(maxit):
    global WINNER_SEQ; # sale mais c'est plus simple
    global ITER_COUNT_SEQ; # sale mais c'est plus simple
    initLocalGameVars();
    getInitBoardRequest();
    interrupted = False;
    i = 0;
    while(playTurn()):
        if detectInfiniteLoop():
            interrupted = True;
            print("<!> INIFINITE LOOP DETECTED <!>")
            # On signale l'interruption
            WINNER_SEQ.append("INF LOOP");
            ITER_COUNT_SEQ.append(i);
            break;
        elif i < maxit:
            i+=1;
            if (i % 10) == 0:
                print("- Iteration %s, continuing..." % i); 
        else:
            interrupted = True;
            print("<!> MAX ITERATION REACHED <!>")
            # On signale l'interruption
            WINNER_SEQ.append("MAX IT");
            ITER_COUNT_SEQ.append(i);
            break;

    if not interrupted:
        # On ajoute le gagnant à la liste
        WINNER_SEQ.append(PLAYER);
        ITER_COUNT_SEQ.append(i);

##
##  Prints game sequence statistics
##
def printStatistics():
    print("\n>> Statistics:\n")
    for i in range(0, len(WINNER_SEQ)):
        print("- Result: winner is %s after %s iterations." % (WINNER_SEQ[i],ITER_COUNT_SEQ[i]));


#---------------------- SCRIPT ------------------------#

try:
    getInitBoardRequest();
    for i in range(0, MAX_GAMES):
        print(">> Starting game %s" % i);
        playGame(MAX_IT);
        print(">> Game finished.\n");
    printStatistics();
except Exception, e:
    print("<!> Error : You may have forgotten to start PROLOG server at %s" % BASE_URL)
    print("Details:\n%s" % e);