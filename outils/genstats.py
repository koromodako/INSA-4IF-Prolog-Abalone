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
MAX_IT = 500;
##
#   Variable globale donnant le nombre de parties à jouer pour réaliser les statisques 
#       (idéalement au moins 100)
##
MAX_GAMES = 1;

##
#   AI depth levels
##
IA_DIFF_EASY = 1;
IA_DIFF_MEDIUM = 2;
IA_DIFF_EXPERT  = 3;

##
#   AI aggressiveness levels
##
IA_AGG_GENTLE = 500;
IA_AGG_DEFAULT = 1000;
IA_AGG_AGGRESSIVE  = 1500;


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
    response = requests.post(BASE_URL + "/check/game/is/over", data=json.dumps({"Player":PLAYER,"Board":BOARD}, sort_keys=True), headers=getHeaders());
    # return isOver value
    return response.json()['isOver'];

## 
##  Update BOARD with the next move played by the IA
##
def makeIAPlayRequest(depth, aggressiveness):
    global BOARD; # sale mais c'est plus simple
    # Get response from post request
    response = requests.post(BASE_URL + "/make/ia/play", data=json.dumps({"Player":PLAYER,"Board":BOARD,"Depth":depth,"Aggressiveness":aggressiveness}, sort_keys=True), headers=getHeaders());
    # Update board
    BOARD = response.json();
    # return void
    return;

##
##  Plays next turn of a game
##
def playTurn(w_ia_depth, w_ia_aggr, b_ia_depth, b_ia_aggr):
    global PLAYER; # sale mais c'est plus simple
    # If game is over
    if checkGameIsOverRequest():
        # On retourne faux lorsque la partie est finie
        return False; 
    else:
        # On passe au prochain joueur
        if PLAYER == 1: # White AI turn
            # On demande à l'IA de jouer
            makeIAPlayRequest(w_ia_depth, w_ia_aggr);
            # On change de player
            PLAYER = 2;
        else: # Black AI turn
            # On demande à l'IA de jouer
            makeIAPlayRequest(b_ia_depth, b_ia_aggr);
            # On change de player
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
def playGame(maxit, w_ia_depth, w_ia_aggr, b_ia_depth, b_ia_aggr):
    global WINNER_SEQ; # sale mais c'est plus simple
    global ITER_COUNT_SEQ; # sale mais c'est plus simple
    initLocalGameVars();
    getInitBoardRequest();
    interrupted = False;
    i = 0;
    while(playTurn(w_ia_depth, w_ia_aggr, b_ia_depth, b_ia_aggr)):
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
        ITER_COUNT_SEQ.append(i);
        if PLAYER == 1: # White AI loose
            WINNER_SEQ.append(2);
        else: # Black AI loose
            WINNER_SEQ.append(1);

##
##  Prints game sequence statistics
##
def printStatistics():
    print("\n>> Statistics:\n")
    for i in range(0, len(WINNER_SEQ)):
        print("- Result: winner is %s after %s iterations." % (WINNER_SEQ[i],ITER_COUNT_SEQ[i]));
    # Reset 



def playGameSequence(w_ia_depth, w_ia_aggr, b_ia_depth, b_ia_aggr):
    global WINNER_SEQ;
    global ITER_COUNT_SEQ;
    # Reset statistiques
    WINNER_SEQ = [];
    ITER_COUNT_SEQ = [];
    # Get init board
    getInitBoardRequest();
    for i in range(0, MAX_GAMES):
        print(">> Starting game %s" % i);
        playGame(MAX_IT, w_ia_depth, w_ia_aggr, b_ia_depth, b_ia_aggr);
        print(">> Game finished.\n");
    printStatistics();

#---------------------- SCRIPT ------------------------#

try:
    # IA 1 vs IA 1
    print("\n -------------------------- IA 1 vs. IA 1 -------------------------- ")
    print("\n ************** IA 1 default vs IA 1 gentle **************\n");
    playGameSequence(IA_DIFF_EASY, IA_AGG_DEFAULT, IA_DIFF_EASY, IA_AGG_GENTLE); #
    print("\n ************** IA 1 aggressive vs IA 1 gentle **************\n");
    playGameSequence(IA_DIFF_EASY, IA_AGG_AGGRESSIVE, IA_DIFF_EASY, IA_AGG_GENTLE); #
    print("\n ************** IA 1 aggressive vs IA 1 default **************\n");
    playGameSequence(IA_DIFF_EASY, IA_AGG_AGGRESSIVE, IA_DIFF_EASY, IA_AGG_DEFAULT); #

    # IA 2 vs IA 2
    print("\n -------------------------- IA 2 vs. IA 2 -------------------------- ")
    print("\n ************** IA 2 default vs IA 2 gentle **************\n");
    playGameSequence(IA_DIFF_MEDIUM, IA_AGG_DEFAULT, IA_DIFF_MEDIUM, IA_AGG_GENTLE); #
    print("\n ************** IA 2 aggressive vs IA 2 gentle **************\n");
    playGameSequence(IA_DIFF_MEDIUM, IA_AGG_AGGRESSIVE, IA_DIFF_MEDIUM, IA_AGG_GENTLE); #
    print("\n ************** IA 2 aggressive vs IA 2 default **************\n");
    playGameSequence(IA_DIFF_MEDIUM, IA_AGG_AGGRESSIVE, IA_DIFF_MEDIUM, IA_AGG_DEFAULT); #

    # IA 3 vs IA 3
    print("\n -------------------------- IA 3 vs. IA 3 -------------------------- ")
    print("\n ************** IA 3 default vs IA 3 gentle **************\n");
    playGameSequence(IA_DIFF_EXPERT, IA_AGG_DEFAULT, IA_DIFF_EXPERT, IA_AGG_GENTLE); #
    print("\n ************** IA 3 aggressive vs IA 3 gentle **************\n");
    playGameSequence(IA_DIFF_EXPERT, IA_AGG_AGGRESSIVE, IA_DIFF_EXPERT, IA_AGG_GENTLE); #
    print("\n ************** IA 3 aggressive vs IA 3 default **************\n");
    playGameSequence(IA_DIFF_EXPERT, IA_AGG_AGGRESSIVE, IA_DIFF_EXPERT, IA_AGG_DEFAULT); #

    # IA 1 vs IA 2
    print("\n -------------------------- IA 1 vs. IA 2 -------------------------- ")
    print("\n ************** IA 1 vs IA 2, both gentle **************\n");
    playGameSequence(IA_DIFF_EASY, IA_AGG_GENTLE, IA_DIFF_MEDIUM, IA_AGG_GENTLE); #
    print("\n ************** IA 1 vs IA 2, both default **************\n");
    playGameSequence(IA_DIFF_EASY, IA_AGG_DEFAULT, IA_DIFF_MEDIUM, IA_AGG_DEFAULT); #
    print("\n ************** IA 1 vs IA 2, both aggressive **************\n");
    playGameSequence(IA_DIFF_EASY, IA_AGG_AGGRESSIVE, IA_DIFF_MEDIUM, IA_AGG_AGGRESSIVE); #
    print("\n ************** IA 1 default vs IA 2 gentle **************\n");
    playGameSequence(IA_DIFF_EASY, IA_AGG_DEFAULT, IA_DIFF_MEDIUM, IA_AGG_GENTLE); #
    print("\n ************** IA 1 aggressive vs IA 2 gentle **************\n");
    playGameSequence(IA_DIFF_EASY, IA_AGG_AGGRESSIVE, IA_DIFF_MEDIUM, IA_AGG_GENTLE); #
    print("\n ************** IA 1 gentle vs IA 2 default **************\n");
    playGameSequence(IA_DIFF_EASY, IA_AGG_GENTLE, IA_DIFF_MEDIUM, IA_AGG_DEFAULT); #
    print("\n ************** IA 1 gentle vs IA 2 aggressive **************\n");
    playGameSequence(IA_DIFF_EASY, IA_AGG_GENTLE, IA_DIFF_MEDIUM, IA_AGG_GENTLE); #
    print("\n ************** IA 1 aggressive vs IA 2 default **************\n");
    playGameSequence(IA_DIFF_EASY, IA_AGG_AGGRESSIVE, IA_DIFF_MEDIUM, IA_AGG_DEFAULT); #
    print("\n ************** IA 1 default vs IA 2 aggressive **************\n");
    playGameSequence(IA_DIFF_EASY, IA_AGG_DEFAULT, IA_DIFF_MEDIUM, IA_AGG_AGGRESSIVE); #

    # IA 1 vs IA 3
    print("\n -------------------------- IA 1 vs. IA 3 -------------------------- ")
    print("\n ************** IA 1 vs IA 3, both gentle **************\n");
    playGameSequence(IA_DIFF_EASY, IA_AGG_GENTLE, IA_DIFF_EXPERT, IA_AGG_GENTLE); #
    print("\n ************** IA 1 vs IA 3, both default **************\n");
    playGameSequence(IA_DIFF_EASY, IA_AGG_DEFAULT, IA_DIFF_EXPERT, IA_AGG_DEFAULT); #
    print("\n ************** IA 1 vs IA 3, both aggressive **************\n");
    playGameSequence(IA_DIFF_EASY, IA_AGG_AGGRESSIVE, IA_DIFF_EXPERT, IA_AGG_AGGRESSIVE); #
    print("\n ************** IA 1 default vs IA 3 gentle **************\n");
    playGameSequence(IA_DIFF_EASY, IA_AGG_DEFAULT, IA_DIFF_EXPERT, IA_AGG_GENTLE); #
    print("\n ************** IA 1 aggressive vs IA 3 gentle **************\n");
    playGameSequence(IA_DIFF_EASY, IA_AGG_AGGRESSIVE, IA_DIFF_EXPERT, IA_AGG_GENTLE); #
    print("\n ************** IA 1 gentle vs IA 3 default **************\n");
    playGameSequence(IA_DIFF_EASY, IA_AGG_GENTLE, IA_DIFF_EXPERT, IA_AGG_DEFAULT); #
    print("\n ************** IA 1 gentle vs IA 3 aggressive **************\n");
    playGameSequence(IA_DIFF_EASY, IA_AGG_GENTLE, IA_DIFF_EXPERT, IA_AGG_GENTLE); #
    print("\n ************** IA 1 aggressive vs IA 3 default **************\n");
    playGameSequence(IA_DIFF_EASY, IA_AGG_AGGRESSIVE, IA_DIFF_EXPERT, IA_AGG_DEFAULT); #
    print("\n ************** IA 1 default vs IA 3 aggressive **************\n");
    playGameSequence(IA_DIFF_EASY, IA_AGG_DEFAULT, IA_DIFF_EXPERT, IA_AGG_AGGRESSIVE); #

    # IA 2 vs IA 3
    print("\n -------------------------- IA 2 vs. IA 3 -------------------------- ")
    print("\n ************** IA 2 vs IA 3, both gentle **************\n");
    playGameSequence(IA_DIFF_MEDIUM, IA_AGG_GENTLE, IA_DIFF_EXPERT, IA_AGG_GENTLE); #
    print("\n ************** IA 2 vs IA 3, both default **************\n");
    playGameSequence(IA_DIFF_MEDIUM, IA_AGG_DEFAULT, IA_DIFF_EXPERT, IA_AGG_DEFAULT); #
    print("\n ************** IA 2 vs IA 3, both aggressive **************\n");
    playGameSequence(IA_DIFF_MEDIUM, IA_AGG_AGGRESSIVE, IA_DIFF_EXPERT, IA_AGG_AGGRESSIVE); #
    print("\n ************** IA 2 default vs IA 3 gentle **************\n");
    playGameSequence(IA_DIFF_MEDIUM, IA_AGG_DEFAULT, IA_DIFF_EXPERT, IA_AGG_GENTLE); #
    print("\n ************** IA 2 aggressive vs IA 3 gentle **************\n");
    playGameSequence(IA_DIFF_MEDIUM, IA_AGG_AGGRESSIVE, IA_DIFF_EXPERT, IA_AGG_GENTLE); #
    print("\n ************** IA 2 gentle vs IA 3 default **************\n");
    playGameSequence(IA_DIFF_MEDIUM, IA_AGG_GENTLE, IA_DIFF_EXPERT, IA_AGG_DEFAULT); #
    print("\n ************** IA 2 gentle vs IA 3 aggressive **************\n");
    playGameSequence(IA_DIFF_MEDIUM, IA_AGG_GENTLE, IA_DIFF_EXPERT, IA_AGG_GENTLE); #
    print("\n ************** IA 2 aggressive vs IA 3 default **************\n");
    playGameSequence(IA_DIFF_MEDIUM, IA_AGG_AGGRESSIVE, IA_DIFF_EXPERT, IA_AGG_DEFAULT); #
    print("\n ************** IA 2 default vs IA 3 aggressive **************\n");
    playGameSequence(IA_DIFF_MEDIUM, IA_AGG_DEFAULT, IA_DIFF_EXPERT, IA_AGG_AGGRESSIVE); #

    # IA 2 vs IA 1
    print("\n -------------------------- IA 2 vs. IA 1 -------------------------- ")
    print("\n ************** IA 2 vs IA 1, both gentle **************\n");
    playGameSequence(IA_DIFF_MEDIUM, IA_AGG_GENTLE, IA_DIFF_EASY, IA_AGG_GENTLE); #
    print("\n ************** IA 2 vs IA 1, both default **************\n");
    playGameSequence(IA_DIFF_MEDIUM, IA_AGG_DEFAULT, IA_DIFF_EASY, IA_AGG_DEFAULT); #
    print("\n ************** IA 2 vs IA 1, both aggressive **************\n");
    playGameSequence(IA_DIFF_MEDIUM, IA_AGG_AGGRESSIVE, IA_DIFF_EASY, IA_AGG_AGGRESSIVE); #
    print("\n ************** IA 2 default vs IA 1 gentle **************\n");
    playGameSequence(IA_DIFF_MEDIUM, IA_AGG_DEFAULT, IA_DIFF_EASY, IA_AGG_GENTLE); #
    print("\n ************** IA 2 aggressive vs IA 1 gentle **************\n");
    playGameSequence(IA_DIFF_MEDIUM, IA_AGG_AGGRESSIVE, IA_DIFF_EASY, IA_AGG_GENTLE); #
    print("\n ************** IA 2 gentle vs IA 1 default **************\n");
    playGameSequence(IA_DIFF_MEDIUM, IA_AGG_GENTLE, IA_DIFF_EASY, IA_AGG_DEFAULT); #
    print("\n ************** IA 2 gentle vs IA 1 aggressive **************\n");
    playGameSequence(IA_DIFF_MEDIUM, IA_AGG_GENTLE, IA_DIFF_EASY, IA_AGG_GENTLE); #
    print("\n ************** IA 2 aggressive vs IA 1 default **************\n");
    playGameSequence(IA_DIFF_MEDIUM, IA_AGG_AGGRESSIVE, IA_DIFF_EASY, IA_AGG_DEFAULT); #
    print("\n ************** IA 2 default vs IA 1 aggressive **************\n");
    playGameSequence(IA_DIFF_MEDIUM, IA_AGG_DEFAULT, IA_DIFF_EASY, IA_AGG_AGGRESSIVE); #

    # IA 3 vs IA 1
    print("\n -------------------------- IA 3 vs. IA 1 -------------------------- ")
    print("\n ************** IA 3 vs IA 1, both gentle **************\n");
    playGameSequence(IA_DIFF_EXPERT, IA_AGG_GENTLE, IA_DIFF_EASY, IA_AGG_GENTLE); #
    print("\n ************** IA 3 vs IA 1, both default **************\n");
    playGameSequence(IA_DIFF_EXPERT, IA_AGG_DEFAULT, IA_DIFF_EASY, IA_AGG_DEFAULT); #
    print("\n ************** IA 3 vs IA 1, both aggressive **************\n");
    playGameSequence(IA_DIFF_EXPERT, IA_AGG_AGGRESSIVE, IA_DIFF_EASY, IA_AGG_AGGRESSIVE); #
    print("\n ************** IA 3 default vs IA 1 gentle **************\n");
    playGameSequence(IA_DIFF_EXPERT, IA_AGG_DEFAULT, IA_DIFF_EASY, IA_AGG_GENTLE); #
    print("\n ************** IA 3 aggressive vs IA 1 gentle **************\n");
    playGameSequence(IA_DIFF_EXPERT, IA_AGG_AGGRESSIVE, IA_DIFF_EASY, IA_AGG_GENTLE); #
    print("\n ************** IA 3 gentle vs IA 1 default **************\n");
    playGameSequence(IA_DIFF_EXPERT, IA_AGG_GENTLE, IA_DIFF_EASY, IA_AGG_DEFAULT); #
    print("\n ************** IA 3 gentle vs IA 1 aggressive **************\n");
    playGameSequence(IA_DIFF_EXPERT, IA_AGG_GENTLE, IA_DIFF_EASY, IA_AGG_GENTLE); #
    print("\n ************** IA 3 aggressive vs IA 1 default **************\n");
    playGameSequence(IA_DIFF_EXPERT, IA_AGG_AGGRESSIVE, IA_DIFF_EASY, IA_AGG_DEFAULT); #
    print("\n ************** IA 3 default vs IA 1 aggressive **************\n");
    playGameSequence(IA_DIFF_EXPERT, IA_AGG_DEFAULT, IA_DIFF_EASY, IA_AGG_AGGRESSIVE); #

    # IA 3 vs IA 2
    print("\n -------------------------- IA 3 vs. IA 2 -------------------------- ")
    print("\n ************** IA 3 vs IA 2, both gentle **************\n");
    playGameSequence(IA_DIFF_EXPERT, IA_AGG_GENTLE, IA_DIFF_MEDIUM, IA_AGG_GENTLE); #
    print("\n ************** IA 3 vs IA 2, both default **************\n");
    playGameSequence(IA_DIFF_EXPERT, IA_AGG_DEFAULT, IA_DIFF_MEDIUM, IA_AGG_DEFAULT); #
    print("\n ************** IA 3 vs IA 2, both aggressive **************\n");
    playGameSequence(IA_DIFF_EXPERT, IA_AGG_AGGRESSIVE, IA_DIFF_MEDIUM, IA_AGG_AGGRESSIVE); #
    print("\n ************** IA 3 default vs IA 2 gentle **************\n");
    playGameSequence(IA_DIFF_EXPERT, IA_AGG_DEFAULT, IA_DIFF_MEDIUM, IA_AGG_GENTLE); #
    print("\n ************** IA 3 aggressive vs IA 2 gentle **************\n");
    playGameSequence(IA_DIFF_EXPERT, IA_AGG_AGGRESSIVE, IA_DIFF_MEDIUM, IA_AGG_GENTLE); #
    print("\n ************** IA 3 gentle vs IA 2 default **************\n");
    playGameSequence(IA_DIFF_EXPERT, IA_AGG_GENTLE, IA_DIFF_MEDIUM, IA_AGG_DEFAULT); #
    print("\n ************** IA 3 gentle vs IA 2 aggressive **************\n");
    playGameSequence(IA_DIFF_EXPERT, IA_AGG_GENTLE, IA_DIFF_MEDIUM, IA_AGG_GENTLE); #
    print("\n ************** IA 3 aggressive vs IA 2 default **************\n");
    playGameSequence(IA_DIFF_EXPERT, IA_AGG_AGGRESSIVE, IA_DIFF_MEDIUM, IA_AGG_DEFAULT); #
    print("\n ************** IA 3 default vs IA 2 aggressive **************\n");
    playGameSequence(IA_DIFF_EXPERT, IA_AGG_DEFAULT, IA_DIFF_MEDIUM, IA_AGG_AGGRESSIVE); #

except Exception, e:
    print("<!> Error : You may have forgotten to start PROLOG server at %s" % BASE_URL)
    print("Details:\n%s" % e);
