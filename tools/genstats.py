#!/bin/python
#-!- encoding:utf8 -!-

import requests, json

#---------------------- CONFIGURATION ------------------------#

##
#   URL de base pour le serveur PROLOG
##
BASE_URL = "http://localhost:8080";
##
#   Activation/Desactivation de la detection des boucles infinie
##
DISABLE_INFINITE_LOOP_DETECTION = True;
##
#   Variable globale donnant le nombre maximal de coups pour chaque partie
##
MAX_IT = 500;
##
#   Variable globale donnant le nombre de parties à jouer pour réaliser les statisques 
#       (idéalement au moins 100)
##
MAX_GAMES = 1;

#---------------------- CONSTANTES ------------------------#

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
#   Profondeur de recherche dans l'arbre pour l'IA
##
IA_DIFF_EASY = 1;
IA_DIFF_MEDIUM = 2;
IA_DIFF_EXPERT  = 3;

##
#   Niveau d'aggressivité de l'IA
##
IA_AGG_GENTLE = 500;
IA_AGG_DEFAULT = 1000;
IA_AGG_AGGRESSIVE  = 1500;


#---------------------- FONCTIONS ------------------------#

def initLocalGameVars():
    global BOARD; 
    global PLAYER; 
    BOARD = "";
    PLAYER = 1;

##
##  Retourne le contenu du header standard pour la requête HTTP
##
def getHeaders():
    headers = {'Content-Type':'application/json; charset=UTF-8'};
    return headers;

##
## Réinitialise le plateau de jeu global avec le board de démarrage
##
def getInitBoardRequest():
    global BOARD; 
    # Recupere la réponse à la requete
    response = requests.post(BASE_URL + "/get/init/board");
    # Met à jour le plateau de jeu
    BOARD = response.json();
    # Retourne void
    return;

##
##  Retourne vrai si le jeu est terminé, faux dans le cas contraire
##
def checkGameIsOverRequest():
    # Recupere la reponse à la requete
    response = requests.post(BASE_URL + "/check/game/is/over", data=json.dumps({"Player":PLAYER,"Board":BOARD}, sort_keys=True), headers=getHeaders());
    # Retourne la valeur de isOver
    return response.json()['isOver'];

## 
##  Met à jour le plateau de jeu global ave le dernier mouvement de l'IA pris en compte
##
def makeIAPlayRequest(depth, aggressiveness):
    global BOARD; 
    # Recupere la reponse à la requete
    response = requests.post(BASE_URL + "/make/ia/play", data=json.dumps({"Player":PLAYER,"Board":BOARD,"Depth":depth,"Aggressiveness":aggressiveness}, sort_keys=True), headers=getHeaders());
    # Met à jour le plateau de jeu global
    BOARD = response.json();
    # Retourne void
    return;

##
##  Joue le prochain tour de jeu avec les valeurs données pour la profondeur et l'aggressivite de chaque IA
##
def playTurn(w_ia_depth, w_ia_aggr, b_ia_depth, b_ia_aggr):
    global PLAYER; 
    # Si le jeu est terminé, ...
    if checkGameIsOverRequest():
        # On retourne faux lorsque la partie est finie
        return False; 
    else: # Sinon ...
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

## Detection des boucles infinies de 5 coups
def detectInfiniteLoop():
    global BOARD_NM1;
    global BOARD_NM2;
    global BOARD_NM3;
    global BOARD_NM4;
    
    if DISABLE_INFINITE_LOOP_DETECTION: # CONDITION de forçage de la non verification d'apparition de boucle infinie
        return False;

    if(BOARD == BOARD_NM4):
        return True; # Infinite loop detected
    else:
        BOARD_NM4 = BOARD_NM3;
        BOARD_NM3 = BOARD_NM2;
        BOARD_NM2 = BOARD_NM1;
        BOARD_NM1 = BOARD;
        return False;

##
##  Joue une partie complete entre deux IAs avec les parametres donnés
##
def playGame(maxit, w_ia_depth, w_ia_aggr, b_ia_depth, b_ia_aggr):
    global WINNER_SEQ; 
    global ITER_COUNT_SEQ; 
    initLocalGameVars();
    getInitBoardRequest();
    interrupted = False;
    i = 0;
    while(playTurn(w_ia_depth, w_ia_aggr, b_ia_depth, b_ia_aggr)):
        if detectInfiniteLoop():
            interrupted = True;
            #print("<!> INIFINITE LOOP DETECTED <!>")
            # On signale l'interruption
            WINNER_SEQ.append("INF LOOP");
            ITER_COUNT_SEQ.append(i);
            break;
        elif i < maxit:
            i+=1;
            #if (i % 10) == 0:
                #print("- Iteration %s, continuing..." % i);
        else:
            interrupted = True;
            #print("<!> MAX ITERATION REACHED <!>")
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
##  Affiche les statistiques de la partie venant d'etre jouée
##
def printStatistics():
    #print("\n>> Statistics:\n")
    for i in range(0, len(WINNER_SEQ)):
        print("- Result: winner is %s after %s iterations." % (WINNER_SEQ[i],ITER_COUNT_SEQ[i]));
    # Reset 


##
##  Joue une sequence de parties et affiche les statistiques de la séquence de parties venant d'etre jouées
##
def playGameSequence(w_ia_depth, w_ia_aggr, b_ia_depth, b_ia_aggr):
    global WINNER_SEQ;
    global ITER_COUNT_SEQ;
    # Reset statistiques
    WINNER_SEQ = [];
    ITER_COUNT_SEQ = [];
    # Recuperation board de depart
    getInitBoardRequest();
    for i in range(0, MAX_GAMES):
        #print(">> Starting game %s" % i);
        playGame(MAX_IT, w_ia_depth, w_ia_aggr, b_ia_depth, b_ia_aggr);
        #print(">> Game finished.\n");
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
    print("\n ************** IA 1 gentle vs IA 1 default **************\n");
    playGameSequence(IA_DIFF_EASY, IA_AGG_GENTLE, IA_DIFF_EASY, IA_AGG_DEFAULT); #
    print("\n ************** IA 1 gentle vs IA 1 aggressive **************\n");
    playGameSequence(IA_DIFF_EASY, IA_AGG_GENTLE, IA_DIFF_EASY, IA_AGG_AGGRESSIVE); #
    print("\n ************** IA 1 default vs IA 1 aggressive **************\n");
    playGameSequence(IA_DIFF_EASY, IA_AGG_DEFAULT, IA_DIFF_EASY, IA_AGG_AGGRESSIVE); #

    # IA 2 vs IA 2
    print("\n -------------------------- IA 2 vs. IA 2 -------------------------- ")
    print("\n ************** IA 2 default vs IA 2 gentle **************\n");
    playGameSequence(IA_DIFF_MEDIUM, IA_AGG_DEFAULT, IA_DIFF_MEDIUM, IA_AGG_GENTLE); #
    print("\n ************** IA 2 aggressive vs IA 2 gentle **************\n");
    playGameSequence(IA_DIFF_MEDIUM, IA_AGG_AGGRESSIVE, IA_DIFF_MEDIUM, IA_AGG_GENTLE); #
    print("\n ************** IA 2 aggressive vs IA 2 default **************\n");
    playGameSequence(IA_DIFF_MEDIUM, IA_AGG_AGGRESSIVE, IA_DIFF_MEDIUM, IA_AGG_DEFAULT); #
    print("\n ************** IA 2 gentle vs IA 2 default **************\n");
    playGameSequence(IA_DIFF_MEDIUM, IA_AGG_GENTLE, IA_DIFF_MEDIUM, IA_AGG_DEFAULT); #
    print("\n ************** IA 2 gentle vs IA 2 aggressive **************\n");
    playGameSequence(IA_DIFF_MEDIUM, IA_AGG_GENTLE, IA_DIFF_MEDIUM, IA_AGG_AGGRESSIVE); #
    print("\n ************** IA 2 default vs IA 2 aggressive **************\n");
    playGameSequence(IA_DIFF_MEDIUM, IA_AGG_DEFAULT, IA_DIFF_MEDIUM, IA_AGG_AGGRESSIVE); #

    # IA 3 vs IA 3
    print("\n -------------------------- IA 3 vs. IA 3 -------------------------- ")
    print("\n ************** IA 3 default vs IA 3 gentle **************\n");
    playGameSequence(IA_DIFF_EXPERT, IA_AGG_DEFAULT, IA_DIFF_EXPERT, IA_AGG_GENTLE); #
    print("\n ************** IA 3 aggressive vs IA 3 gentle **************\n");
    playGameSequence(IA_DIFF_EXPERT, IA_AGG_AGGRESSIVE, IA_DIFF_EXPERT, IA_AGG_GENTLE); #
    print("\n ************** IA 3 aggressive vs IA 3 default **************\n");
    playGameSequence(IA_DIFF_EXPERT, IA_AGG_AGGRESSIVE, IA_DIFF_EXPERT, IA_AGG_DEFAULT); #
    print("\n ************** IA 3 gentle vs IA 3 default **************\n");
    playGameSequence(IA_DIFF_EXPERT, IA_AGG_GENTLE, IA_DIFF_EXPERT, IA_AGG_DEFAULT); #
    print("\n ************** IA 3 gentle vs IA 3 aggressive **************\n");
    playGameSequence(IA_DIFF_EXPERT, IA_AGG_GENTLE, IA_DIFF_EXPERT, IA_AGG_AGGRESSIVE); #
    print("\n ************** IA 3 default vs IA 3 aggressive **************\n");
    playGameSequence(IA_DIFF_EXPERT, IA_AGG_DEFAULT, IA_DIFF_EXPERT, IA_AGG_AGGRESSIVE); #

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
