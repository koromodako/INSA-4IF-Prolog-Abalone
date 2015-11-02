#!/bin/python


# ----------------------------------- IMPORTS -------------------------------

import sys, math

# ----------------------------------- FONCTIONS -------------------------------

def get6uplet(X,Y, HexSideSize):
#   Calcul des constantes
    HEX_SIDE_PV=math.cos(math.pi/3)*HexSideSize;
    HEX_SIDE_PH=math.cos(math.pi/6)*HexSideSize;
    #print("Hexagone params: side=%.3f gives PV=%.3f & PH=%.3f" % (HexSideSize, HEX_SIDE_PV, HEX_SIDE_PH)); # DEBUG ONLY
#   Debut du calcul
    points=[];
    points.append([X,Y]);
    points.append([X+HEX_SIDE_PH,Y-HEX_SIDE_PV]);
    points.append([X+HEX_SIDE_PH,Y-HEX_SIDE_PV-HexSideSize]);
    points.append([X,Y-2*HEX_SIDE_PV-HexSideSize]);
    points.append([X-HEX_SIDE_PH,Y-HEX_SIDE_PV]);
    points.append([X-HEX_SIDE_PH,Y-HEX_SIDE_PV-HexSideSize]);
#   Affichage du resultat
    print("\t\t<g class=\"tile\">\n\t\t\t<polygon points=\"%.3f,%.3f %.3f,%.3f %.3f,%.3f %.3f,%.3f %.3f,%.3f %.3f,%.3f\"/>\n\t\t\t<circle class=\"emptyTile\" cx=\"%.3f\" cy=\"%.3f\" r=\"%.3f\"/>\n\t\t</g>" 
            % (
               points[4][0],points[4][1],
               points[5][0],points[5][1],
               points[3][0],points[3][1],
               points[2][0],points[2][1],
               points[1][0],points[1][1],
               points[0][0],points[0][1],
               points[3][0],points[3][1]+HexSideSize,
               HexSideSize/2
               )
          );
               
def getAbaloneSVG(XOffset, YOffset, SquareMatrixDimension, HexSideSize):
    MIDDLE=int(SquareMatrixDimension/2);
    HEX_SIDE_PV=math.cos(math.pi/3)*HexSideSize;
    HEX_SIDE_PH=math.cos(math.pi/6)*HexSideSize;
    # Double boucle de generation du vecteur unitaire de multiplication
    print("<svg id=\"grid-offset-even-r\" width=\"480\" height=\"360\">\n\t<g>");
    for i in range(-MIDDLE,MIDDLE+1):
        for j in range(0, SquareMatrixDimension-abs(i)):
            get6uplet(XOffset + (abs(i)+2*j)*HEX_SIDE_PH,
                      YOffset + i*(HexSideSize+HEX_SIDE_PV),
                      HexSideSize
                      );
    print("\t</g>\n</svg>");

        

# --------------------------------- SCRIPT ------------------------------------

if(len(sys.argv) < 5):
    print("Usage : python script_generate_board.py XOffset YOffset SquareMatrixDimension HexSideSize");
    exit();

getAbaloneSVG(float(sys.argv[1]), float(sys.argv[2]), int(sys.argv[3]), float(sys.argv[4]));