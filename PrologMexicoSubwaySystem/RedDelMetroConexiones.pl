conecta(X, Y, L):- sigue(X, Y, L); sigue(Y, X, L).


cerca(X, Y):- conecta(X, Y,_).
cerca(X, Y):- conecta(X, Z,_),conecta(Z, Y,_).
conecta(chabacano, obrera, linea_9):- sigue(chabacano, obrera, linea_9); sigue(obrera, chabacano, linea_9).