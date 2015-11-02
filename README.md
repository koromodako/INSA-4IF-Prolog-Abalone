# Abalone (prolog)

## IHM Console

Ouvrir dans prolog `play.pl`, puis lancer une partie avec le prédicat `play:play.`

### Jouer

Il y a trois modes de jeu :
- Humain contre Humain
- Humain contre Ordinateur
- Ordinateur contre Ordinateur

Suivre les instructions dans la console pour jouer.

### Configuration

Par défaut, un nombre maximum d'itération est en place (100) afin de se prémunir des boucles infinies.
Vous pouvez modifier ce paramètre dans le haut du fichier `play.pl` (sous le prédicat play).

## IHM Web

Dans le fichier `web/index.html`, on trouve le code du plateau, qui a comme feuille de style `web/css/main.css`.
Les autres fichiers concernant l'interface web sont dans le dossier `web`.

L'interface propose elle aussi les trois modes de jeu.
Cependant, elle présente un avantage sur la configuration de l'ordinateur qui combat (agréssivité et difficulté).

### Lancement du serveur Web

- Ouvrir `configuration.pl` et mettre le chemin absolu du projet à la place de celui présent : `user:file_search_path(projectRoot, '/path/to/root/project').`
- Ouvrir dans prolog le fichier `webserver.pl` puis entrer le prédicat `server(Port)`, où `Port` est le numéro du port sur lequel le serveur peut écouter (exemple `8080`).
- Ouvrir un navigateur à l'adresse `http://localhost:8080/game` (changer `8080` par le port précédemment choisi)
- Suivez les indications de l'interface pour jouer.

## Outils

Vous trouverez plus d'explications sur les outils (génération du SVG et génération de statistiques) : [Outils](tools/README.md)