# Prolog-Abalone

Drive : [ici](https://docs.google.com/document/d/1E-JNow5ES92GtSDYKef_E-OvXDUMxF-eFcAZj14hXoQ/edit?usp=sharing)

## IHM Console

Ouvrir dans prolog `play.pl`, puis lancer une partie avec le prédicat `play:play.`

## IHM Web

`web/index.html` contient le code du plateau-test, qui a comme feuille de style `web/css/main.css`.

### Génération du plateau

La structure du plateau est générée grâce au script `scritp_generate_board.py`, les marbles ont été mises à la main en attendant d'avoir intégré ça dans le script.

Les fichiers `test.xml` et `test2.xml` sont des sorties obtenues avec le script précédemment cité.


### Lancement du serveur Web

- Ouvrir `configuration.pl` et mettre le chemin absolu du projet à la place de celui présent : `user:file_search_path(projectRoot, '/path/to/root/project').`
- Ouvrir dans prolog `webserver.pl` puis entrer `server(Port)`, où `Port` est le numéro du port sur lequel le serveur peut écouter (exemple `8080`).
- Ouvrir un navigateur à l'adresse `http://localhost:8080/game` (changer `8080` par le port précédemment choisi)
- Cliquer sur Transfert ! pour effectuer un test de requête (visible dans web/js/main.js). Cette dernière ne fait que retourner les paramètres qui lui sont envoyés
