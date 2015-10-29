$(function() {

    $( document ).ready(function(){
        if(DEBUG){
            $('#debug').show();
        }
    });

    /*****************************************
     *** Globals                           ***
     *****************************************/

    /**** Constantes ****/
    /** States **/
    const STOP = 0;
    const READY = 1;
    const CHOOSE_MARBLE = 2;
    const MARBLE_SELECTED = 3;
    const IA_PLAY = 4;

    /** Player **/
    const HUMAN = 0;
    const COMPUTER = 1;

    /**** Globals variables ****/
    var DEBUG = 1;
    var currentSelectedTile = null;
    var board = null;
    var playerTurn = 1;
    var playerType = {1:null, 2:null};
    var currentState = STOP;
    var IAPlayTimeoutID = null;

    /**
     * URL is the same so nothing needed here
     * Else it's possible to reconstruct the baseUrl with this :
     * var baseURL = location.protocol+'//'+location.hostname+(location.port ? ':'+location.port: '');
     */
    var baseUrl = '';

    /*****************************************
     *** Actions                           ***
     *****************************************/

    $('#start-game').click(startGame);

    $('#stop-game').click(stopGame);

    $('g.tile').click(function() {

        // Action to corresponding state
        if (currentState == CHOOSE_MARBLE) {

            // Check if the marble have the right color
            if ((playerTurn == 1 && $(this).find('circle.marble.whiteMarble').length == 0)
                || (playerTurn == 2 && $(this).find('circle.marble.blackMarble').length == 0)) {
                // TODO Notification to explain why you can't play with this marble
                return;
            }

            // Add style to selected marble
            $(this).addClass('selected');
            currentSelectedTile = $(this);

            currentState = MARBLE_SELECTED;

            // Display possibilities
            getPlayerMovementsRequest(function(json) {

                $.each(json, function(index, item) {

                    var col = item[1];
                    var line = item[0];

                    if (line > 5) { // Matrix conversion
                        col -= line - 5;
                    }
                    $('g.tile.col-' + col + '.line-' + line).addClass('movable');
                });
            });

        } else if (currentState == MARBLE_SELECTED) {

            // Not a movement available
            if (!$(this).hasClass('movable')) {

                $('g.tile.movable').removeClass('movable');
                $('g.tile.selected').removeClass('selected');
                currentState = CHOOSE_MARBLE;

            } else { // Make movement

                $('g.tile.movable').removeClass('movable');
                $('g.tile.selected').removeClass('selected');

                makePlayerMovementRequest($(this), function(json) {
                    board = json;
                    updateBoard(board);
                    nextPlayer();
                    playTurn();
                });

            }

        }

        updateDebugInfo($(this));
    });

    /*****************************************
     *** Requests                          ***
     *****************************************/

    function getInitBoardRequest($success) {

        $.ajax({
            method: 'POST',
            url: baseUrl + '/get/init/board',
            success: $success,
            error: function (resultat, statut, erreur) {
                alert("Erreur lors de l'appel de l'initialisation du plateau.");
                console.log(resultat, statut, erreur);
            }
        });

    }

    function getPlayerMovementsRequest($success) {

        var position = getPositionOfTile(currentSelectedTile);

        $.ajax({
            method: 'POST',
            url: baseUrl + '/get/player/movements',
            dataType: 'json',
            data: JSON.stringify({
                Board: board,
                Player: playerTurn,
                Line: position.line,
                Col: position.col
            }),
            contentType: "application/json",
            success: $success,
            error: function (resultat, statut, erreur) {
                alert("Erreur lors de l'appel pour récupérer les mouvements possibles.");
                console.log(resultat, statut, erreur);
            }
        });
    }

    function makePlayerMovementRequest($moveToTile, $success) {

        var position = getPositionOfTile(currentSelectedTile);
        var destination = getPositionOfTile($moveToTile);

        $.ajax({
            method: 'POST',
            url: baseUrl + '/make/player/movement',
            dataType: 'json',
            data: JSON.stringify({
                Board: board,
                Line: position.line,
                Col: position.col,
                NextLine: destination.line,
                NextCol: destination.col
            }),
            contentType: "application/json",
            success: $success,
            error: function (resultat, statut, erreur) {
                alert("Erreur lors de l'appel pour effectuer le movement.");
                console.log(resultat, statut, erreur);
            }
        });
    }

    function makeIAPlayRequest($success) {

        $.ajax({
            method: 'POST',
            url: baseUrl + '/make/ia/play',
            dataType: 'json',
            data: JSON.stringify({
                Board: board,
                Player: playerTurn
            }),
            contentType: "application/json",
            success: $success,
            error: function (resultat, statut, erreur) {
                alert("Erreur lors de l'appel pour faire jouer l'IA.");
                console.log(resultat, statut, erreur);
            }
        });
    }

    function checkGameIsOverRequest($success) {

        $.ajax({
            method: 'POST',
            url: baseUrl + '/check/game/is/over',
            dataType: 'json',
            data: JSON.stringify({
                Board: board,
                Player: playerTurn
            }),
            contentType: "application/json",
            success: $success,
            error: function (resultat, statut, erreur) {
                alert("Erreur lors de l'appel pour vérifier si le jeu est terminé.");
                console.log(resultat, statut, erreur);
            }
        });
    }

    /*****************************************
     *** functions                    ***
     *****************************************/

    function getClassArrayOfElement($elt) {
        return $elt.attr('class').split(/\s+/);
    }

    function getPositionOfTile($tile) {
        var col = null, line = null;

        $.each(getClassArrayOfElement($tile), function(index, item) {
            if (item.match("^col-")) {
                col = parseInt(item.split('-')[1]);
            } else if (item.match("^line-")) {
                line = parseInt(item.split('-')[1]);
            }
        });

        if (line > 5) { // Matrix conversion
            col += line - 5;
        }

        return {
            col: col,
            line: line
        };
    }

    function updateBoard($board) {

        var line;
        for (line = 0; line < $board.length; ++line) {
            var col;
            var realCol;
            for (col = 0, realCol = 1; col < $board[line].length; ++col) {

                var color = $board[line][col];

                if (color < 0) {
                    continue;
                }

                var tile = $('g.tile.col-' + realCol + '.line-' + (line+1)).first();

                if (color == 1 && tile.find('circle.marble.whiteMarble').length == 0) {

                    tile.find('circle.marble').first().removeClass('blackMarble').addClass('whiteMarble').attr('fill', 'url(#whiteMarble)');

                } else if (color == 2 && tile.find('circle.marble.blackMarble').length == 0) {

                    tile.find('circle.marble').first().removeClass('whiteMarble').addClass('blackMarble').attr('fill', 'url(#blackMarble)');

                } else if (color == 0) {

                    tile.find('circle.marble').first().removeClass('whiteMarble').removeClass('blackMarble').attr('fill', 'none');

                }
                ++realCol;
            }
        }
    }

    function nextPlayer() {
        playerTurn = playerTurn == 1 ? 2 : 1;
        $('#player').text('player ' + (playerTurn == 1 ? 'white' : 'black'));
    }

    function playTurn() {
        if (currentState != STOP) { // The game is stopped
            if (IAPlayTimeoutID != null) { // Wait if the previous turn is not done
                setTimeout(playTurn, 1000);
            } else {

                // Check if the game is over
                checkGameIsOverRequest(function(json) {
                    if(json.isOver) {

                        stopGame();

                        // Display winner
                        var genericModal = $('#generic-modal');
                        genericModal.find('h4.modal-title').first().text('Congratulation');
                        genericModal.find('div.modal-body').first().html(
                            '<div class="text-center">' +
                            '<h3>The ' + (playerTurn == 2 ? 'white' : 'black') + ' player win !</h3>' +
                            '</div>'
                        );
                        genericModal.find('div.modal-footer').first().html(
                            '<button type="button" class="btn btn-default" data-dismiss="modal">Ok</button> '
                        );

                        genericModal.modal('show');

                    } else {
                        // Play IA if it's a computer player
                        if (playerType[playerTurn] == COMPUTER) {
                            currentState = IA_PLAY;
                            IAPlayTimeoutID = setTimeout(function () {
                                makeIAPlayRequest(function (json) {
                                    board = json;
                                    updateBoard(board);
                                    nextPlayer();
                                    IAPlayTimeoutID = null;
                                    playTurn();
                                });
                            }, 200);

                        // Play manual
                        } else {
                            currentState = CHOOSE_MARBLE;
                        }

                    }
                });
            }
        }
    }

    function initGame() {
        getInitBoardRequest(function(json){
            board = json;
            updateBoard(board);

            $('#start-game').text('New Game');
            $('#stop-game').removeAttr('disabled');
            currentSelectedTile = null;
            playerTurn = 1;

            $('#player').text('player ' + (playerTurn == 1 ? 'white' : 'black'));
            $('#white-marble-out').text('0');
            $('#black-marble-out').text('0');

            currentState = READY;

            playTurn();
        });
    }

    function updateDebugInfo($tile) {

        if(DEBUG) {

            var circleEmptyTitle = $tile.children('circle.emptyTile').first();

            $('#printCx').text(circleEmptyTitle.attr('cx'));
            $('#printCy').text(circleEmptyTitle.attr('cy'));

            if($(this).has('circle.blackMarble').length) {

                $('#printColor').text('Black marble');

            } else if($(this).has('.whiteMarble').length) {

                $('#printColor').text('White marble');

            } else {

                $('#printColor').text('Empty tile');

            }
        }
    }

    function stopGame() {
        currentState = STOP;
        clearTimeout(IAPlayTimeoutID); // Stop the last IA Play
        IAPlayTimeoutID = null;
        $('#player').text('-');
        $('#stop-game').attr('disabled', 'disabled');
    }

    function startGame() {

        var genericModal = $('#generic-modal');
        genericModal.find('h4.modal-title').first().text('Choose play mode');
        genericModal.find('div.modal-body').first().html(
            '<div class="text-center">' +
            '<button id="mode-human-vs-human" type="button" class="btn btn-info">Human vs Human</button> ' +
            '<button id="mode-human-vs-computer" type="button" class="btn btn-info">Human vs Computer</button> ' +
            '<button id="mode-computer-vs-computer" type="button" class="btn btn-info">Computer vs Computer</button>' +
            '</div>'
        );
        genericModal.find('div.modal-footer').first().html(
            '<button type="button" class="btn btn-default" data-dismiss="modal">Cancel</button> '
        );

        function stopAndInitGame() {
            $('#generic-modal').modal('hide');
            if (currentState != STOP) {
                stopGame();
            }
            initGame();
        }

        $('#mode-human-vs-human').click(function() {
            playerType[1] = HUMAN;
            playerType[2] = HUMAN;
            stopAndInitGame();
        });

        $('#mode-human-vs-computer').click(function() {
            playerType[1] = HUMAN;
            playerType[2] = COMPUTER;
            $('#generic-modal').modal('hide');
            stopAndInitGame();
        });

        $('#mode-computer-vs-computer').click(function() {
            playerType[1] = COMPUTER;
            playerType[2] = COMPUTER;
            $('#generic-modal').modal('hide');
            stopAndInitGame();
        });

        genericModal.modal('show');

    }
});

