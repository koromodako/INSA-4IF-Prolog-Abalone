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
    const INIT = 0;
    const CHOOSE_MARBLE = 1;
    const MARBLE_SELECTED = 2;
    const IA_PLAY = 3;

    /** Player **/
    const HUMAN = 0;
    const COMPUTER = 1;

    /**** Globals variables ****/
    var DEBUG = 1;
    var currentSelectedTile = null;
    var board = null;
    var baseUrl = location.protocol+'//'+location.hostname+(location.port ? ':'+location.port: '');
    var playerTurn = 1;
    var playerType = {1:null, 2:null};
    var currentState = INIT;
    var timeoutId = null;

    /*****************************************
     *** Actions                           ***
     *****************************************/

    $('#start-game').click(function() {

        var genericModal = $('#generic-modal');
        genericModal.find('h4.modal-title').first().text('Choose play mode');
        genericModal.find('div.modal-body').first().html(
            '<button id="mode-human-vs-human" type="button" class="btn btn-default">Human vs Human</button> '
            //+ '<button id="mode-human-vs-computer" type="button" class="btn btn-default">Human vs Computer</button> '
            + '<button id="mode-computer-vs-computer" type="button" class="btn btn-default">Computer vs Computer</button>'
        ).addClass('text-center');

        $('#mode-human-vs-human').click(function() {
            playerType[1] = HUMAN;
            playerType[2] = HUMAN;
            $('#generic-modal').modal('hide');
            initGame();
        });

        $('#mode-human-vs-computer').click(function() {
            playerType[1] = HUMAN;
            playerType[2] = COMPUTER;
            $('#generic-modal').modal('hide');
            initGame();
        });

        $('#mode-computer-vs-computer').click(function() {
            playerType[1] = COMPUTER;
            playerType[2] = COMPUTER;
            $('#generic-modal').modal('hide');
            initGame();
        });

        genericModal.modal('show');


        function initGame() {
            getInitBoard(function(json){
                board = json;
                updateBoard(board);

                $('#start-game').text('New Game');
                $('#stop-game').removeAttr('disabled');
                currentSelectedTile = null;
                playerTurn = 1;

                $('#player').text('player ' + (playerTurn == 1 ? 'white' : 'black'));
                $('#white-marble-out').text('0');
                $('#black-marble-out').text('0');

                currentState = IA_PLAY;
                function playIA(){
                    makeIAPlay(function (json) {
                        board = json;
                        updateBoard(board);
                        playerTurn = playerTurn == 1 ? 2 : 1;
                        $('#player').text('player ' + (playerTurn == 1 ? 'white' : 'black'));
                        if (playerType[playerTurn] == COMPUTER) {
                            timeoutId = setTimeout(playIA, 200);
                        } else {
                            currentState = CHOOSE_MARBLE;
                        }
                    });
                }
                timeoutId = setTimeout(playIA, 1000);
            });
        }

    });

    $('#stop-game').click(function() {
        clearTimeout(timeoutId);
        currentState = INIT;
        $('#player').text('-');
        $(this).attr('disabled', 'disabled');
    });

    $('g.tile').click( function() {

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
            getPlayerMovements(function(json, statut) {

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

                makePlayerMovement($(this), function(json, statut) {
                    board = json;
                    updateBoard(board);
                    playerTurn = playerTurn == 1 ? 2 : 1;
                    $('#player').text('player ' + (playerTurn == 1 ? 'white' : 'black'));
                    currentState = CHOOSE_MARBLE;
                });

            }

        }


        // Debug infos
        if(DEBUG) {

            var circleEmptyTitle = $(this).children('circle.emptyTile').first();

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
    });

    /*****************************************
     *** Requests                          ***
     *****************************************/

    function getInitBoard($success) {

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

    function getPlayerMovements($success) {

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

    function makePlayerMovement($moveToTile, $success) {

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

    function makeIAPlay($success) {

        $.ajax({
            method: 'POST',
            url: baseUrl + '/make/ia/play',
            async: false,
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

    /*****************************************
     *** Utils                             ***
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

    /*****************************************
     *** Update Board                      ***
     *****************************************/

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
});

