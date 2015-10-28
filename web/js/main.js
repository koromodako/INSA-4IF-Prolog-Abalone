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
    const UPDATE_BOARD = 3;

    /**** Globals variables ****/
    var DEBUG = 1;
    var currentSelectedTile = null;
    var board = null;
    var baseUrl = 'http://localhost:8080';
    var playerTurn = 1;
    var currentState = INIT;

    /*****************************************
     *** Actions                           ***
     *****************************************/

    $('#start-game').click(function() {
        // TODO Add a loading popup
        getInitBoard(function(json, statut){
            board = json;
            updateBoard(board);
            currentState = CHOOSE_MARBLE;
        });
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

                    if (line > 5) {
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
                    playerTurn = playerTurn % 2 ? 2 : 1;
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

        if (line > 5) {
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

