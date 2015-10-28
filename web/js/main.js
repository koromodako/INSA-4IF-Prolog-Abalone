$(function() {

    var DEBUG = 1;
    var currentSelectedTile = null;
    var board = null;
    var baseUrl = 'http://localhost:8080';
    var playerTurn = 1;

    $( document ).ready(function(){
        if(DEBUG){
            $('#debug').show();
        }
    });

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

    // Actions
    $('#start-game').click(function() {
        // TODO Add a loading popup
        getInitBoard(function(json, statut){
            board = json;
            updateBoard(board);
        });
    });

    $('g.tile').click( function(){
        if ((playerTurn == 1 && $(this).find('circle.marble.whiteMarble').length == 0)
            || (playerTurn == 2 && $(this).find('circle.marble.blackMarble').length == 0)) {
            // TODO Notification to explain why you can't play with this marble
            return;
        }
        if (currentSelectedTile) {
            currentSelectedTile.removeClass('active');
        }
        $(this).addClass('active');
        currentSelectedTile = $(this);

        getPlayerMovements(function(json, statut) {
            console.log(statut);
            console.log(json);
        });

        if(DEBUG) {
            // print debug tiles infos
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

    // Requests
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

        if (!currentSelectedTile) {
            return;
        }

        var position = getPositionOfTile(currentSelectedTile);

        $.ajax({
            method: 'POST',
            url: baseUrl + '/get/player/movements',
            dataType: 'json',
            data: [board, playerTurn, position.line, position.col],
            success: $success,
            error: function (resultat, statut, erreur) {
                alert("Erreur lors de l'appel pour récupérer les mouvements possibles.");
                console.log(resultat, statut, erreur);
            }
        });
    }

    // Utils
    function getClassArrayOfElement($elt) {
        return $elt.attr('class').split(/\s+/);
    }

    function getPositionOfTile($tile) {
        var col = null, line = null;

        $.each(getClassArrayOfElement($tile), function(index, item) {
            if (item.match("^col-")) {
                col = item.split('-')[1];
            } else if (item.match("^line-")) {
                line = item.split('-')[1];
            }
        });

        return {
            col: col,
            line: line
        };
    }
});

