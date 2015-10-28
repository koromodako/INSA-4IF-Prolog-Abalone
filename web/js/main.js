$(function() {

    var DEBUG = 1;
    var currentSelectedTile = null;
    var board = null;
    var baseUrl = 'http://localhost:8080';

    $( document ).ready(function(){
        if(DEBUG){
            $('#debug').show();
        }
    });


    $('g.tile').click( function(){
        if (currentSelectedTile) {
            currentSelectedTile.removeClass('active');
        }
        $(this).addClass('active');
        currentSelectedTile = $(this);

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

    $('#start-game').click( function(){
        $.ajax({
            method: 'POST',
            url: baseUrl + '/get/init/board',
            success: function(json, statut){
                board = json;
                updateBoard(board);
            },
            error: function (resultat, statut, erreur) {
                alert("Erreur lors de l'appel de l'initialisation du plateau.");
                console.log(resultat, statut, erreur);
            }
        });
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
});

