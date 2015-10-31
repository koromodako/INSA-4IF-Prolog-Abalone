$(function() {

    $( document ).ready(function(){
        if(debug){
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

    /** Rules **/
    const numberOutToLoose = 6;
    const numberInitMax = 14;

    /**** Globals variables ****/
    var debug = true;
    var currentSelectedTile = null;
    var board = null;
    var playerTurn = 1;
    var playerType = {1:null, 2:null};
    var currentState = STOP;
    var IAPlayTimeoutID = null;
    var IAPlayerConfiguration = {
        1: {
            depth: 1,
            aggressiveness: 1000
        },
        2: {
            depth: 1,
            aggressiveness: 1000
        }
    };

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

                var min = 0, max = 0;
                if (debug) {
                    $.each(json, function(index, item) {
                        if (index == 0) {
                            min = item[2];
                            max = item[2];
                        } else {
                            if (min > item[2]) {
                                min = item[2];
                            } else if (max < item[2]) {
                                max = item[2];
                            }
                        }
                    });
                }

                $.each(json, function(index, item) {

                    var col = item[1];
                    var line = item[0];
                    var tile = $('g.tile.col-' + col + '.line-' + line);
                    tile.addClass('movable');

                    if (debug) {
                        tile.find('title.title').first().text('Score : ' + (Math.round(item[2] * 100) / 100));
                        var scoreNorm = ((( item[2] - min ) / ( max - min )) / 3 ) * 2;
                        tile.find('polygon').first().css('fill', 'rgba(195,225,227,' + (scoreNorm + 0.33) + ')');
                    }
                });
            });

        } else if (currentState == MARBLE_SELECTED) {

            // Not a movement available
            if (!$(this).hasClass('movable')) {

                removeSelectedAndMovableClass();
                currentState = CHOOSE_MARBLE;

            } else { // Make movement

                removeSelectedAndMovableClass();

                makePlayerMovementRequest($(this), function(json) {
                    board = json;
                    updateBoard();
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

    function getInitBoardRequest(success) {

        $.ajax({
            method: 'POST',
            url: baseUrl + '/get/init/board',
            success: success,
            error: function (resultat, statut, erreur) {
                alert("Erreur lors de l'appel de l'initialisation du plateau.");
                console.log(resultat, statut, erreur);
            }
        });

    }

    function getPlayerMovementsRequest(success) {

        var position = getPositionOfTile(currentSelectedTile);

        $.ajax({
            method: 'POST',
            url: baseUrl + '/get/player/movements',
            dataType: 'json',
            data: JSON.stringify({
                Board: board,
                Player: playerTurn,
                Line: position.line,
                Col: position.col,
                Debug: debug
            }),
            contentType: "application/json",
            success: success,
            error: function (resultat, statut, erreur) {
                alert("Erreur lors de l'appel pour récupérer les mouvements possibles.");
                console.log(resultat, statut, erreur);
            }
        });
    }

    function makePlayerMovementRequest(moveToTile, success) {

        var position = getPositionOfTile(currentSelectedTile);
        var destination = getPositionOfTile(moveToTile);

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
            success: success,
            error: function (resultat, statut, erreur) {
                alert("Erreur lors de l'appel pour effectuer le movement.");
                console.log(resultat, statut, erreur);
            }
        });
    }

    function makeIAPlayRequest(success) {

        $.ajax({
            method: 'POST',
            url: baseUrl + '/make/ia/play',
            dataType: 'json',
            data: JSON.stringify({
                Board: board,
                Player: playerTurn,
                Depth: IAPlayerConfiguration[playerTurn].depth,
                Aggressiveness: IAPlayerConfiguration[playerTurn].aggressiveness
            }),
            contentType: "application/json",
            success: success,
            error: function (resultat, statut, erreur) {
                alert("Erreur lors de l'appel pour faire jouer l'IA.");
                console.log(resultat, statut, erreur);
            }
        });
    }

    function checkGameIsOverRequest(success) {

        $.ajax({
            method: 'POST',
            url: baseUrl + '/check/game/is/over',
            dataType: 'json',
            data: JSON.stringify({
                Board: board,
                Player: playerTurn
            }),
            contentType: "application/json",
            success: success,
            error: function (resultat, statut, erreur) {
                alert("Erreur lors de l'appel pour vérifier si le jeu est terminé.");
                console.log(resultat, statut, erreur);
            }
        });
    }

    /*****************************************
     *** functions                         ***
     *****************************************/

    function getClassArrayOfElement(elt) {
        return elt.attr('class').split(/\s+/);
    }

    function getPositionOfTile(tile) {
        var col = null, line = null;

        $.each(getClassArrayOfElement(tile), function(index, item) {
            if (item.match("^col-")) { // TODO add in the regex the number
                col = parseInt(item.split('-')[1]);
            } else if (item.match("^line-")) {
                line = parseInt(item.split('-')[1]);
            }
        });

        return {
            col: col,
            line: line
        };
    }

    function updateBoard() {

        var whiteMarblesLeft = 0;
        var blackMarblesLeft = 0;
        var line;
        for (line = 0; line < board.length; ++line) {
            var col;
            for (col = 0; col < board[line].length; ++col) {

                var color = board[line][col];

                if (color < 0) {
                    continue;
                }

                var tile = $('g.tile.col-' + (col+1) + '.line-' + (line+1)).first();

                if (color == 1) {

                    if (tile.find('circle.marble.whiteMarble').length == 0) {
                        tile.find('circle.marble').first().removeClass('blackMarble').addClass('whiteMarble').attr('fill', 'url(#whiteMarble)');
                    }

                    whiteMarblesLeft++;

                } else if (color == 2) {

                    if (tile.find('circle.marble.blackMarble').length == 0) {
                        tile.find('circle.marble').first().removeClass('whiteMarble').addClass('blackMarble').attr('fill', 'url(#blackMarble)');
                    }

                    blackMarblesLeft++;

                } else if (color == 0) {

                    tile.find('circle.marble').first().removeClass('whiteMarble').removeClass('blackMarble').attr('fill', 'none');

                }
            }
        }

        // TODO move this section in the right place
        $('#player-1-number-out').text((numberInitMax - whiteMarblesLeft) + ' / ' + numberOutToLoose);
        $('#player-2-number-out').text((numberInitMax - blackMarblesLeft) + ' / ' + numberOutToLoose);

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
                            '<h3>The ' + (playerTurn == 2 ? 'white' : 'black') + ' player wins !</h3>' +
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
                                showLoading('Wait ...');
                                makeIAPlayRequest(function (json) {
                                    board = json;
                                    updateBoard();
                                    nextPlayer();
                                    IAPlayTimeoutID = null;
                                    playTurn();
                                    hideLoading();
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
            updateBoard();

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

    function updateDebugInfo(tile) {

        if(debug) {

            var circleEmptyTitle = tile.children('circle.emptyTile').first();

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

        if (IAPlayTimeoutID != null) {
            clearTimeout(IAPlayTimeoutID); // Stop the last IA Play
            IAPlayTimeoutID = null;
        }

        $('#player').text('-');
        $('#stop-game').attr('disabled', 'disabled');
        removeSelectedAndMovableClass();

    }

    function startGame() {

        var genericModal = $('#generic-modal');
        var genericModalTitle = genericModal.find('h4.modal-title').first();
        var genericModalBody = genericModal.find('div.modal-body').first();
        var genericModalFooter = genericModal.find('div.modal-footer').first();
        genericModalTitle.text('Choose play mode');
        genericModalBody.html(
            '<div class="text-center">' +
                '<button id="mode-human-vs-human" type="button" class="btn btn-info">Human vs Human</button> ' +
                '<button id="mode-human-vs-computer" type="button" class="btn btn-info">Human vs Computer</button> ' +
                '<button id="mode-computer-vs-computer" type="button" class="btn btn-info">Computer vs Computer</button>' +
            '</div>'
        );
        genericModalFooter.html(
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
            genericModal.modal('hide');
            stopAndInitGame();
        });

        $('#mode-human-vs-computer').click(function() {
            playerType[1] = HUMAN;
            playerType[2] = COMPUTER;
            genericModalTitle.text('Configuration IA Player');
            genericModalBody.html(
                '<form id="setConfigurationIA" class="form-horizontal">' +
                    '<div class="form-group">' +
                        '<label for="selectDifficulty" class="col-sm-5 control-label">Select the difficulty</label>' +
                        '<div class="col-sm-6">' +
                            '<select id="selectDifficulty" class="form-control">' +
                                '<option value="1">Easy (depth 1)</option>' +
                                '<option value="2">Medium (depth 2)</option>' +
                                '<option value="3">Expert (depth 3)</option>' +
                            '</select>' +
                        '</div>' +
                    '</div>' +
                    '<div class="form-group">' +
                        '<label for="selectAggressiveness" class="col-sm-5 control-label">Select the aggressiveness</label>' +
                        '<div class="col-sm-6">' +
                            '<select id="selectAggressiveness" class="form-control">' +
                                '<option value="500">Gentle (500)</option>' +
                                '<option value="1000">Default (1000)</option>' +
                                '<option value="1500">Aggressive (1500)</option>' +
                            '</select>' +
                        '</div>'+
                    '</div>'+
                    '<div class="text-right">' +
                        '<button type="button" class="btn btn-default" data-dismiss="modal">Cancel</button>  ' +
                        '<button type="submit" class="btn btn-primary">Start</button> ' +
                    '</div>'+
                '</form>'
            );
            genericModalFooter.html('');
            $( "#setConfigurationIA" ).submit(function( event ) {
                event.preventDefault();
                IAPlayerConfiguration[2].depth = parseInt($('#selectDifficulty').val());
                IAPlayerConfiguration[2].aggressiveness = parseInt($('#selectAggressiveness').val());
                $('#generic-modal').modal('hide');
                stopAndInitGame();
            });
        });

        $('#mode-computer-vs-computer').click(function() {
            playerType[1] = COMPUTER;
            playerType[2] = COMPUTER;
            genericModalTitle.text('Configuration IA Players');
            genericModalBody.html(
                '<form id="setConfigurationIA" class="form-horizontal">' +
                    '<fieldset>' +
                        '<legend>White Player</legend>' +
                        '<div class="form-group">' +
                            '<label for="selectDifficultyPlayer1" class="col-sm-5 control-label">Select the difficulty</label>' +
                            '<div class="col-sm-6">' +
                                '<select id="selectDifficultyPlayer1" class="form-control">' +
                                    '<option value="1">Easy (depth 1)</option>' +
                                    '<option value="2">Medium (depth 2)</option>' +
                                    '<option value="3">Expert (depth 3)</option>' +
                                '</select>' +
                            '</div>' +
                        '</div>' +
                        '<div class="form-group">' +
                            '<label for="selectAggressivenessPlayer1" class="col-sm-5 control-label">Select the aggressiveness</label>' +
                            '<div class="col-sm-6">' +
                                '<select id="selectAggressivenessPlayer1" class="form-control">' +
                                    '<option value="500">Gentle (500)</option>' +
                                    '<option value="1000">Default (1000)</option>' +
                                    '<option value="1500">Aggressive (1500)</option>' +
                                '</select>' +
                            '</div>'+
                        '</div>'+
                    '</fieldset>' +
                    '<fieldset>' +
                        '<legend>Black Player</legend>' +
                        '<div class="form-group">' +
                            '<label for="selectDifficultyPlayer2" class="col-sm-5 control-label">Select the difficulty</label>' +
                            '<div class="col-sm-6">' +
                                '<select id="selectDifficultyPlayer2" class="form-control">' +
                                    '<option value="1">Easy (depth 1)</option>' +
                                    '<option value="2">Medium (depth 2)</option>' +
                                    '<option value="3">Expert (depth 3)</option>' +
                                '</select>' +
                            '</div>' +
                        '</div>' +
                        '<div class="form-group">' +
                            '<label for="selectAggressivenessPlayer2" class="col-sm-5 control-label">Select the aggressiveness</label>' +
                            '<div class="col-sm-6">' +
                                '<select id="selectAggressivenessPlayer2" class="form-control">' +
                                    '<option value="500">Gentle (500)</option>' +
                                    '<option value="1000">Default (1000)</option>' +
                                    '<option value="1500">Aggressive (1500)</option>' +
                                '</select>' +
                            '</div>'+
                        '</div>'+
                    '</fieldset>' +
                    '<div class="text-right">' +
                        '<button type="button" class="btn btn-default" data-dismiss="modal">Cancel</button>  ' +
                        '<button type="submit" class="btn btn-primary">Start</button> ' +
                    '</div>'+
                '</form>'
            );
            genericModalFooter.html('');
            $( "#setConfigurationIA" ).submit(function( event ) {
                event.preventDefault();
                IAPlayerConfiguration[1].depth = parseInt($('#selectDifficultyPlayer1').val());
                IAPlayerConfiguration[1].aggressiveness = parseInt($('#selectAggressivenessPlayer1').val());
                IAPlayerConfiguration[2].depth = parseInt($('#selectDifficultyPlayer2').val());
                IAPlayerConfiguration[2].aggressiveness = parseInt($('#selectAggressivenessPlayer2').val());
                $('#generic-modal').modal('hide');
                console.log(IAPlayerConfiguration);
                stopAndInitGame();
            });
        });

        genericModal.modal('show');

    }

    function removeSelectedAndMovableClass() {
        if (debug) {
            $('g.tile.movable').each(function() {
                $(this).find('polygon').css('fill', '');
            }).removeClass('movable');
        } else {
            $('g.tile.movable').removeClass('movable');
        }
        $('g.tile.selected').removeClass('selected');
    }

    function showLoading(message) {
        var loading = $('#loading');
        if (message) {
            loading.html(message);
        } else {
            loading.html('');
        }
        loading.show();
    }

    function hideLoading() {
        $('#loading').hide();
    }
});

