$(function() {

    var DEBUG = 1;

    $( document ).ready(function(){

        // hide debug section if not in debug mode
        if(!DEBUG){
            $( "#debug" ).hide();
        }

    });

    function isNextTo(currentTile, selectedTile) {
        var dCx = $(selectedTile).children("circle.emptyTile").attr("cx") - $(currentTile).children("circle.emptyTile").attr("cx");
        var dCy = $(selectedTile).children("circle.emptyTile").attr("cy") - $(currentTile).children("circle.emptyTile").attr("cy");
        var distance = Math.sqrt(Math.pow(dCx,2)+Math.pow(dCy,2));
        return distance<(20*2);
    };

    function move(currentTile, nextTile) {
        if($(nextTile).children("circle.marble").length == 0) {
            // if no marble -> empty tile

            // move the xml object to the right tile
            var marble = $(currentTile).children("circle.marble");
            $(nextTile).append(marble);
            var cx = $(nextTile).children("circle.emptyTile").attr("cx");
            var cy = $(nextTile).children("circle.emptyTile").attr("cy");
            $(marble).attr("cx", cx);
            $(marble).attr("cy", cy);
        }
    }

    var currentSelectedTile = null;
    $('g.tile').click( function(){
        if (currentSelectedTile) {
            if($(currentSelectedTile).children("circle.marble") && isNextTo(currentSelectedTile, this)) {
                move(currentSelectedTile, this);
            }
            currentSelectedTile.removeClass('active');
        }
        $(this).addClass('active');
        currentSelectedTile = $(this);

        if(DEBUG) {
            // print debug tiles infos
            var circleEmptyTitle = $(this).children("circle.emptyTile").first();
            $( "#printCx" ).text(circleEmptyTitle.attr("cx"));
            $( "#printCy" ).text(circleEmptyTitle.attr("cy"));
            if($(this).has("circle.blackMarble").length) {
                $( "#printColor" ).text("Black marble");
            } else if($(this).has(".whiteMarble").length) {
                $( "#printColor" ).text("White marble");
            } else {
                $( "#printColor" ).text("Empty tile");
            }
        }


    });

});

