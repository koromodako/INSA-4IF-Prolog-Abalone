$(function() {

    var DEBUG = 1;

    $( document ).ready(function(){

        // hide debug section if not in debug mode
        if(!DEBUG){
            $( "#debug" ).hide();
        }

    });


    var currentSelectedTile = null;
    $('g.tile').click( function(){
        if (currentSelectedTile) {
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

