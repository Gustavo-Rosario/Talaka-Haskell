$ = jQuery;

$(function(){
    $("#go").click(function(){
        let pid = parseInt($("#pid").val());
        $.ajax({
            url: "/project/"+pid,
            type: "GET",
            success: function(result) {
                alert(JSON.stringify(result));
            },
            dataType: "json"
        });
        
    });
});