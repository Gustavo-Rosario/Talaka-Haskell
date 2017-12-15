$(main)

function main(){
    $('#close').click(function(){
       $('#bgMenu').fadeOut();
       $('#financiamento, #comentarioArea').fadeOut();
    });
    $('#btnFinanciar').click(function(){
        $('#bgMenu').fadeIn();
        $("#financiamento").fadeIn().css('display','flex');
    });
    $('#btnComentar').click(function(){
        $('#bgMenu').fadeIn();
        $("#comentarioArea").fadeIn().css('display','flex');
    });
    //Confirmacao de Admin
    $("form.delProj").submit(function(e){
        e.preventDefault();
        let id = e.target.id;
        var $toastContent = $('<span>Deletar Projeto?</span>').add($('<button class="btn-flat toast-action" onclick="delProjConfirm('+id+')">Sim</button>'));
        Materialize.toast($toastContent, 10000);
        return false;
    });
    
    
    //================================== AJAX ==================================
    $("#comentario").submit(function(event){
        event.preventDefault();
        console.log("ALOOOO");
        // let pid = parseInt($("#pid").val());
        // $.ajax({
        //     url: "/project/"+pid,
        //     type: "GET",
        //     success: function(result) {
        //         alert(JSON.stringify(result));
        //     },
        //     dataType: "json"
        // });
    });
    
}
//ToolTip
$(document).ready(function(){
    $('.tooltipped').tooltip({delay: 50});
    
});


function delProjConfirm(id){
    id.submit();
}