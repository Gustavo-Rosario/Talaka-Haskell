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
    //COMENTAR
    $("#comentario").submit(function(event){
        event.preventDefault();
        let action = $(this).prop("action");
        let $f = document.getElementById("comentario"); 
        let form = new FormData($f);
        $.ajax({
            url: action,
            data: form,
            processData: false,
            contentType: false,
            type: 'POST',
            method: 'POST'
        }).done(function(response){
            
            if(response.stats == "success"){
                 Materialize.toast("Comentário realizado com Sucesso", 4000);
                 // Inserir mensagem
                 let msg =  $("#comentario #hident2").val();
                 $("#listaComentarios").prepend("<li><b>"+response.msg+"</b> "+msg+"</li>");
            }else{
                Materialize.toast("Erro ao Comentar", 4000);
                console.log(response);
            }
        }).fail(function(response){
            Materialize.toast("Erro no Servidor", 4000);
            console.log(response);
        });
        return false;
    });

    //DELETAR COMENTARIO
    // $("#comentario").submit(function(event){
    //     event.preventDefault();
    //     let action = $(this).prop("action");
    //     let $f = document.getElementById("comentario"); 
    //     let form = new FormData($f);
    //     $.ajax({
    //         url: action,
    //         data: form,
    //         processData: false,
    //         contentType: false,
    //         type: 'DELETE'
    //     }).done(function(response){
            
    //         if(response.stats == "success"){
    //              Materialize.toast("Comentário realizado com Sucesso", 4000);
    //              // Inserir mensagem
    //              let msg =  $("#comentario #hident2").val();
    //              $("#listaComentarios").prepend("<li><b>"+response.msg+"</b> "+msg+"</li>");
    //         }else{
    //             Materialize.toast("Erro ao Comentar", 4000);
    //             console.log(response);
    //         }
    //     }).fail(function(response){
    //         Materialize.toast("Erro no Servidor", 4000);
    //         console.log(response);
    //     });
    //     return false;
    // });
}
//ToolTip
$(document).ready(function(){
    $('.tooltipped').tooltip({delay: 50});
    
});


function delProjConfirm(id){
    id.submit();
}