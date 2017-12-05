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
    
}