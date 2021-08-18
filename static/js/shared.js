/*
    Método que gera requisições POST com um objeto de dados
    e executa um after function se a mesma tiver obtido sucesso
*/
function RequestPOST (url, data, afterFunction){
    $.ajax({
        url: url,
        method: "post",
        data: data,
        dataType: "json",
        success: afterFunction,
        error: function(jqXHRm, error, ex){
            toast(error);
        }
    })
}

/*
    Método que gera requisições GET a partir de uma uri
    e executa um after function se a mesma tiver obtido sucesso
*/
function RequestGET (url, afterFunction){
    $.ajax({
        url: url,
        method: "get",
        success: afterFunction,
        error: function(jqXHRm, error, ex){
            toast(error);
        }
    })
}

/*
    Função que seja avisos no front-end
*/
function toast (message){
    $('.toast-body').html(message);

    var toast = new bootstrap.Toast($('.toast'));

    toast.show();
}