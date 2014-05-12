genericmturkr <-
function(operation, parameters = NULL, ...){
    if(is.null(keypair))
        stop("No keypair provided or 'credentials' object not stored")
    operation <- operation
    GETparameters <- parameters
    request <- request(operation, GETparameters = GETparameters, ...)
    if(is.null(request$valid))
        return(request)
    if(request$valid & verbose)
        message("Operation (", operation, ") Successful")
    else if(!request$valid & verbose)
        warning("Invalid Request")
    return(request)
}
