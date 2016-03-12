credentials <- function(keypair=NULL){
    message("credentials() is being deprecated!\nInstead, specify AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY environment variables.")
    if (!is.null(keypair)) {
        options(MTurkR.keypair = keypair)
    } else {
        return(getOption('MTurkR.keypair'))    
    }
}
