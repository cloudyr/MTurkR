credentials <- function(keypair=NULL){
    if(!is.null(keypair))
        options(MTurkR.keypair = keypair)
    else
        return(getOption('MTurkR.keypair'))
    
}
