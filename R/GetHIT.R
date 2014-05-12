GetHIT <-
gethit <-
hit <-
function(hit, response.group = NULL,
    return.hit.dataframe = TRUE, return.qual.dataframe = TRUE,
    verbose = getOption('MTurkR.verbose'), ...){
    # temporary check for `print` argument (remove after v1.0)
    if('print' %in% names(list(...)) && is.null(verbose))
        verbose <- list(...)$print
    operation <- "GetHIT"
    GETparameters <- paste("&HITId=", hit, sep = "")
    if(!is.null(response.group)) {
        if(!response.group %in% c("Request", "Minimal", "HITDetail", 
            "HITQuestion", "HITAssignmentSummary")) 
            stop("ResponseGroup must be in c(Request,Minimal,HITDetail,HITQuestion,HITAssignmentSummary)")
        if(length(response.group) == 1) 
            GETparameters <- paste(GETparameters, "&ResponseGroup=", 
                response.group, sep = "")
        else {
            for(i in 1:length(response.group)) {
                GETparameters <- paste(GETparameters, "&ResponseGroup", 
                    i - 1, "=", response.group[i], sep = "")
            }
        }
    }
    request <- request(operation, GETparameters = GETparameters, ...)
    if(is.null(request$valid))
        return(request)
    if(request$valid == TRUE) {
        z <- HITsToDataFrame(xml = request$xml, sandbox = sandbox)
        if(verbose) 
            message("HIT (", hit, ") Retrieved")
        if(return.hit.dataframe == TRUE & return.qual.dataframe == TRUE) 
            return.list <- list(HITs = z$HITs, QualificationRequirements = z$QualificationRequirements)
        else if(return.hit.dataframe == TRUE & return.qual.dataframe == FALSE) 
            return.list <- list(HITs = z$HITs)
        else if(return.hit.dataframe == FALSE & return.qual.dataframe == TRUE) 
            return.list <- list(QualificationRequirements = z$QualificationRequirements)
        else
            return.list <- NULL
    }
    else {
        if(verbose) 
            message("No HITs Retrieved")
        return.list <- NULL
    }
    return(return.list)
}
