RegisterHITType <-
hittype <-
function (title, description, reward, duration, keywords = NULL, 
    auto.approval.delay = NULL, qual.req = NULL,
    verbose = getOption('MTurkR.verbose', TRUE), ...){
    operation <- "RegisterHITType"
    if (nchar(curl_escape(title)) > 128) {
        stop("Title too long (128 char max)")
    }
    if (nchar(curl_escape(description)) > 2000) {
        stop("Description too long (2000 char max)")
    }
    if (as.numeric(duration) < 30 || as.numeric(duration) > 3153600) {
        stop("Duration must be between 30 (30 seconds) and 3153600 (365 days)")
    }
    GETparameters <- paste("&Title=", curl_escape(title), 
                           "&Description=", curl_escape(description), 
                           "&Reward.1.Amount=", reward, 
                           "&Reward.1.CurrencyCode=USD", "&AssignmentDurationInSeconds=", 
                           duration, sep = "")
    if (!is.null(keywords)) {
        if (nchar(curl_escape(keywords)) < 1000) {
            GETparameters <- paste(GETparameters, "&Keywords=", curl_escape(keywords), sep = "")
        } else {
            stop("Keywords too long (1000 char max)")
        }
    }
    if (!is.null(auto.approval.delay)) {
        if (as.numeric(auto.approval.delay) > 0 & as.numeric(auto.approval.delay) <= 2592000) {
            GETparameters <- paste(GETparameters, "&AutoApprovalDelayInSeconds=", auto.approval.delay, sep = "")
        } else {
            warning("AutoApprovalDelayInSeconds must be between 0 (0 seconds) and 2592000 (30 days); defaults to 30 days")
        }
    }
    if (!is.null(qual.req)) {
        GETparameters <- paste(GETparameters, qual.req, sep = "")
    }
    HITType <- emptydf(1, 2, c("HITTypeId", "Valid"))
    request <- request(operation, GETparameters = GETparameters, ...)
    if (is.null(request$valid)) {
        return(request)
    }
    if (request$valid == TRUE) {
        hit.type <- strsplit(strsplit(request$xml, "<HITTypeId>")[[1]][2], "</HITTypeId>")[[1]][1]
        HITType[1, ] <- c(hit.type, request$valid)
        if (verbose) {
            message("HITType Registered: ", HITType$HITTypeId[1])
        }
    } else if (request$valid == FALSE) {
        HITType[1, ] <- c(NULL, request$valid)
        if (verbose) {
            warning("Invalid Request")
        }
    }
    HITType$Valid <- factor(HITType$Valid, levels=c('TRUE','FALSE'))
    return(HITType)
}
