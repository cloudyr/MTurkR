GetReviewResultsForHIT <-
reviewresults <-
function (hit, assignment = NULL, policy.level = NULL, retrieve.results = TRUE, 
    retrieve.actions = TRUE, keypair = getOption('MTurkR.keypair'), print = getOption('MTurkR.print'), 
    log.requests = getOption('MTurkR.log'),
    sandbox = getOption('MTurkR.sandbox'), validation.test = getOption('MTurkR.test')) {
    if(is.null(keypair))
        stop("No keypair provided or 'credentials' object not stored")
    operation <- "GetReviewResultsForHIT"
    if(is.null(hit))
        stop("Must specify HITId as 'hit'")
    if(is.factor(hit))
        hit <- as.character(hit)
    GETparameters <- paste("&HITId=", hit, sep = "")
    if(!is.null(policy.level)) {
        if(!policy.level %in% c("HIT", "Assignments")) 
            stop("PolicyLevel must be 'HIT' | 'Assignments'")
        GETparameters <- paste(GETparameters, "&PolicyLevel=", 
            policy.level, sep = "")
    }
    if(!is.null(assignment)) {
        if(is.factor(assignment))
            assignment <- as.character(assignment)
        GETparameters <- paste(GETparameters, "&AssignmentId=", 
            assignment, sep = "")
    }
    if(!is.null(retrieve.actions)) {
        if(!retrieve.actions %in% c(TRUE, FALSE)) 
            stop("RetrieveActions must be TRUE or FALSE")
        else if(retrieve.actions == TRUE) 
            GETparameters <- paste(GETparameters, "&RetrieveActions=T", sep = "")
        else if(retrieve.actions == FALSE) 
            GETparameters <- paste(GETparameters, "&RetrieveActions=F", sep = "")
    }
    if(!is.null(retrieve.results)) {
        if(!retrieve.results %in% c(TRUE, FALSE)) 
            stop("RetrieveResults must be TRUE or FALSE")
        else if(retrieve.results == TRUE) 
            GETparameters <- paste(GETparameters, "&RetrieveResults=T", sep = "")
        else if(retrieve.results == FALSE) 
            GETparameters <- paste(GETparameters, "&RetrieveResults=F", sep = "")
    }
    
    request <- request(keypair[1], operation, secret=keypair[2],
        GETparameters = GETparameters, log.requests = log.requests, 
        sandbox = sandbox, validation.test = validation.test)
    if(validation.test)
        return(invisible(request))
    if(request$valid == TRUE) {
        ReviewResults <- ReviewResultsToDataFrame(xml = request$xml)
        if(print == TRUE) {
            message("ReviewResults Retrieved: ", appendLF=FALSE)
            if(is.null(ReviewResults)) 
                message("0\n")
            else {
              if("AssignmentReviewResult" %in% names(ReviewResults)) 
                message(length(ReviewResults$AssignmentReviewResults), 
                  " Assignment ReviewResults Retrieved")
              if("AssignmentReviewAction" %in% names(ReviewResults)) 
                message(length(ReviewResults$AssignmentReviewResults), 
                  " Assignment ReviewActions Retrieved")
              if("HITReviewResult" %in% names(ReviewResults)) 
                message(length(ReviewResults$AssignmentReviewResults), 
                  " HIT ReviewResults Retrieved")
              if("HITReviewAction" %in% names(ReviewResults)) 
                message(length(ReviewResults$AssignmentReviewResults), 
                  " HIT ReviewActions Retrieved")
            }
        }
    }
    else if(request$valid == FALSE & print == TRUE)
        warning("Invalid Request")
    return(ReviewResults)
}
