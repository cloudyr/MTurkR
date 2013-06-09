GetReviewResultsForHIT <-
reviewresults <-
function (hit, assignment = NULL, policy.level = NULL, retrieve.results = TRUE, 
    retrieve.actions = TRUE, keypair = credentials(), print = TRUE, 
    browser = FALSE, log.requests = TRUE, sandbox = FALSE) 
{
    keyid <- keypair[1]
    secret <- keypair[2]
    operation = "GetReviewResultsForHIT"
    GETparameters <- paste("&HITId=", hit, sep = "")
    if (!is.null(policy.level)) {
        if (!policy.level %in% c("HIT", "Assignments")) 
            stop("PolicyLevel must be 'HIT' | 'Assignments'")
        GETparameters <- paste(GETparameters, "&PolicyLevel=", 
            policy.level, sep = "")
    }
    if (!is.null(assignment)) 
        GETparameters <- paste(GETparameters, "&AssignmentId=", 
            assignment, sep = "")
    if (!is.null(retrieve.actions)) {
        if (!retrieve.actions %in% c(TRUE, FALSE)) 
            stop("RetrieveActions must be TRUE or FALSE")
        else if (retrieve.actions == TRUE) 
            GETparameters <- paste(GETparameters, "&RetrieveActions=T", 
                sep = "")
        else if (retrieve.actions == FALSE) 
            GETparameters <- paste(GETparameters, "&RetrieveActions=F", 
                sep = "")
    }
    if (!is.null(retrieve.results)) {
        if (!retrieve.results %in% c(TRUE, FALSE)) 
            stop("RetrieveResults must be TRUE or FALSE")
        else if (retrieve.results == TRUE) 
            GETparameters <- paste(GETparameters, "&RetrieveResults=T", 
                sep = "")
        else if (retrieve.results == FALSE) 
            GETparameters <- paste(GETparameters, "&RetrieveResults=F", 
                sep = "")
    }
    auth = authenticate(operation, secret)
    if (browser == TRUE) {
        request <- request(keyid, auth$operation, auth$signature, 
            auth$timestamp, GETparameters, browser = browser, 
            sandbox = sandbox)
    }
    else {
        request <- request(keyid, auth$operation, auth$signature, 
            auth$timestamp, GETparameters, log.requests = log.requests, 
            sandbox = sandbox)
        if (request$valid == TRUE) {
            ReviewResults <- ReviewResultsToDataFrame(xml = request$xml)
            if (print == TRUE) {
                cat("ReviewResults Retrieved: ")
                if (is.null(ReviewResults)) 
                  cat("0\n")
                else {
                  if ("AssignmentReviewResult" %in% names(ReviewResults)) 
                    cat(length(ReviewResults$AssignmentReviewResults), 
                      " Assignment ReviewResults Retrieved\n", 
                      sep = "")
                  if ("AssignmentReviewAction" %in% names(ReviewResults)) 
                    cat(length(ReviewResults$AssignmentReviewResults), 
                      " Assignment ReviewActions Retrieved\n", 
                      sep = "")
                  if ("HITReviewResult" %in% names(ReviewResults)) 
                    cat(length(ReviewResults$AssignmentReviewResults), 
                      " HIT ReviewResults Retrieved\n", sep = "")
                  if ("HITReviewAction" %in% names(ReviewResults)) 
                    cat(length(ReviewResults$AssignmentReviewResults), 
                      " HIT ReviewActions Retrieved\n", sep = "")
                  return(ReviewResults)
                }
            }
            else invisible(ReviewResults)
        }
        else if (request$valid == FALSE) {
            if (print == TRUE) 
                cat("Invalid Request\n")
            invisible(request)
        }
    }
}
