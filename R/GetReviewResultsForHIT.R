GetReviewResultsForHIT <-
reviewresults <-
function(hit, 
         assignment = NULL, 
         policy.level = NULL, 
         retrieve.results = TRUE, 
         retrieve.actions = TRUE, 
         return.all = FALSE, 
         pagenumber = 1, 
         pagesize = 400, 
         verbose = getOption('MTurkR.verbose', TRUE), ...) {
    operation <- "GetReviewResultsForHIT"
    if (is.null(hit)) {
        stop("Must specify HITId as 'hit'")
    }
    if (is.factor(hit)) {
        hit <- as.character(hit)
    }
    GETparameters <- paste("&HITId=", hit, sep = "")
    if (!is.null(policy.level)) {
        if (!policy.level %in% c("HIT", "Assignment")) {
            stop("PolicyLevel must be 'HIT' | 'Assignment'")
        }
        GETparameters <- paste(GETparameters, "&PolicyLevel=", policy.level[1], sep = "")
    }
    if (!is.null(assignment)) {
        if (is.factor(assignment)) {
            assignment <- as.character(assignment)
        }
        GETparameters <- paste(GETparameters, "&AssignmentId=", assignment, sep = "")
    }
    if (!is.null(retrieve.actions)) {
        if (!retrieve.actions %in% c(TRUE, FALSE)) {
            stop("RetrieveActions must be TRUE or FALSE")
        } else if (retrieve.actions == TRUE) {
            GETparameters <- paste(GETparameters, "&RetrieveActions=T", sep = "")
        } else if (retrieve.actions == FALSE) {
            GETparameters <- paste(GETparameters, "&RetrieveActions=F", sep = "")
        }
    }
    if (!is.null(retrieve.results)) {
        if (!retrieve.results %in% c(TRUE, FALSE)) {
            stop("RetrieveResults must be TRUE or FALSE")
        } else if (retrieve.results == TRUE) {
            GETparameters <- paste(GETparameters, "&RetrieveResults=T", sep = "")
        } else if (retrieve.results == FALSE) {
            GETparameters <- paste(GETparameters, "&RetrieveResults=F", sep = "")
        }
    }
    if (as.numeric(pagesize) < 1) 
        stop("'pagesize' must be greater than 1")
    if (as.numeric(pagenumber) < 1) 
        stop("'pagenumber' must be greater than 1")
    if (return.all == TRUE) {
        pagesize <- "65535"
        pagenumber <- "1"
    }
    batch <- function(pagenumber){
        GETiteration <- paste(GETparameters, 
                              "&PageNumber=", pagenumber, 
                              "&PageSize=", pagesize, sep = "")        
        batch <- request(operation, GETparameters = GETiteration, ...)
        if (is.null(batch$valid)) {
            return(batch)
        } else {
            return(as.data.frame.ReviewResults(xml.parsed = xmlParse(batch$xml)))
        }
    }
    request <- batch(pagenumber)
    if (return.all) {
        runningtotal <- request$AssignmentResults
        pagenumber <- 2
        while (request$AssignmentTotalResults > runningtotal) {
            nextbatch <- batch(pagenumber)
            request$AssignmentReviewResult <- rbind(request$AssignmentReviewResult, nextbatch$AssignmentReviewResult)
            request$AssignmentReviewAction <- rbind(request$AssignmentReviewAction, nextbatch$AssignmentReviewAction)
            request$HITReviewResult <- rbind(request$HITReviewResult, nextbatch$HITReviewResult)
            request$HITReviewAction <- rbind(request$HITReviewAction, nextbatch$HITReviewAction)
            runningtotal <- runningtotal + nextbatch$AssignmentTotalResults
            pagenumber <- pagenumber + 1
        }
        request$AssignmentResults <- NULL
        request$AssignmentTotalResults <- NULL
    }   
    if (verbose) {
        message("ReviewResults Retrieved: ", appendLF=FALSE)
        if (is.null(request)) {
            message("0\n")
        } else {
            message("\n")
            if ("AssignmentReviewResult" %in% names(request)) {
                message(length(request$AssignmentReviewResults), " Assignment ReviewResults Retrieved")
            }
            if ("AssignmentReviewAction" %in% names(request)) {
                message(length(request$AssignmentReviewActions), " Assignment ReviewActions Retrieved")
            }
            if ("HITReviewResult" %in% names(request)) {
                message(length(request$HITReviewResults), " HIT ReviewResults Retrieved")
            }
            if ("HITReviewAction" %in% names(request)) {
                message(length(request$HITReviewActions), " HIT ReviewActions Retrieved")
            }
            message("\n")
        }
    }
    return(request)
}
