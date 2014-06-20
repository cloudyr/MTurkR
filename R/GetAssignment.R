assignment <-
assignments <-
GetAssignment <-
GetAssignments <-
function (assignment = NULL, hit = NULL, hit.type = NULL, status = NULL, 
    return.all = FALSE, pagenumber = "1", pagesize = "10", sortproperty = "SubmitTime", 
    sortdirection = "Ascending", response.group = NULL,
    return.assignment.dataframe = TRUE,
    verbose = getOption('MTurkR.verbose'), ...) {
    # temporary check for `print` argument (remove after v1.0)
    if('print' %in% names(list(...)) && is.null(verbose))
        verbose <- list(...)$print
    if(!sortproperty %in% c("AcceptTime", "SubmitTime", "AssignmentStatus")) 
        stop("'sortproperty' must be 'AcceptTime' | 'SubmitTime' | 'AssignmentStatus'")
    if(!sortdirection %in% c("Ascending", "Descending")) 
        stop("'sortdirection' must be 'Ascending' | 'Descending'")
    if(as.numeric(pagesize) < 1 || as.numeric(pagesize) > 100) 
        stop("'pagesize' must be in range (1,100)")
    if(as.numeric(pagenumber) < 1) 
        stop("'pagenumber' must be > 1")
    GETresponsegroup <- ""
    if(!is.null(response.group)) {
        if(!is.null(assignment)) {
            if(!response.group %in% c("Request", "Minimal", 
                    "AssignmentFeedback", "HITDetail", "HITQuestion")) 
                stop("ResponseGroup must be in c(Request,Minimal,AssignmentFeedback,HITDetail,HITQuestion)")
        } else {
            if(!response.group %in% c("Request", "Minimal", "AssignmentFeedback")) 
                stop("ResponseGroup must be in c(Request,Minimal,AssignmentFeedback)")
        }
        if(length(response.group) == 1) 
            GETresponsegroup <- paste("&ResponseGroup=", response.group, sep = "")
        else {
            for(i in 1:length(response.group)){
                GETresponsegroup <- paste("&ResponseGroup", i-1,
                                          "=", response.group[i], sep = "")
            }
        }
    }
    if (!is.null(assignment)) {
        operation <- "GetAssignment"
        for(i in 1:length(assignment)) {
            GETparameters <- paste("&AssignmentId=", assignment[i], GETresponsegroup, sep = "")
            request <- request(operation, GETparameters = GETparameters, ...)
            if(is.null(request$valid))
                return(request)
            QualificationRequirements <- list()
            if(request$valid) {
                a <- as.data.frame.Assignments(xml.parsed = xmlParse(request$xml))$assignments
                a$Answer <- NULL
                if(i == 1)
                    Assignments <- a
                else
                    Assignments <- merge(Assignments, a, all=TRUE)
                if(verbose) 
                    message(i, ": Assignment ", assignment[i], " Retrieved")
            }
        }
        return(Assignments)#, HITs = HITs, 
            #QualificationRequirements = QualificationRequirements))
    } else {
        operation <- "GetAssignmentsForHIT"
        if((is.null(hit) & is.null(hit.type)) | (!is.null(hit) & !is.null(hit.type))) 
            stop("Must provide 'assignment' xor 'hit' xor 'hit.type'")
        else if(!is.null(hit)){
            if(is.factor(hit))
                hit <- as.character(hit)
            hitlist <- hit
        } else if(!is.null(hit.type)) {
            if(is.factor(hit.type))
                hit.type <- as.character(hit.type)
            hitsearch <- SearchHITs(verbose = FALSE, 
                                    return.qual.dataframe = FALSE, ...)
            hitlist <- hitsearch$HITs$HITId[hitsearch$HITs$HITTypeId %in% hit.type]
            if(length(hitlist) == 0)
                stop("No HITs found for HITType")
        }
        if(return.all == TRUE | length(hitlist)>1) {
            sortproperty <- "SubmitTime"
            sortdirection <- "Ascending"
            pagesize <- "100"
            pagenumber <- "1"
        }
        batch <- function(batchhit, pagenumber) {
            GETiteration <- ""
            if(!is.null(status)) {
                if(all(status %in% c("Approved", "Rejected", "Submitted")))
                    GETiteration <- paste(GETiteration, "&AssignmentStatus=", 
                                          paste(status,collapse=","),
                                          GETresponsegroup, sep = "")
                else
                    status <- NULL
            }
            GETiteration <- paste("&HITId=", batchhit, "&PageNumber=", 
                            pagenumber, "&PageSize=", pagesize, "&SortProperty=", 
                            sortproperty, "&SortDirection=", sortdirection, 
                            GETiteration, sep = "")
            batch <- request(operation, GETparameters = GETiteration, ...)
            batch$total <- as.numeric(strsplit(strsplit(batch$xml, 
                            "<TotalNumResults>")[[1]][2], "</TotalNumResults>")[[1]][1])
            batch$batch.total <- length(xpathApply(xmlParse(batch$xml), "//Assignment"))
            if(batch$batch.total > 0 & return.assignment.dataframe == TRUE) {
                batch$assignments <- as.data.frame.Assignments(xml.parsed = xmlParse(batch$xml))$assignments
                batch$assignments$Answer <- NULL
            } else if(batch$batch.total > 0 & return.assignment.dataframe == FALSE) 
                batch$assignments <- NULL
            else
                batch$assignments <- NA
            return(batch)
        }
        cumulative <- 0
        for(i in 1:length(hitlist)) {
            if(i == 1){
                request <- batch(hitlist[i], pagenumber)
                if(is.null(request$valid))
                    return(request)
                runningtotal <- request$batch.total
             } else{
                pagenumber <- 1
                nextrequest <- batch(hitlist[i], pagenumber)
                if(is.null(nextrequest$valid))
                    return(nextrequest)
                request$total <- request$total + nextrequest$total
                if (return.assignment.dataframe == TRUE) 
                request$assignments <- merge(request$assignments, 
                                             nextrequest$assignments, all=TRUE)
                request$pages.returned <- pagenumber
                runningtotal <- nextrequest$batch.total
            }
            if(return.all == TRUE) {
                pagenumber <- 2
                while (request$total > runningtotal) {
                    nextbatch <- batch(hitlist[i], pagenumber)
                    if(is.null(nextbatch$valid))
                        return(nextbatch)
                    if(return.assignment.dataframe == TRUE) 
                         request$assignments <- merge(request$assignments, 
                                                      nextbatch$assignments, all=TRUE)
                    request$pages.returned <- pagenumber
                    runningtotal <- runningtotal + nextbatch$batch.total
                    pagenumber <- pagenumber + 1
                }
            }
            cumulative <- cumulative + runningtotal
            request$batch.total <- NULL
            if(!is.null(hit.type))
                 request$assignments["HITTypeId"] <- hit.type
        }
        if(verbose) 
            message(cumulative, " of ", request$total, " Assignments Retrieved")
        if(is.na(request$assignments)){
            request$assignments <- 
            setNames(data.frame(matrix(nrow=0, ncol=12)),
                c("AssignmentId",
                  "WorkerId",
                  "HITId",
                  "AssignmentStatus",
                  "AutoApprovalTime",
                  "AcceptTime",
                  "SubmitTime",
                  "ApprovalTime",
                  "RejectionTime",
                  "RequesterFeedback",
                  "ApprovalRejectionTime",
                  "SecondsOnHIT"))
        }
        return(request$assignments)
    }
}
