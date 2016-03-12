approveall <-
ApproveAllAssignments <-
function(hit = NULL, 
         hit.type = NULL, 
         annotation = NULL,
         feedback = NULL,
         verbose = getOption('MTurkR.verbose', TRUE), ...){
    if (!is.null(feedback)) {
        if (length(feedback) > 1) {
            stop("Can only specify one feedback message; no assignments approved")
        } else if(nchar(curl_escape(feedback)) > 1024) {
            stop(paste("Feedback is too long (1024 char max); no assignments approved", sep = ""))
        }
    }
    if ((is.null(hit) & is.null(hit.type) & is.null(annotation)) | 
        (!is.null(hit) & !is.null(hit.type) & !is.null(annotation))) {
        stop("Must provide 'hit' xor 'hit.type' xor 'annotation'")
    } else if (!is.null(hit)) {
        assignments <- GetAssignments(hit = hit, 
                                      return.all = TRUE, 
                                      status = "Submitted", 
                                      verbose = verbose, ...)$AssignmentId
    } else if (!is.null(hit.type) | !is.null(annotation)) {
        hitsearch <- SearchHITs(return.qual.dataframe = FALSE, ...)
        if (!is.null(hit.type)) {
            if (is.factor(hit.type)) {
                hit.type <- as.character(hit.type)
            }
            hitlist <- hitsearch$HITs$HITId[hitsearch$HITs$HITTypeId %in% hit.type]
        } else if(!is.null(annotation)){
            if (is.factor(annotation)) {
                annotation <- as.character(annotation)
            }
            hitlist <- hitsearch$HITs$HITId[hitsearch$HITs$RequesterAnnotation %in% annotation]
        }
        if (length(hitlist) == 0) {
            stop("No HITs found for HITType")
        }
        assignments <- sapply(hitlist, function(i){
                           GetAssignments(hit = i, 
                                          return.all = TRUE, 
                                          status = "Submitted", 
                                          verbose = verbose,  ...)$AssignmentId
                       })
    }
    if (length(assignments)==0) {
        return(emptydf(0, 3, c("AssignmentId","Feedback","Valid")))
    } else {
        request <- ApproveAssignments(assignments,
                                      feedback = feedback, 
                                      verbose = verbose, ...)
        return(request)
    }
}
