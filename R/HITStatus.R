HITStatus <-
status <-
function (hit = NULL, 
          hit.type = NULL, 
          annotation = NULL,
          verbose = getOption('MTurkR.verbose', TRUE), 
          ...){
    hitsearch <- SearchHITs(verbose = TRUE, 
                            return.all = TRUE,
                            return.qual.dataframe = FALSE, ...)
    HITs <- hitsearch$HITs
    if (is.null(HITs)) {
        return(HITs) # return if NULL
    }
    if (!is.null(hit)) {
        if (is.factor(hit)) {
            hit <- as.character(hit)
        }
        HITs <- HITs[grep(hit, HITs$HITId), ]
        toprint <- HITs[, c("HITId", "HITReviewStatus", "NumberOfAssignmentsPending", 
                            "NumberOfAssignmentsAvailable", "NumberOfAssignmentsCompleted", 
                            "Expiration", "RequesterAnnotation")]
        toprint$RequesterAnnotation <- ifelse(is.na(toprint$RequesterAnnotation), "", toprint$RequesterAnnotation)
        names(toprint) <- c("HITId", "ReviewStatus", "Pending", 
                            "Available", "Completed", "Expiration", "Annotation")
    } else {
        if (!is.null(hit.type)) {
            if (is.factor(hit.type)) {
                hit <- as.character(hit.type)
            }
            HITs <- HITs[HITs$HITTypeId %in% hit.type, ]
        } else if (!is.null(annotation)) {
            if (is.factor(annotation)) {
                hit <- as.character(annotation)
            }
            HITs <- HITs[HITs$RequesterAnnotation %in% annotation, ]
        }
        if (dim(HITs)[1] == 0) {
            warning("No HITs found!")
            toprint <- data.frame(HITId = character(),
                                  ReviewStatus = character(),
                                  Pending = character(),
                                  Available = character(),
                                  Completed = character(),
                                  Expiration = character(), stringsAsFactors = FALSE)
            return(invisible(toprint))
        }
        toprint <- HITs[,c("HITId","HITReviewStatus","NumberOfAssignmentsPending",
                           "NumberOfAssignmentsAvailable", "NumberOfAssignmentsCompleted",
                           "Expiration", "RequesterAnnotation")]
        toprint$RequesterAnnotation <- ifelse(is.na(toprint$RequesterAnnotation), "", toprint$RequesterAnnotation)
        if (dim(HITs)[1] > 1) {
            totals <- data.frame(HITId = c("------------------------------", "Totals"),
                                 HITReviewStatus = c("------------",""),
                                 NumberOfAssignmentsPending = c("-------",
                                     sum(as.numeric(HITs$NumberOfAssignmentsPending))),
                                 NumberOfAssignmentsAvailable = c("---------",
                                     sum(as.numeric(HITs$NumberOfAssignmentsAvailable))),
                                 NumberOfAssignmentsCompleted = c("---------",
                                     sum(as.numeric(HITs$NumberOfAssignmentsCompleted))),
                                 Expiration = c("--------------------",""),
                                 RequesterAnnotation = c("", ""))
            toprint <- rbind(toprint,totals)
        }
        toprint <- setNames(toprint, c("HIT", "ReviewStatus", "Pending", "Available", 
                                       "Completed", "Expiration", "Annotation"))
    }
    if (all(toprint$Annotation == "")) {
        toprint <- toprint[, -length(toprint)]
    }
    print(toprint, row.names = FALSE)
    return(invisible(HITs))
}
