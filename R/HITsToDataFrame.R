HITsToDataFrame <-
function (xml = NULL, xml.parsed = NULL, return.hit.xml = FALSE, 
    return.qual.list = TRUE) 
{
    if (!is.null(xml) & !is.null(xml.parsed)) 
        stop("No XML or parsed XML provided to convert to dataframe")
    if (!is.null(xml)) 
        xml.parsed <- xmlParse(xml)
    hit.xml <- xpathApply(xml.parsed, "//HIT")
    if (!is.null(length(hit.xml))) {
        quals <- list()
        HITs <- data.frame(matrix(nrow = length(hit.xml), ncol = 19))
        names(HITs) <- c("HITId", "HITTypeId", "CreationTime", 
            "Title", "Description", "Keywords", "HITStatus", 
            "MaxAssignments", "Amount", "AutoApprovalDelayInSeconds", 
            "Expiration", "AssignmentDurationInSeconds", "NumberOfSimilarHITs", 
            "HITReviewStatus", "RequesterAnnotation", "NumberofAssignmentsPending", 
            "NumberofAssignmentsAvailable", "NumberofAssignmentsCompleted", 
            "Question")
        for (i in 1:length(hit.xml)) {
            q <- xpathApply(xml.parsed, "//HIT")[[i]]
            HITs[i, 1] <- xmlValue(xmlChildren(q)$HITId)
            HITs[i, 2] <- xmlValue(xmlChildren(q)$HITTypeId)
            HITs[i, 3] <- xmlValue(xmlChildren(q)$CreationTime)
            HITs[i, 4] <- xmlValue(xmlChildren(q)$Title)
            HITs[i, 5] <- xmlValue(xmlChildren(q)$Description)
            HITs[i, 6] <- xmlValue(xmlChildren(q)$Keywords)
            HITs[i, 7] <- xmlValue(xmlChildren(q)$HITStatus)
            HITs[i, 8] <- xmlValue(xmlChildren(q)$MaxAssignments)
            HITs[i, 9] <- xmlValue(xmlChildren(xmlChildren(q)$Reward)$Amount)
            HITs[i, 10] <- xmlValue(xmlChildren(q)$AutoApprovalDelayInSeconds)
            HITs[i, 11] <- xmlValue(xmlChildren(q)$Expiration)
            HITs[i, 12] <- xmlValue(xmlChildren(q)$AssignmentDurationInSeconds)
            HITs[i, 13] <- xmlValue(xmlChildren(q)$NumberOfSimilarHITs)
            HITs[i, 14] <- xmlValue(xmlChildren(q)$HITReviewStatus)
            HITs[i, 15] <- xmlValue(xmlChildren(q)$RequesterAnnotation)
            HITs[i, 16] <- xmlValue(xmlChildren(q)$NumberOfAssignmentsPending)
            HITs[i, 17] <- xmlValue(xmlChildren(q)$NumberOfAssignmentsAvailable)
            HITs[i, 18] <- xmlValue(xmlChildren(q)$NumberOfAssignmentsCompleted)
            HITs[i, 19] <- xmlValue(xmlChildren(q)$Question)
            if (return.qual.list == TRUE) {
                quals.nodeset <- xpathApply(xml.parsed, paste("//HIT[", 
                  i, "]/QualificationRequirement", sep = ""))
                if (!is.null(quals.nodeset) && length(quals.nodeset) > 
                  0) {
                  quals[[i]] <- QualificationRequirementsToDataFrame(xmlnodeset = quals.nodeset, 
                    hit.number = i)
                }
                else quals[[i]] <- NULL
            }
        }
        if (!is.null(quals)) 
            return(list(HITs = HITs, QualificationRequirements = quals))
        else return(list(HITs = HITs))
    }
    else
		return(list(HITs = NULL))
}
