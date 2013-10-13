AssignmentsToDataFrame <-
function (xml = NULL, xml.parsed = NULL, return.assignment.xml = FALSE) {
    if(!is.null(xml)) 
        xml.parsed <- xmlParse(xml)
    assignments.xml <- xpathApply(xml.parsed, "//Assignment")
    if(!is.null(length(assignments.xml))) {
        assignments <- data.frame(matrix(nrow=length(assignments.xml),ncol=13))
        names(assignments) <- c("HITId", "AssignmentId", "WorkerId", 
            "AssignmentStatus", "AutoApprovalTime", "AcceptTime", 
            "SubmitTime", "SecondsOnHIT", "ApprovalTime", "RejectionTime", 
            "ApprovalRejectionTime", "RequesterFeedback", "Answer")
        assignments$HITId <- xmlValue(xpathApply(xml.parsed, 
            paste("//HITId", sep = ""))[[1]])
        for(i in 1:length(assignments.xml)) {
            q <- xpathApply(xml.parsed, "//Assignment")[[i]]
            assignments$AssignmentId[i] <- xmlValue(xmlChildren(q)$AssignmentId)
            assignments$WorkerId[i] <- xmlValue(xmlChildren(q)$WorkerId)
            assignments$HITId[i] <- xmlValue(xmlChildren(q)$HITId)
            assignments$AssignmentStatus[i] <- xmlValue(xmlChildren(q)$AssignmentStatus)
            assignments$AutoApprovalTime[i] <- xmlValue(xmlChildren(q)$AutoApprovalTime)
            assignments$AcceptTime[i] <- xmlValue(xmlChildren(q)$AcceptTime)
            assignments$SubmitTime[i] <- xmlValue(xmlChildren(q)$SubmitTime)
            assignments$SecondsOnHIT[i] <- as.double(strptime(assignments$SubmitTime[i], 
                format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")) - 
                as.double(strptime(assignments$AcceptTime[i], 
                  format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
            assignments$ApprovalTime[i] <- xmlValue(xmlChildren(q)$ApprovalTime)
            assignments$ApprovalRejectionTime[i] <- xmlValue(xmlChildren(q)$ApprovalTime)
            assignments$RejectionTime[i] <- xmlValue(xmlChildren(q)$RejectionTime)
            if(!is.null(assignments$ApprovalRejectionTime[i])) 
                assignments$ApprovalRejectionTime[i] <- xmlValue(xmlChildren(q)$RejectionTime)
            assignments$RequesterFeedback[i] <- xmlValue(xmlChildren(q)$RequesterFeedback)
            assignments$Answer[i] <- xmlValue(xmlChildren(q)$Answer)
        }
	    # return answers and merge
	    answers <- QuestionFormAnswersToDataFrame(xml.parsed = xml.parsed)
	    values <- reshape(answers, timevar="QuestionIdentifier",
	                    direction="wide", idvar="AssignmentId",
				        drop=c( "WorkerId","HITId","FreeText","SelectionIdentifier",
				                "OtherSelectionField","UploadedFileKey","UploadedFileSizeInBytes"))
	    names(values) <- gsub("Combined.Answers.","",names(values),fixed=TRUE)
	    assignments <- merge(assignments,values,by="AssignmentId",all=TRUE)
	    return(list(assignments = assignments, answers = answers))
    }
    else
		return(list(assignments = NULL, answers = NULL))
}
