AssignmentsToDataFrame <-
function (xml = NULL, xml.parsed = NULL, return.assignment.xml = FALSE) 
{
    if (!is.null(xml)) 
        xml.parsed <- xmlParse(xml)
    assignments.xml <- xpathApply(xml.parsed, "//Assignment")
    if (!is.null(length(assignments.xml))) {
        assignments <- data.frame(matrix(nrow = length(assignments.xml), 
            ncol = 13))
        names(assignments) <- c("HITId", "AssignmentId", "WorkerId", 
            "AssignmentStatus", "AutoApprovalTime", "AcceptTime", 
            "SubmitTime", "SecondsOnHIT", "ApprovalTime", "RejectionTime", 
            "ApprovalRejectionTime", "RequesterFeedback", "Answer")
        assignments$HITId <- xmlValue(xpathApply(xml.parsed, 
            paste("//HITId", sep = ""))[[1]])
        for (i in 1:length(assignments.xml)) {
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
            if (!is.null(assignments$ApprovalRejectionTime[i])) 
                assignments$ApprovalRejectionTime[i] <- xmlValue(xmlChildren(q)$RejectionTime)
            assignments$RequesterFeedback[i] <- xmlValue(xmlChildren(q)$RequesterFeedback)
            assignments$Answer[i] <- xmlValue(xmlChildren(q)$Answer)
        }
        answers <- QuestionFormAnswersToDataFrame(xml.parsed = xml.parsed)
        vars.returned <- answers[answers$AssignmentId == assignments$AssignmentId[1], 
            "QuestionIdentifier"]
        n.returned <- length(vars.returned)
        values <- data.frame(matrix(nrow = dim(assignments)[1], 
            ncol = n.returned + 1))
        names(values) <- c(vars.returned, "AssignmentIdfromAnswers")
        for (j in 1:n.returned) {
            for (k in 1:dim(assignments)[1]) {
                values$AssignmentIdfromAnswers[k] <-
					answers[answers$AssignmentId == assignments$AssignmentId[k], "AssignmentId"][1]
                values[k, j] <- 
					answers[(answers$AssignmentId == assignments$AssignmentId[k] & 
					answers$QuestionIdentifier == vars.returned[j]), ]$Combined.Answers
            }
        }
        assignments <- cbind(assignments, values)
        return(list(assignments = assignments, answers = answers))
    }
    else
		return(list(assignments = NULL, answers = NULL))
}
