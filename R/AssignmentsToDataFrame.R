AssignmentsToDataFrame <-
function (xml = NULL, xml.parsed = NULL, return.assignment.xml = FALSE) {
    if(!is.null(xml)) 
        xml.parsed <- xmlParse(xml)
    assignments <- xpathApply(xml.parsed, "//Assignment", function(x){
        children <- xmlChildren(x)
        return(list(
            AssignmentId = xmlValue(children$AssignmentId),
            WorkerId = xmlValue(children$WorkerId),
            HITId = xmlValue(children$HITId),
            AssignmentStatus = xmlValue(children$AssignmentStatus),
            AutoApprovalTime = xmlValue(children$AutoApprovalTime),
            AcceptTime = xmlValue(children$AcceptTime),
            SubmitTime = xmlValue(children$SubmitTime),
            ApprovalTime = xmlValue(children$ApprovalTime),
            RejectionTime = xmlValue(children$RejectionTime),
            RequesterFeedback = xmlValue(children$RequesterFeedback),
            Answer = xmlValue(children$Answer)
        ))
    })
    assignments <- do.call(rbind.data.frame, assignments)
    assignments$HITId <- xmlValue(xpathApply(xml.parsed, 
            paste("//HITId", sep = ""))[[1]])
    assignments$ApprovalRejectionTime <-
        ifelse(!is.na(assignments$ApprovalTime),
            assignments$ApprovalTime, assignments$RejectionTime)
    assignments$SecondsOnHIT <- as.double(strptime(assignments$SubmitTime, 
            format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")) - 
            as.double(strptime(assignments$AcceptTime, 
              format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))

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
