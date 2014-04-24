ReviewResultsToDataFrame <-
function (xml = NULL, xml.parsed = NULL) {
    if(!is.null(xml)) 
        xml.parsed <- xmlParse(xml)
    hit.xml <- xpathApply(xml.parsed, "//GetReviewResultsForHITResult")
    if(!is.null(hit.xml) && length(hit.xml) >= 1) {
        hit <- xmlValue(xpathApply(xml.parsed, "//HITId")[[1]])
        if(length(xpathApply(xml.parsed, "//AssignmentReviewPolicy")) > 0) 
            assignment.policy <- xmlValue(xpathApply(xml.parsed, 
                "//AssignmentReviewPolicy")[[1]])
        else
            assignment.policy <- NA
        if(length(xpathApply(xml.parsed, "//HITReviewPolicy")) > 0) 
            hit.policy <- xmlValue(xpathApply(xml.parsed, "//HITReviewPolicy")[[1]])
        else
            hit.policy <- NA
        if(!is.na(assignment.policy)) {
            assignment.report <- xmlChildren(xpathApply(xml.parsed, 
                "//AssignmentReviewReport")[[1]])
            if(!is.null(assignment.report) && length(assignment.report) >= 1) {
                AssignmentReviewResult <- setNames(data.frame(matrix(
                    nrow=sum(names(assignment.report) == "ReviewResult"), ncol=7)),
                    c("AssignmentReviewPolicy", "ActionId", "SubjectId",
                    "ObjectType", "QuestionId", "Key", "Value"))
                AssignmentReviewAction <- setNames(data.frame(matrix(
                    nrow = sum(names(assignment.report) == "ReviewAction"), ncol = 9)),
                    c("AssignmentReviewPolicy", "ActionId", "ActionName", "ObjectId",
                    "ObjectType", "Status", "CompleteTime", "Result", "ErrorCode"))
                r <- 1
                a <- 1
                for(i in 1:length(assignment.report)) {
                  if(xmlName(assignment.report[[i]]) == "ReviewResult") {
                    AssignmentReviewResult$AssignmentReviewPolicy[r] <- assignment.policy
                    AssignmentReviewResult$ActionId[r] <- xmlValue(xmlChildren(assignment.report[[i]])$ActionId)
                    AssignmentReviewResult$SubjectId[r] <- xmlValue(xmlChildren(assignment.report[[i]])$SubjectId)
                    AssignmentReviewResult$ObjectType[r] <- xmlValue(xmlChildren(assignment.report[[i]])$ObjectType)
                    AssignmentReviewResult$QuestionId[r] <- xmlValue(xmlChildren(assignment.report[[i]])$QuestionId)
                    AssignmentReviewResult$Key[r] <- xmlValue(xmlChildren(assignment.report[[i]])$Key)
                    AssignmentReviewResult$Value[r] <- xmlValue(xmlChildren(assignment.report[[i]])$Value)
                    r <- r + 1
                  }
                  else {
                    AssignmentReviewAction$AssignmentReviewPolicy[a] <- assignment.policy
                    AssignmentReviewAction$ActionId[a] <- xmlValue(xmlChildren(assignment.report[[i]])$ActionId)
                    AssignmentReviewAction$ActionName[a] <- xmlValue(xmlChildren(assignment.report[[i]])$ActionName)
                    AssignmentReviewAction$ObjectId[a] <- xmlValue(xmlChildren(assignment.report[[i]])$ObjectId)
                    AssignmentReviewAction$ObjectType[a] <- xmlValue(xmlChildren(assignment.report[[i]])$ObjectType)
                    AssignmentReviewAction$CompleteTime[a] <- xmlValue(xmlChildren(assignment.report[[i]])$CompleteTime)
                    AssignmentReviewAction$Status[a] <- xmlValue(xmlChildren(assignment.report[[i]])$Status)
                    AssignmentReviewAction$Result[a] <- xmlValue(xmlChildren(assignment.report[[i]])$Result)
                    AssignmentReviewAction$ErrorCode[a] <- xmlValue(xmlChildren(assignment.report[[i]])$ErrorCode)
                    a <- a + 1
                  }
                }
            }
        }
        if(!is.na(hit.policy)) {
            hit.report <- xmlChildren(xpathApply(xml.parsed, 
                "//HITReviewReport")[[1]])
            if(!is.null(hit.report) && length(hit.report) >= 1) {
                HITReviewResult <- setNames(data.frame(matrix(
                    nrow = sum(names(hit.report) == "ReviewResult"), ncol = 7)),
                    c("HITReviewPolicy", "ActionId", "SubjectId", "ObjectType",
                    "QuestionId", "Key", "Value"))
                HITReviewAction <- setNames(data.frame(matrix(
                    nrow = sum(names(hit.report) == "ReviewAction"), ncol = 9)),
                    c("HITReviewPolicy", "ActionId", "ActionName", "ObjectId",
                    "ObjectType", "Status", "CompleteTime", "Result", "ErrorCode"))
                r <- 1
                a <- 1
                for(i in 1:length(hit.report)) {
                  if(xmlName(hit.report[[i]]) == "ReviewResult") {
                    HITReviewResult$HITReviewPolicy[r] <- hit.policy
                    HITReviewResult$ActionId[r] <- xmlValue(xmlChildren(hit.report[[i]])$ActionId)
                    HITReviewResult$SubjectId[r] <- xmlValue(xmlChildren(hit.report[[i]])$SubjectId)
                    HITReviewResult$ObjectType[r] <- xmlValue(xmlChildren(hit.report[[i]])$ObjectType)
                    HITReviewResult$QuestionId[r] <- xmlValue(xmlChildren(hit.report[[i]])$QuestionId)
                    HITReviewResult$Key[r] <- xmlValue(xmlChildren(hit.report[[i]])$Key)
                    HITReviewResult$Value[r] <- xmlValue(xmlChildren(hit.report[[i]])$Value)
                    r <- r + 1
                  }
                  else {
                    HITReviewAction$HITReviewPolicy[a] <- hit.policy
                    HITReviewAction$ActionId[a] <- xmlValue(xmlChildren(hit.report[[i]])$ActionId)
                    HITReviewAction$ActionName[a] <- xmlValue(xmlChildren(hit.report[[i]])$ActionName)
                    HITReviewAction$ObjectId[a] <- xmlValue(xmlChildren(hit.report[[i]])$ObjectId)
                    HITReviewAction$ObjectType[a] <- xmlValue(xmlChildren(hit.report[[i]])$ObjectType)
                    HITReviewAction$CompleteTime[a] <- xmlValue(xmlChildren(hit.report[[i]])$CompleteTime)
                    HITReviewAction$Status[a] <- xmlValue(xmlChildren(hit.report[[i]])$Status)
                    HITReviewAction$Result[a] <- xmlValue(xmlChildren(hit.report[[i]])$Result)
                    HITReviewAction$ErrorCode[a] <- xmlValue(xmlChildren(hit.report[[i]])$ErrorCode)
                    a <- a + 1
                  }
                }
            }
        }
        if(is.na(hit.policy) & is.na(assignment.policy)) 
            return(NULL)
        else {
            return.list <- list(AssignmentReviewResult = AssignmentReviewResult, 
                AssignmentReviewAction = AssignmentReviewAction, 
                HITReviewResult = HITReviewResult, HITReviewAction = HITReviewAction)
            return(return.list)
        }
    }
    else
        return(list(AssignmentReviewResult = NULL, AssignmentReviewAction = NULL, 
                HITReviewResult = NULL, HITReviewAction = NULL))
}
