GenerateReviewPolicy <-
function (hitpolicy = NULL, assignpolicy = NULL) 
{
    if (!is.null(hitpolicy)) {
        hitpolicyname <- "SimplePlurality/2011-09-01"
        hitreviewpolicy <- newXMLNode("HITReviewPolicy")
        HITPolicyName <- newXMLNode("PolicyName", hitpolicyname, 
            parent = hitreviewpolicy)
        addChildren(hitreviewpolicy, HITPolicyName)
        for (i in 1:length(hitpolicy)) {
            newnode <- newXMLNode("Parameter", parent = hitreviewpolicy)
            if (!names(hitpolicy)[i] %in% c("QuestionIds", "QuestionIdAgreementThreshold", 
                "DisregardAssignmentIfRejected", "DisregardAssignmentIfKnownAnswerScoreIsLessThan", 
                "ExtendIfHITAgreementScoreIsLessThan", "ExtendMaximumAssignments", 
                "ExtendMinimumTimeInSeconds", "ApproveIfWorkerAgreementScoreIsNotLessThan", 
                "RejectIfWorkerAgreementScoreIsLessThan", "RejectReason")) 
                stop("Inappropriate HIT ReviewPolicy Parameter hitpolicy[[", 
                  i, "]]")
            if (names(hitpolicy)[i] == "QuestionIds") {
                if (length(hitpolicy[[i]]) > 15) 
                  stop("Max number of 'QuestionIds' is 15")
                newsubnode1 <- newXMLNode("Key", "QuestionIDs", 
                  parent = newnode)
                addChildren(newnode, newsubnode1)
                for (j in 1:length(hitpolicy[[i]])) {
                  valuenode <- newXMLNode("Value", hitpolicy[[i]][j], 
                    parent = newnode)
                  addChildren(newnode, valuenode)
                }
            }
            else {
                newsubnode1 <- newXMLNode("Key", names(hitpolicy)[i], 
                  parent = newnode)
                newsubnode2 <- newXMLNode("Value", hitpolicy[[i]], 
                  parent = newnode)
            }
        }
        addChildren(hitreviewpolicy, newnode)
        hitstring <- toString.XMLNode(hitreviewpolicy)
        hitencoded <- curlEscape(hitstring)
    }
    if (!is.null(assignpolicy)) {
        assignpolicyname <- "ScoreMyKnownAnswers/2011-09-01"
        assignreviewpolicy <- newXMLNode("AssignmentReviewPolicy")
        AssignPolicyName <- newXMLNode("PolicyName", assignpolicyname, 
            parent = assignreviewpolicy)
        addChildren(assignreviewpolicy, AssignPolicyName)
        for (i in 1:length(assignpolicy)) {
            newnode <- newXMLNode("Parameter", parent = assignreviewpolicy)
            if (!names(assignpolicy)[i] %in% c("AnswerKey", "ApproveIfKnownAnswerScoreIsAtLeast", 
                "ApproveReason", "RejectIfKnownAnswerScoreIsLessThan", 
                "RejectReason", "ExtendIfKnownAnswerScoreIsLessThan", 
                "ExtendMaximumAssignments", "ExtendMinimumTimeInSeconds")) 
                stop("Inappropriate Assignment ReviewPolicy Parameter for assignpolicy[[", 
                  i, "]]")
            if (names(assignpolicy)[i] == "AnswerKey") {
                newsubnode1 <- newXMLNode("Key", "AnswerKey", 
                  parent = newnode)
                addChildren(newnode, newsubnode1)
                for (j in 1:length(assignpolicy[[i]])) {
                  newsubnode2 <- newXMLNode("MapEntry", parent = newnode)
                  newsubnode2a <- newXMLNode("Key", assignpolicy[[i]][[j]]$Key, 
                    parent = newsubnode2)
                  newsubnode2b <- newXMLNode("Value", assignpolicy[[i]][[j]]$Value, 
                    parent = newsubnode2)
                  addChildren(newsubnode2, c(newsubnode2a, newsubnode2b))
                  addChildren(newnode, c(newsubnode2))
                }
            }
            else {
                newsubnode1 <- newXMLNode("Key", names(assignpolicy)[i], 
                  parent = newnode)
                newsubnode2 <- newXMLNode("Value", assignpolicy[[i]], 
                  parent = newnode)
                addChildren(newnode, c(newsubnode1, newsubnode2))
            }
            addChildren(assignreviewpolicy, newnode)
        }
        assignstring <- toString.XMLNode(assignreviewpolicy)
        assignencoded <- curlEscape(assignstring)
    }
    if (is.null(assignpolicy) & !is.null(hitpolicy)) {
        invisible(list(HITReviewPolicy = list(xml.parsed = hitreviewpolicy, 
            string = hitstring, url.encoded = hitencoded)))
    }
    else if (!is.null(assignpolicy) & is.null(hitpolicy)) {
        invisible(list(AssignmentReviewPolicy = list(xml.parsed = assignreviewpolicy, 
            string = assignstring, url.encoded = assignencoded)))
    }
    else {
        invisible(list(HITReviewPolicy = list(xml.parsed = hitreviewpolicy, 
            string = hitstring, url.encoded = hitencoded), AssignmentReviewPolicy = list(xml.parsed = assignreviewpolicy, 
            string = assignstring, url.encoded = assignencoded)))
    }
}
