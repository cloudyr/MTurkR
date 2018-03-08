ListOperations <-
function (op = NULL) {
    ops <- c("ApproveAssignment", "ApproveRejectedAssignment", 
        "AssignQualification", "BlockWorker", "ChangeHITTypeOfHIT", 
        "CreateHIT", "CreateQualificationType", "DisableHIT", 
        "DisposeHIT", "DisposeQualificationType", "ExtendHIT", 
        "ForceExpireHIT", "GetAccountBalance", "GetAssignmentsForHIT", 
        "GetBlockedWorkers", "GetBonusPayments", 
        "GetHIT", "GetHITsForQualificationType", "GetQualificationsForQualificationType", 
        "GetQualificationRequests", "GetQualificationScore", 
        "GetQualificationType", "GetRequesterStatistic", "GetRequesterWorkerStatistic", 
        "GetReviewableHITs", "GetReviewResultsForHIT", "GrantBonus", 
        "GrantQualification", "Help", "NotifyWorkers", "RegisterHITType", 
        "RejectAssignment", "RejectQualificationRequest", "RevokeQualification", 
        "SearchHITs", "SearchQualificationTypes", "SendTestEventNotification", 
        "SetHITAsReviewing", "SetHITTypeNotification", "UnblockWorker", 
        "UpdateQualificationScore", "UpdateQualificationType")
    if (!is.null(op)) {
        return(ops[op])
    } else {
        return(ops)
    }
}
