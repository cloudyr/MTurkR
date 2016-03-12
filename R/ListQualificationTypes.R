ListQualificationTypes <-
function (qual = NULL) {
    quals <- setNames(as.data.frame(rbind(
        #c("Worker_PercentAssignmentsSubmitted", "00000000000000000000"),
        #c("Worker_PercentAssignmentsAbandoned", "00000000000000000070"),
        #c("Worker_PercentAssignmentsReturned", "000000000000000000E0"),
        c("Worker_PercentAssignmentsApproved", "000000000000000000L0"),
        #c("Worker_PercentAssignmentsRejected", "000000000000000000S0"),
        c("Worker_NumberHITsApproved", "00000000000000000040"),
        c("Worker_Locale", "00000000000000000071"), 
        c("Worker_Adult", "00000000000000000060"),
        c("Masters (Sandbox)","2ARFPLSP75KLA8M8DH1HTEQVJT3SY6"),
        c("Masters (Production)","2F1QJWKUDD8XADTFD2Q0G6UTO95ALH")
        ), stringsAsFactors = FALSE), c("Qualification", "QualificationTypeId"))
    if (!is.null(qual)) {
        return(as.character(quals[quals$Qualification == qual, 2]))
    } else {
        return(quals)
    }
}
