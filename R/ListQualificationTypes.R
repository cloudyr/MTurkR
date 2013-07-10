ListQualificationTypes <-
function (qual = NULL) 
{
    quals <- as.data.frame(rbind(
        #c("Worker_PercentAssignmentsSubmitted", "00000000000000000000"),
        #c("Worker_PercentAssignmentsAbandoned", "00000000000000000070"),
        #c("Worker_PercentAssignmentsReturned", "000000000000000000E0"),
        c("Worker_PercentAssignmentsApproved", "000000000000000000L0"),
        #c("Worker_PercentAssignmentsRejected", "000000000000000000S0"),
        c("Worker_NumberHITsApproved", "00000000000000000040"),
        c("Worker_Locale", "00000000000000000071"), 
        c("Worker_Adult", "00000000000000000060"),
        c("Categorization Masters (Sandbox)", "2F1KVCNHMVHV8E9PBUB2A4J79LU20F"),
        c("Categorization Masters (Production)", "2NDP2L92HECWY8NS8H3CK0CP5L9GHO"),
        c("Photo Moderation Masters (Sandbox)", "2TGBB6BFMFFOM08IBMAFGGESC1UWJX"),
        c("Photo Moderation Masters (Production)", "21VZU98JHSTLZ5BPP4A9NOBJEK3DPG")
        ), stringsAsFactors = FALSE)
    names(quals) <- c("Qualification", "QualificationTypeId")
    if (!is.null(qual)) 
        return(as.character(quals[quals$Qualification == qual, 2]))
    else
        return(quals)
}
