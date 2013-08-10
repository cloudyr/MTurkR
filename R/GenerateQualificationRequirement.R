GenerateQualificationRequirement <-
function (qual, comparator, value, preview = NULL, qual.number = NULL, 
    format = "REST") 
{
    if (is.null(qual)) 
        stop("No QualificationTypeId specified")
    else if (length(qual) == 1) {
        if (!is.null(qual.number) && length(qual.number) > 1) 
            stop("length(QualificationTypeId) != length(qual.number)")
        else if (!is.null(qual.number) && as.numeric(qual.number) > 
            10) 
            stop("qual.number must be <= 10")
    }
    else if (length(qual) > 1) {
        qual.number <- NULL
    }
    if (is.null(comparator)) 
        stop("No comparator specified")
    else if (!length(qual) == length(comparator)) 
        stop("length(QualificationTypeId) != length(comparator)")
    if (is.null(value)) 
        stop("No value specified")
    else {
        if (!length(qual) == length(value)) 
            stop("length(QualificationTypeId) != length(value)")
        if (!length(value) == length(comparator)) 
            stop("length(value) != length(comparator)")
    }
    if (!is.null(preview) && !length(preview) == length(qual)) 
        preview <- rep(preview[1], length(qual))
    else if (is.null(preview)) 
        preview <- rep(NA, length(qual))
    x <- ""
    for (i in 1:length(qual)) {
        if (length(qual) > 1) 
            qual.number <- i
        else if (is.null(qual.number)) 
            qual.number <- 1
        #if (qual[i] == "PercentAssignmentsSubmitted" | qual[i] == "Submitted") 
        #    qual[i] <- "00000000000000000000"
        #else if (qual[i] == "PercentAssignmentsAbandoned" | qual[i] == "Abandoned") 
        #    qual[i] <- "00000000000000000070"
        #else if (qual[i] == "PercentAssignmentsReturned" | qual[i] == "Returned") 
        #    qual[i] <- "000000000000000000E0"
        #else if (qual[i] == "PercentAssignmentsRejected" | qual[i] == "Rejected") 
        #    qual[i] <- "000000000000000000S0"
        if (qual[i] == "PercentAssignmentsApproved" | qual[i] == "Approved") 
            qual[i] <- "000000000000000000L0"
        else if (qual[i] == "NumberHITsApproved" | qual[i] == 
            "NumberApproved" | qual[i] == "HITs") 
            qual[i] <- "00000000000000000040"
        else if (qual[i] == "Locale" | qual[i] == "Country" | qual[i] == "Location") 
            qual[i] <- "00000000000000000071"
        else if (qual[i] == "Adult") 
            qual[i] <- "00000000000000000060"
        else if (qual[i] == "Categorization" | qual[i] == "Categorization Masters" | 
            qual[i] == "CategorizationMasters") 
            qual[i] <- "2NDP2L92HECWY8NS8H3CK0CP5L9GHO"
        else if (qual[i] == "Photo Moderation" | qual[i] == "Photo Moderation Masters" | 
            qual[i] == "PhotoModerationMasters") 
            qual[i] <- "21VZU98JHSTLZ5BPP4A9NOBJEK3DPG"
        else if (qual[i] == "Masters" | qual[i] == "MTurkMasters") 
            qual[i] <- "2F1QJWKUDD8XADTFD2Q0G6UTO95ALH"
        if (qual[i] %in% c("2ARFPLSP75KLA8M8DH1HTEQVJT3SY6", 
            "2F1KVCNHMVHV8E9PBUB2A4J79LU20F", "2TGBB6BFMFFOM08IBMAFGGESC1UWJX")) 
            warning("QualificationTypeIds for Sandbox used")
        if (comparator[i] == "<") 
            comparator[i] <- "LessThan"
        else if (comparator[i] == "<=") 
            comparator[i] <- "LessThanOrEqualTo"
        else if (comparator[i] == ">") 
            comparator[i] <- "GreaterThan"
        else if (comparator[i] == ">=") 
            comparator[i] <- "GreaterThanOrEqualTo"
        else if (comparator[i] == "=" | comparator[i] == "==") 
            comparator[i] <- "EqualTo"
        else if (comparator[i] == "!=") 
            comparator[i] <- "NotEqualTo"
        if (!comparator[i] %in% c("LessThan", "LessThanOrEqualTo", 
            "GreaterThan", "GreaterThanOrEqualTo", "EqualTo", 
            "NotEqualTo", "Exists")) 
            stop("Inappropriate comparator specified for QualificationRequirement")
        if (qual[i] == "00000000000000000071" & !comparator[i] %in% 
            c("EqualTo", "NotEqualTo")) 
            stop("Worker_Locale (00000000000000000071) Requirement can only be used with 'EqualTo' or 'NotEqualTo' comparators")
        if (qual[i] %in% c("2NDP2L92HECWY8NS8H3CK0CP5L9GHO", 
            "21VZU98JHSTLZ5BPP4A9NOBJEK3DPG", "2F1QJWKUDD8XADTFD2Q0G6UTO95ALH") && 
            !comparator == "Exists") 
            stop("Masters qualifications can only accept 'Exists' comparator")
        if (comparator[i] == "Exists" & !is.null(value[i])) 
            value[i] <- NULL
        if (!is.null(preview)) {
            if (!is.na(preview[i])) {
                if (preview[i] %in% c(TRUE, "true", "True", "1", 
                  1)) 
                  preview[i] <- 1
                else if (preview[i] %in% c(FALSE, "false", "False", 
                  "0", 0)) 
                  preview[i] <- 0
            }
        }
        if (format == "get" | format == "Get" | format == "GET" | 
            format == "rest" | format == "REST") {
            x <- paste(x, "&QualificationRequirement.", qual.number, 
                ".QualificationTypeId=", qual[i], "&QualificationRequirement.", 
                qual.number, ".Comparator=", comparator[i], sep = "")
            if (qual[i] == "00000000000000000071") 
                x <- paste(x, "&QualificationRequirement.", qual.number, 
                  ".LocaleValue.Country=", value[i], sep = "")
            if (!qual[i] == "00000000000000000071") 
                x <- paste(x, "&QualificationRequirement.", qual.number, 
                  ".IntegerValue=", value[i], sep = "")
            if (!is.na(preview[i])) 
                x <- paste(x, "&QualificationRequirement.", qual.number, 
                  ".RequiredToPreview=", preview[i], sep = "")
        }
        else if (format == "xml" | format == "Xml" | format == 
            "XML" | format == "soap" | format == "SOAP") {
            stop("Only REST/GET requests currently supported")
        }
        else {
            stop("Inapropraite 'format' requested; only REST/GET requests currently supported")
        }
    }
    return(x)
}
