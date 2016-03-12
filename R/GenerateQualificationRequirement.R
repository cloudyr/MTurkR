GenerateQualificationRequirement <-
function (qual, comparator, value, preview = NULL, qual.number = NULL) {
    if (is.null(qual)) {
        stop("No QualificationTypeId specified")
    }
    if (is.null(qual.number)) {
        qual.number <- seq_along(qual)
    }
    if (is.null(comparator)) {
        stop("No comparator specified")
    } else if (!length(qual) == length(comparator)) {
        stop("length(QualificationTypeId) != length(comparator)")
    }
    if (is.null(value)) {
        stop("No value specified")
    } else {
        if (!length(qual) == length(value)) {
            stop("length(QualificationTypeId) != length(value)")
        }
        if (!length(value) == length(comparator)) {
            stop("length(value) != length(comparator)")
        }
    }
    if (!is.null(preview) && !length(preview) == length(qual)) {
        preview <- rep(preview[1], length(qual))
    } else if (is.null(preview)) {
        preview <- rep(NA, length(qual))
    }
    qual <- sapply(as.character(qual), .AliasToQualificationType)
    if (any(qual %in% c("2ARFPLSP75KLA8M8DH1HTEQVJT3SY6"))) {
        message("A QualificationTypeId for a Sandbox Qualification has been used.")
    }
    for (i in seq_along(qual.number)) {
        if (comparator[i] == "<") {
            comparator[i] <- "LessThan"
        } else if(comparator[i] == "<=") {
            comparator[i] <- "LessThanOrEqualTo"
        } else if(comparator[i] == ">") {
            comparator[i] <- "GreaterThan"
        } else if(comparator[i] == ">=") {
            comparator[i] <- "GreaterThanOrEqualTo"
        } else if(comparator[i] %in% c("=","==")) {
            comparator[i] <- "EqualTo"
        } else if(comparator[i] == "!=") {
            comparator[i] <- "NotEqualTo"
        }
        if (!comparator[i] %in% c("LessThan", "LessThanOrEqualTo", 
                "GreaterThan", "GreaterThanOrEqualTo", "EqualTo", 
                "NotEqualTo", "Exists", "DoesNotExist", "In", "NotIn")) {
            stop("Inappropriate comparator specified for QualificationRequirement")
        }
        if (qual[i] == "00000000000000000071" & !comparator[i] %in% c("EqualTo", "NotEqualTo", "In", "NotIn")) {
            stop("Worker_Locale (00000000000000000071) Requirement can only be used with 'EqualTo', 'NotEqualTo', 'In', or 'NotIn' comparators")
        }
        # replace removed sandbox masters qualifications
        if (qual[i] %in% c("2F1KVCNHMVHV8E9PBUB2A4J79LU20F", "2TGBB6BFMFFOM08IBMAFGGESC1UWJX")) {
            warning("Categorization/Moderation Masters Qualifications have been removed.\nUsing generic Masters Qualification instead.")
            qual[i] <- "2ARFPLSP75KLA8M8DH1HTEQVJT3SY6"
        }
        # replace deprecated production masters qualifications
        if (qual[i] %in% c("2NDP2L92HECWY8NS8H3CK0CP5L9GHO", "21VZU98JHSTLZ5BPP4A9NOBJEK3DPG")) {
            warning("Categorization/Moderation Masters Qualifications have been removed.\nUsing generic Masters Qualification instead.")
            qual[i] <- "2F1QJWKUDD8XADTFD2Q0G6UTO95ALH"
        }
        if (qual[i] %in% c("2ARFPLSP75KLA8M8DH1HTEQVJT3SY6", "2F1QJWKUDD8XADTFD2Q0G6UTO95ALH") && 
                        (!comparator %in% c("Exists","DoesNotExist"))) {
            stop("Masters qualifications can only accept 'Exists' comparator")
        }
        if (comparator[i] %in% c("Exists","DoesNotExist") & !is.null(value[i])) {
            value[i] <- ""
        }
        if (!is.null(preview)) {
            if (!is.na(preview[i])) {
                if (preview[i] %in% c(TRUE, "true", "True", "1", 1)) {
                    preview[i] <- 1
                } else if (preview[i] %in% c(FALSE, "false", "False", "0", 0)) {
                    preview[i] <- 0
                }
            }
        }
    }
    
    # handle multiple LocaleValue
    ltmp <- unname(mapply(function(x = NULL, qn) { 
        v <- strsplit(x,',')[[1]]
        paste0('QualificationRequirement.',qn,'.LocaleValue.',seq_along(v),
               '.Country', '=', regmatches(v, gregexpr("^[[:alpha:]]{2}", v)), 
                      # paste in subdivisions, if present
                      ifelse(grepl("-", v), 
                             paste0('&QualificationRequirement.',qn,'.LocaleValue.',seq_along(v),
                                    '.Subdivision=', 
                                    regmatches(v, regexpr("(?<=[-])[[:alpha:]]{2}$", v, perl=TRUE))), 
                             ""), 
               collapse='&')
    }, value, qual.number))
    # handle multiple IntegerValue
    itmp <- unname(mapply(function(x = NULL, qn) { 
        v <- strsplit(x,',')[[1]]
        paste0('QualificationRequirement.',qn,'.IntegerValue.', seq_along(v),'=', v, collapse='&')
    }, value, qual.number))
    
    out <- 
    paste(paste("&QualificationRequirement.", qual.number, 
            ".QualificationTypeId=", qual, "&QualificationRequirement.", 
            qual.number, ".Comparator=", comparator, sep = ""),
            ifelse(comparator %in% c("Exists","DoesNotExist"), "",
          ifelse(qual == "00000000000000000071", paste0("&", ltmp),
                                                 paste0("&", itmp))),
          ifelse(!is.na(preview), paste("&QualificationRequirement.", qual.number, 
                                        ".RequiredToPreview=", preview, sep=""), ""),
          sep = "")
    structure(paste(out, collapse =""), class = "QualificationRequirement")
}

.AliasToQualificationType <- function(qual){
    if (qual %in% c("PercentAssignmentsApproved","Approved")) {
        qual <- "000000000000000000L0"
    } else if (qual %in% c("NumberHITsApproved","NumberApproved","HITs")) {
        qual <- "00000000000000000040"
    } else if (qual %in% c("Locale","Country","Location")) {
        qual <- "00000000000000000071"
    } else if (qual == "Adult") {
        qual <- "00000000000000000060"
    } else if (qual %in% c("Categorization",
                        "Categorization Masters",
                        "CategorizationMasters")) {
        qual <- "2F1QJWKUDD8XADTFD2Q0G6UTO95ALH"
    } else if (qual %in% c("Photo Moderation",
                        "Photo Moderation Masters",
                        "PhotoModerationMasters")) {
        qual <- "2F1QJWKUDD8XADTFD2Q0G6UTO95ALH"
    } else if (qual %in% c("Masters","MTurkMasters")) {
        qual <- "2F1QJWKUDD8XADTFD2Q0G6UTO95ALH"
    }
    return(qual)
}
