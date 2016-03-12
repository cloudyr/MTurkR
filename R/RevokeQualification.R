RevokeQualification <-
RevokeQualifications <-
revokequal <-
function (qual, worker, reason = NULL, verbose = getOption('MTurkR.verbose', TRUE), ...){
    operation <- "RevokeQualification"
    if (is.factor(qual)) {
        qual <- as.character(qual)
    }
    if (is.factor(worker)) {
        worker <- as.character(worker)
    }
    if (!is.null(reason) && length(reason) > 1) {
        stop("Reason must be NULL or length==1; other configurations not currently supported")
    }
    batch <- function(qualbatch, workerbatch, reasonbatch) {
        GETparameters <- paste("&QualificationTypeId=", qualbatch, 
            "&SubjectId=", workerbatch, sep = "")
        if (!is.null(reason)) {
            GETparameters <- paste(GETparameters, "&SendNotification=", 
                                   curl_escape(reasonbatch), sep = "")
        }
        request <- request(operation, GETparameters = GETparameters, ...)
        if (is.null(request$valid)) {
            return(request)
        }
        return(request)
    }
    Qualifications <- emptydf(0, 4, c("WorkerId", "QualificationTypeId", "Reason", "Valid"))
    if (length(qual) == 1 & length(worker) == 1) {
        x <- batch(qual[1], worker[1], reason)
        Qualifications[1, ] <- c(worker[1], qual[1], if(!is.null(reason)) reason else "", x$valid)
    } else if (length(qual) > 1 & length(worker) == 1) {
        for (i in 1:length(qual)) {
            x <- batch(qual[i], worker[1], reason)
            if (x$valid & verbose) {
                message(i, ": Qualification (", qual[i], ") for worker ", worker[1], " Revoked")
            } else if (!x$valid & verbose) {
                warning(i, ": Invalid Request for worker ", worker[1])
            }
            Qualifications[i, ] <- c(worker[1], qual[i], if(!is.null(reason)) reason else "", x$valid)
        }
    } else if (length(qual) == 1 & length(worker) > 1) {
        for (i in 1:length(worker)) {
            x <- batch(qual[1], worker[i], reason)
            Qualifications[i, ] <- c(worker[i], qual[1], if(!is.null(reason)) reason else "", x$valid)
        }
    } else if (length(qual) > 1 & length(worker) > 1) {
        for (i in 1:length(worker)) {
            for (j in 1:length(qual)) {
                x <- batch(qual[j], worker[i], reason)
                Qualifications[i, ] <- c(worker[i], qual[j], if(!is.null(reason)) reason else "", x$valid)
            }
        }
    }
    Qualifications$Valid <- factor(Qualifications$Valid, levels=c('TRUE','FALSE'))
    return(Qualifications)
}
