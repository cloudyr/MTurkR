contact <-
ContactWorker <-
ContactWorkers <-
function (subjects, msgs, workers, batch = FALSE,
    verbose = getOption('MTurkR.verbose', TRUE), ...) {
    operation <- "NotifyWorkers"
    if (is.factor(subjects)) {
        subjects <- as.character(subjects)
    }
    if (is.factor(msgs)) {
        msgs <- as.character(msgs)
    }
    if (is.factor(workers)) {
        workers <- as.character(workers)
        if (length(workers) > length(unique(workers))) {
            warning("Duplicated WorkerIds removed from 'workers'")
        }
        workers <- unique(workers)
    }
    if (batch) {
        if (length(msgs) > 1) {
            stop("If 'batch'==TRUE, only one message can be used")
        } else if (nchar(curl_escape(subjects)) > 200) {
            stop("Subject Too Long (200 char max)")
        }
        if (length(subjects) > 1) {
            stop("If 'batch'==TRUE, only one subject can be used")
        } else if (nchar(curl_escape(msgs)) > 4096) {
            stop("Message Text Too Long (4096 char max)")
        }
        Notifications <- emptydf(length(workers), 4, c("WorkerId", "Subject", "Message", "Valid"))
        Notifications$WorkerId <- workers
        Notifications$Subject <- subjects
        Notifications$Message <- msgs

        workerbatch <- split(workers, rep(1:((length(workers) %/% 100) + 1), each = 100)[1:length(workers)])
        for (i in 1:length(workerbatch)) {
            GETworkers <- paste0("&WorkerId.", seq_along(workerbatch[[i]]), "=", 
                                 workerbatch[[i]], collapse = "")
            GETparameters <- paste0("&Subject=", curl_escape(subjects), 
                                   "&MessageText=", curl_escape(msgs), GETworkers)
            request <- request(operation, GETparameters = GETparameters, ...)
            if (is.null(request$valid)) {
                return(request)
            }
            Notifications$Valid[Notifications$WorkerId %in% workerbatch[[i]]] <- request$valid
            if (request$valid) {
                if (verbose) {
                    message(i, ": Workers ", workerbatch[[i]][1], " to ", tail(workerbatch[[i]],1), " Notified")
                }
                parsed <- xmlParse(request$xml)
                if (length(getNodeSet(parsed, '//NotifyWorkersFailureStatus'))>0) {
                    x <- xpathApply(parsed, '//NotifyWorkersFailureStatus', function(x) {
                        w <- xmlValue(xmlChildren(x)$WorkerId)
                        f <- xmlValue(xmlChildren(x)$NotifyWorkersFailureMessage)
                        message(paste("Invalid Request for worker ",w, ": ",f,sep=""))
                        return(c(w,f))
                    })
                    for (k in 1:length(x)) {
                        Notifications$Valid[Notifications$WorkerId==x[[k]][1]] <- 'HardFailure'
                    }
                }
            } else if (request$valid == FALSE) {
                if (verbose) {
                    warning(i,": Invalid Request for workers ", workerbatch[[i]][1], " to ", tail(workerbatch[[i]],1))
                }
            }
        }
    } else {
        for (i in 1:length(subjects)) {
            if (nchar(curl_escape(subjects[i])) > 200) {
                stop(paste("Subject ", i, " Too Long (200 char max)", sep = ""))
            }
        }
        for (i in 1:length(msgs)) {
            if (nchar(curl_escape(msgs[i])) > 4096) {
                stop(paste("Message ", i, "Text Too Long (4096 char max)", sep = ""))
            }
        }
        if (length(subjects) == 1) {
            subjects <- rep(subjects[1], length(workers))
        } else if (!length(subjects) == length(workers)) {
            stop("Number of subjects is not 1 nor length(workers)")
        }
        if (length(msgs) == 1) {
            msgs <- rep(msgs[1], length(workers))
        } else if (!length(msgs) == length(workers)) {
            stop("Number of messages is not 1 nor length(workers)")
        }
        Notifications <- emptydf(length(workers), 4, c("WorkerId", "Subject", "Message", "Valid"))
        for (i in 1:length(workers)) {
            GETparameters <- paste("&Subject=", curl_escape(subjects[i]), 
                                   "&MessageText=", curl_escape(msgs[i]),
                                   "&WorkerId.1=", workers[i], sep = "")
            request <- request(operation, GETparameters = GETparameters, ...)
            if (is.null(request$valid)) {
                return(request)
            }
            parsed <- xmlParse(request$xml)
            if (length(getNodeSet(parsed,'//NotifyWorkersFailureStatus'))>0) {
                request$valid <- xmlValue(getNodeSet(parsed,'//NotifyWorkersFailureCode')[[1]])
            }
            Notifications[i, ] <- c(workers[i], subjects[i], msgs[i], request$valid)
            if (request$valid == 'HardFailure') {
                if(verbose) {
                    message(i, ": Worker (", workers[i], ") not contacted: ",
                            xmlValue(getNodeSet(parsed,'//NotifyWorkersFailureMessage')[[1]]))
                }
            }
            if (request$valid == TRUE) {
                if (verbose) {
                    message(i, ": Worker (", workers[i], ") Notified")
                }
            } else if (request$valid == FALSE) {
                if (verbose) {
                    warning(i,": Invalid Request for worker ", workers[i])
                }
            }
        }
    }
    Notifications$Valid <- factor(Notifications$Valid, levels=c('TRUE','FALSE','HardFailure'))
    return(Notifications)
}
