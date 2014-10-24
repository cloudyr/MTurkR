contact <-
ContactWorker <-
ContactWorkers <-
function (subjects, msgs, workers, batch = FALSE,
    verbose = getOption('MTurkR.verbose', TRUE), ...) {
    # temporary check for `print` argument (remove after v1.0)
    if('print' %in% names(list(...)) && is.null(verbose))
        verbose <- list(...)$print
    operation <- "NotifyWorkers"
    if(is.factor(subjects))
        subjects <- as.character(subjects)
    if(is.factor(msgs))
        msgs <- as.character(msgs)
    if(is.factor(workers))
        workers <- as.character(workers)
    if(batch) {
        if(length(msgs) > 1) 
            stop("If 'batch'==TRUE, only one message can be used")
        else if(nchar(curlEscape(subjects)) > 200) 
            stop("Subject Too Long (200 char max)")
        if(length(subjects) > 1) 
            stop("If 'batch'==TRUE, only one subject can be used")
        else if(nchar(curlEscape(msgs)) > 4096) 
            stop("Message Text Too Long (4096 char max)")
        nbatches <- ceiling(length(workers)/100)
        lastbatch <- length(workers) - (100 * (nbatches - 1))
        Notifications <- setNames(data.frame(matrix(nrow = length(workers), ncol = 4)),
                            c("WorkerId", "Subject", "Message", "Valid"))
        Notifications$WorkerId <- workers
        Notifications$Subject <- subjects
        Notifications$Message <- msgs
        
        i <- 1
        j <- 1
        while(j <= nbatches) {
            GETworkers <- ""
            firstworker <- ""
            lastworker <- ""
            if(j == nbatches) {
                workerbatch <- workers[i:(i + (lastbatch - 1))]
                upper <- lastbatch
            } else {
                workerbatch <- workers[i:(i + 99)]
                upper <- 100
            }
            GETworkers <- paste0("&WorkerId.", seq_along(workerbatch), "=", 
                                 workerbatch, collapse = "")
            firstworker <- workerbatch[1]
            lastworker <- workerbatch[upper]
            GETparameters <- paste0("&Subject=", curlEscape(subjects), 
                                   "&MessageText=", curlEscape(msgs), GETworkers)
            request <- request(operation, GETparameters = GETparameters, ...)
            if(is.null(request$valid))
                return(request)
            if(j == nbatches) {
                Notifications$Valid[i:(i + (lastbatch - 1))] <- request$valid
            } else {
                Notifications$Valid[i:(i + 99)] <- request$valid
            }
            if(request$valid == TRUE) {
                if(verbose)
                    message(j, ": Workers ", firstworker, " to ",lastworker, " Notified")
                parsed <- xmlParse(request$xml)
                if(length(getNodeSet(parsed, '//NotifyWorkersFailureStatus'))>0){
                    x <- xpathApply(parsed, '//NotifyWorkersFailureStatus', function(x) {
                        w <- xmlValue(xmlChildren(x)$WorkerId)
                        f <- xmlValue(xmlChildren(x)$NotifyWorkersFailureMessage)
                        message(paste("Invalid Request for worker ",w, ": ",f,sep=""))
                        return(c(w,f))
                    })
                    for(i in 1:length(x))
                        Notifications$Valid[Notifications$Worker==x[[i]][1]] <- 'HardFailure'
                }
            } else if(request$valid == FALSE) {
                if(verbose) 
                    warning(j,": Invalid Request for workers ",firstworker," to ",lastworker)
            }
            i <- i + 100
            j <- j + 1
        }
    } else {
        for(i in 1:length(subjects)) {
            if(nchar(curlEscape(subjects[i])) > 200) 
                stop(paste("Subject ", i, " Too Long (200 char max)", sep = ""))
        }
        for(i in 1:length(msgs)) {
            if(nchar(curlEscape(msgs[i])) > 4096) 
                stop(paste("Message ", i, "Text Too Long (4096 char max)", sep = ""))
        }
        if(length(subjects) == 1) 
            subjects <- rep(subjects[1], length(workers))
        else if(!length(subjects) == length(workers)) 
            stop("Number of subjects is not 1 nor length(workers)")
        if(length(msgs) == 1) 
            msgs <- rep(msgs[1], length(workers))
        else if(!length(msgs) == length(workers)) 
            stop("Number of messages is not 1 nor length(workers)")
        Notifications <- setNames(data.frame(matrix(nrow = length(workers), ncol = 4)),
                            c("WorkerId", "Subject", "Message", "Valid"))
        for (i in 1:length(workers)) {
            GETparameters <- paste("&Subject=", curlEscape(subjects[i]), 
                                   "&MessageText=", curlEscape(msgs[i]),
                                   "&WorkerId.1=", workers[i], sep = "")
            request <- request(operation, GETparameters = GETparameters, ...)
            if(is.null(request$valid))
                return(request)
            parsed <- xmlParse(request$xml)
            if(length(getNodeSet(parsed,'//NotifyWorkersFailureStatus'))>0){
                request$valid <- xmlValue(getNodeSet(parsed,'//NotifyWorkersFailureCode')[[1]])
            }
            Notifications[i, ] <- c(workers[i], subjects[i], msgs[i], request$valid)
            if(request$valid == 'HardFailure'){
                if(verbose) 
                    message(i, ": Worker (", workers[i], ") not contacted: ",
                    xmlValue(getNodeSet(parsed,'//NotifyWorkersFailureMessage')[[1]]))
            }
            if(request$valid == TRUE) {
                if(verbose) 
                    message(i, ": Worker (", workers[i], ") Notified")
            }
            else if(request$valid == FALSE) {
                if(verbose) 
                    warning(i,": Invalid Request for worker ", workers[i])
            }
        }
    }
    Notifications$Valid <- factor(Notifications$Valid, levels=c('TRUE','FALSE','HardFailure'))
    return(Notifications)
}
