contact <-
ContactWorker <-
ContactWorkers <-
function (subjects, msgs, workers, batch = FALSE, keypair = credentials(), 
    print = getOption('MTurkR.print'), browser = getOption('MTurkR.browser'),
    log.requests = getOption('MTurkR.log'), sandbox = getOption('MTurkR.sandbox'),
    validation.test = getOption('MTurkR.test')) {
    if(!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else
        stop("No keypair provided or 'credentials' object not stored")
    operation <- "NotifyWorkers"
    if(is.factor(subjects))
        subjects <- as.character(subjects)
    if(is.factor(msgs))
        msgs <- as.character(msgs)
    if(is.factor(workers))
        workers <- as.character(workers)
    if(batch == TRUE) {
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
        #Notifications <- setNames(data.frame(matrix(nrow = nbatches, ncol = 6)),
        #                    c("Batch", "FirstWorkerId", "LastWorkerId", 
        #                    "Subject", "Message", "Valid"))
        i <- 1
        j <- 1
        while(j <= nbatches) {
            GETworkers <- ""
            firstworker <- ""
            lastworker <- ""
            if(j == nbatches) {
                workerbatch <- workers[i:(i + (lastbatch - 1))]
                upper <- lastbatch
            }
            else {
                workerbatch <- workers[i:(i + 99)]
                upper <- 100
            }
            for(k in 1:upper) {
                GETworkers <- paste(GETworkers, "&WorkerId.", k,
                                    "=", workerbatch[k], sep = "")
                if(k == 1) 
                    firstworker <- workerbatch[k]
                else if(k == upper) 
                    lastworker <- workerbatch[k]
            }
            GETparameters <- paste(    "&Subject=", curlEscape(subjects), 
                                    "&MessageText=", curlEscape(msgs), GETworkers, 
                                    sep = "")
            auth <- authenticate(operation, secret)
            if(browser == TRUE) {
                request <- request(keyid, auth$operation, auth$signature, 
                    auth$timestamp, GETparameters, browser = browser, 
                    sandbox = sandbox, validation.test = validation.test)
                if(validation.test)
                    return(invisible(request))
            }
            else {
                request <- request(keyid, auth$operation, auth$signature, 
                    auth$timestamp, GETparameters, log.requests = log.requests, 
                    sandbox = sandbox, xml.parse=TRUE, validation.test = validation.test)
                if(validation.test)
                    return(invisible(request))
                Notifications$Valid[i:(i + (lastbatch - 1))] <- request$valid
                #Notifications[j, ] <- c(nbatches, firstworker, lastworker,
                #                        subjects, msgs, request$valid)
                if(request$valid == TRUE) {
                    if(print == TRUE)
                        message(j, ": Workers ", firstworker, " to ",lastworker, " Notified")
                    parsed <- request$xml.parsed
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
                }
                else if(request$valid == FALSE) {
                    if(print == TRUE) 
                        warning(j,": Invalid Request for workers ",firstworker," to ",lastworker)
                }
            }
            i <- i + 100
            j <- j + 1
        }
    }
    else {
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
            auth <- authenticate(operation, secret)
            if(browser == TRUE) {
                request <- request(keyid, auth$operation, auth$signature, 
                    auth$timestamp, GETparameters, browser = browser, 
                    sandbox = sandbox, validation.test = validation.test)
                if(validation.test)
                    return(invisible(request))
            }
            else {
                request <- request(keyid, auth$operation, auth$signature, 
                    auth$timestamp, GETparameters, log.requests = log.requests, 
                    sandbox = sandbox, xml.parse=TRUE, validation.test = validation.test)
                if(validation.test)
                    return(invisible(request))
                parsed <- request$xml.parsed
                if(length(getNodeSet(parsed,'//NotifyWorkersFailureStatus'))>0){
                    request$valid <- xmlValue(getNodeSet(parsed,'//NotifyWorkersFailureCode')[[1]])
                }
                Notifications[i, ] <- c(workers[i], subjects[i], msgs[i], request$valid)
                if(request$valid == 'HardFailure'){
                    if(print == TRUE) 
                        message(i, ": Worker (", workers[i], ") not contacted: ",
                        xmlValue(getNodeSet(parsed,'//NotifyWorkersFailureMessage')[[1]]))
                }
                if(request$valid == TRUE) {
                    if(print == TRUE) 
                        message(i, ": Worker (", workers[i], ") Notified")
                }
                else if(request$valid == FALSE) {
                    if(print == TRUE) 
                        warning(i,": Invalid Request for worker ", workers[i])
                }
            }
        }
    }
    Notifications$Valid <- factor(Notifications$Valid, levels=c('TRUE','FALSE','HardFailure'))
    return(Notifications)
}
