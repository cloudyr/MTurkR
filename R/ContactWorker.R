contact <-
ContactWorker <-
ContactWorkers <-
function (subjects, msgs, workers, batch = FALSE, keypair = credentials(), 
    print = FALSE, browser = FALSE, log.requests = TRUE, sandbox = FALSE,
	validation.test = FALSE) 
{
    if (!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else stop("No keypair provided or 'credentials' object not stored")
    operation <- "NotifyWorkers"
    if (batch == TRUE) {
        if (length(msgs) > 1) 
            stop("If 'batch'==TRUE, only one message can be used")
        else if (nchar(curlEscape(subjects)) > 200) 
            stop("Subject Too Long (200 char max)")
        if (length(subjects) > 1) 
            stop("If 'batch'==TRUE, only one subject can be used")
        else if (nchar(curlEscape(msgs)) > 4096) 
            stop("Message Text Too Long (4096 char max)")
        nbatches <- ceiling(length(workers)/100)
        lastbatch <- length(workers) - (100 * (nbatches - 1))
        Notifications <- data.frame(matrix(nrow = nbatches, ncol = 6))
        names(Notifications) <- c("Batch", "FirstWorkerId", "LastWorkerId", 
            "Subject", "Message", "Valid")
        i <- 1
        j <- 1
        while (j <= nbatches) {
            GETworkers <- ""
            firstworker <- ""
            lastworker <- ""
            if (j == nbatches) {
                workerbatch <- workers[i:(i + (lastbatch - 1))]
                upper <- lastbatch
            }
            else {
                workerbatch <- workers[i:(i + 99)]
                upper <- 100
            }
            for (k in 1:upper) {
                GETworkers <- paste(GETworkers, "&WorkerId.", k,
									"=", workerbatch[k], sep = "")
                if (k == 1) 
					firstworker <- workerbatch[k]
                else if (k == upper) 
					lastworker <- workerbatch[k]
            }
            GETparameters <- paste(	"&Subject=", curlEscape(subjects), 
									"&MessageText=", curlEscape(msgs), GETworkers, 
									sep = "")
            auth <- authenticate(operation, secret)
            if (browser == TRUE) {
                request <- request(keyid, auth$operation, auth$signature, 
					auth$timestamp, GETparameters, browser = browser, 
					sandbox = sandbox, validation.test = validation.test)
				if(validation.test)
					invisible(request)
            }
            else {
                request <- request(keyid, auth$operation, auth$signature, 
					auth$timestamp, GETparameters, log.requests = log.requests, 
					sandbox = sandbox, validation.test = validation.test)
				if(validation.test)
					invisible(request)
                Notifications[j, ] <- c(nbatches, firstworker, lastworker,
										subjects, msgs, request$valid)
                if (request$valid == TRUE) {
					if (print == TRUE) 
						message(j, ": Workers ", firstworker, " to ",lastworker, " Notified")
                }
                else if (request$valid == FALSE) {
					if (print == TRUE) 
						warning(j,": Invalid Request for workers ",firstworker," to ",lastworker)
                }
            }
            i <- i + 100
            j <- j + 1
        }
        if (print == TRUE) 
            return(Notifications)
        else invisible(Notifications)
    }
    else {
        for (i in 1:length(subjects)) {
            if (nchar(curlEscape(subjects[i])) > 200) 
                stop(paste("Subject ", i, " Too Long (200 char max)", sep = ""))
        }
        for (i in 1:length(msgs)) {
            if (nchar(curlEscape(msgs[i])) > 4096) 
                stop(paste("Message ", i, "Text Too Long (4096 char max)", sep = ""))
        }
        if (length(subjects) == 1) 
            subjects <- rep(subjects[1], length(workers))
        else if (!length(subjects) == length(workers)) 
            stop("Number of subjects is not 1 nor length(workers)")
        if (length(msgs) == 1) 
            msgs <- rep(msgs[1], length(workers))
        else if (!length(msgs) == length(workers)) 
            stop("Number of messages is not 1 nor length(workers)")
        Notifications <- data.frame(matrix(nrow = length(workers), ncol = 4))
        names(Notifications) <- c("WorkerId", "Subject", "Message", "Valid")
        for (i in 1:length(workers)) {
            GETparameters <- paste("&Subject=", curlEscape(subjects[i]), 
									"&MessageText=", curlEscape(msgs[i]),
									"&WorkerId.1=", workers[i], sep = "")
            auth <- authenticate(operation, secret)
            if (browser == TRUE) {
                request <- request(keyid, auth$operation, auth$signature, 
					auth$timestamp, GETparameters, browser = browser, 
					sandbox = sandbox, validation.test = validation.test)
				if(validation.test)
					invisible(request)
            }
            else {
                request <- request(keyid, auth$operation, auth$signature, 
					auth$timestamp, GETparameters, log.requests = log.requests, 
					sandbox = sandbox, validation.test = validation.test)
				if(validation.test)
					invisible(request)
                Notifications[i, ] <- c(workers[i], subjects[i], msgs[i], request$valid)
                if (request$valid == TRUE) {
					if (print == TRUE) 
						message(i, ": Worker (", workers[i], ") Notified")
                }
                else if (request$valid == FALSE) {
					if (print == TRUE) 
                    warning(i,": Invalid Request for worker ", workers[i])
                }
            }
        }
        if (print == TRUE) 
            return(Notifications)
        else
			invisible(Notifications)
    }
}
