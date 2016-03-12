block <-
BlockWorker <-
BlockWorkers <-
function (workers, reasons, verbose = getOption('MTurkR.verbose', TRUE), ...){
    operation <- "BlockWorker"
    if (is.factor(workers)) {
        workers <- as.character(workers)
    }
    if (is.null(reasons)) {
        stop("Must specify one reason for block for all workers or one reason per worker")
    }
    if (is.factor(reasons)) {
        reasons <- as.character(reasons)
    }
    if (length(workers) > 1) {
        if (length(reasons) == 1) {
            reasons <- rep(reasons, length(workers))
        } else if (!length(workers) == length(reasons)) {
            stop("length(reasons) must equal length(workers) or 1")
        }
    }
    Workers <- emptydf(length(workers), 3, c("WorkerId", "Reason", "Valid"))
    for (i in 1:length(workers)) {
        GETparameters <- paste("&WorkerId=", workers[i],
                               "&Reason=", curl_escape(reasons[i]), sep = "")
        request <- request(operation, GETparameters = GETparameters, ...)
        if (is.null(request$valid)) {
            return(request)
        }
        Workers[i, ] <- c(workers[i], reasons[i], request$valid)
        if (request$valid == TRUE & verbose) {
            message(i, ": Worker ", workers[i], " Blocked")
        } else if (request$valid == FALSE & verbose) {
            warning(i,": Invalid Request for worker ",workers[i])
        }
    }
    Workers$Valid <- factor(Workers$Valid, levels=c('TRUE','FALSE'))
    return(Workers)
}
