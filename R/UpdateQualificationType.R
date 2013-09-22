UpdateQualificationType <-
updatequal <-
function (qual, description = NULL, status = NULL, retry.delay = NULL, 
    test = NULL, answerkey = NULL, test.duration = NULL, auto = NULL, 
    auto.value = NULL, keypair = credentials(), print = TRUE, 
    browser = FALSE, log.requests = TRUE, sandbox = getOption('MTurkR.sandbox'),
    validation.test = FALSE) 
{
    if (!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else stop("No keypair provided or 'credentials' object not stored")
    operation <- "UpdateQualificationType"
    GETparameters <- paste("&QualificationTypeId=", qual, sep = "")
    if (!is.null(description)) 
        GETparameters <- paste(GETparameters, "&Description=", curlEscape(description), sep = "")
    if (!is.null(status)) 
        GETparameters <- paste(GETparameters, "&QualificationTypeStatus=", status, sep = "")
    if (!is.null(test)) 
        GETparameters <- paste(GETparameters, "&Test=", test, "&TestDurationInSeconds=", test.duration, sep = "")
    if (!is.null(retry.delay)) 
        GETparameters <- paste(GETparameters, "&RetryDelayInSeconds=", retry.delay, sep = "")
    if (!is.null(answerkey)) 
        GETparameters <- paste(GETparameters, "&AnswerKey=", answerkey, sep = "")
    if (!is.null(auto.value)) {
        if (!is.numeric(as.numeric(auto.value))) 
            stop("AutoGrantedValue must be numeric or coercable to numeric")
    }
    if (is.null(auto) & !is.null(auto.value)) 
        GETparameters <- paste(GETparameters, "&AutoGrantedValue=", auto.value, sep = "")
    else if (!is.null(auto) && auto == TRUE) {
        GETparameters <- paste(GETparameters, "&AutoGranted=", "1", sep = "")
        if (is.null(test) & !is.null(auto.value)) 
            GETparameters <- paste(GETparameters, "&AutoGrantedValue=", auto.value, sep = "")
    }
    else if (!is.null(auto) && auto == FALSE) {
        GETparameters <- paste(GETparameters, "&AutoGranted=", "0", sep = "")
        if (is.null(test) & !is.null(auto.value)) 
            GETparameters <- paste(GETparameters, "&AutoGrantedValue=", auto.value, sep = "")
    }
    else if (!is.null(auto) && auto == TRUE & is.null(test) & 
        is.null(auto.value)) 
        GETparameters <- paste(GETparameters, "&AutoGranted=", "1", sep = "")
    else if (!is.null(auto) && auto == FALSE & is.null(test) & 
        is.null(auto.value)) 
        GETparameters <- paste(GETparameters, "&AutoGranted=", "0", sep = "")
    else if (!is.null(auto) && !is.null(test)) 
        warning("AutoGranted Ignored! Test and AutoGranted cannot be declared together")
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
		if (request$valid == TRUE) {
            QualificationType <- QualificationTypesToDataFrame(xml = request$xml)
            if (print == TRUE) {
                message("QualificationType ", QualificationType$QualificationTypeId[1]," Updated")
                return(QualificationType)
            }
            else invisible(QualificationType)
        }
        else if (request$valid == FALSE) {
            if (print == TRUE) {
                warning("Invalid Request")
            }
            invisible(NULL)
        }
    }
}
