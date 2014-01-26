UpdateQualificationType <-
updatequal <-
function (qual, description = NULL, status = NULL, retry.delay = NULL, 
    test = NULL, answerkey = NULL, test.duration = NULL,
    validate.test = FALSE, validate.answerkey = FALSE,
    auto = NULL, auto.value = NULL,
    keypair = credentials(), print = getOption('MTurkR.print'), 
    browser = getOption('MTurkR.browser'), log.requests = getOption('MTurkR.log'),
    sandbox = getOption('MTurkR.sandbox'), validation.test = getOption('MTurkR.test')) {
    if(!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else
        stop("No keypair provided or 'credentials' object not stored")
    operation <- "UpdateQualificationType"
    if(is.factor(qual))
        qual <- as.character(qual)
    GETparameters <- paste("&QualificationTypeId=", qual, sep = "")
    if(!is.null(description)) 
        GETparameters <- paste(GETparameters, "&Description=", curlEscape(description), sep = "")
    if(!is.null(status)) 
        GETparameters <- paste(GETparameters, "&QualificationTypeStatus=", status, sep = "")
    if(!is.null(test)) {
        if(validate.test==TRUE){
			if(!is.null(xmlChildren(xmlParse(test))$QuestionForm))
				namespace <- xmlNamespace(xmlChildren(xmlParse(test))$QuestionForm)[1]
			else
				stop("No Namespace specified in 'test'")
			validation <- xmlSchemaValidate(namespace, test)
			if(!validation$status==0){
				warning("'test' object does not validate against MTurk schema")
				return(validation)
			}
        }
        GETparameters <- paste(GETparameters, "&Test=", curlEscape(test), "&TestDurationInSeconds=", test.duration, sep = "")
    }
    if(!is.null(retry.delay)) 
        GETparameters <- paste(GETparameters, "&RetryDelayInSeconds=", retry.delay, sep = "")
    if(!is.null(answerkey)) {
        if(validate.answerkey==TRUE){
            if(!is.null(xmlChildren(xmlParse(answerkey))$AnswerKey))
                namespace <- xmlNamespace(xmlChildren(xmlParse(answerkey))$AnswerKey)[1]
            else
                stop("No Namespace specified in 'answerkey'")
            validation <- xmlSchemaValidate(namespace, answerkey)
            if(!validation$status==0){
                warning("'answerkey' object does not validate against MTurk schema")
                return(validation)
            }
        }
        t.temp <- unique(QuestionFormToDataFrame(test)$Questions$QuestionIdentifier)
        a.temp <- unique(AnswerKeyToDataFrame(answerkey)$Questions$QuestionIdentifier)
        if(!sum(a.temp %in% t.temp) == length(a.temp)) 
            stop("One or more QuestionIdentifiers in AnswerKey not in QuestionForm")
        if(!sum(t.temp %in% a.temp) == length(t.temp)) 
            stop("One or more QuestionIdentifiers in QuestionForm not in AnswerKey")
        GETparameters <- paste(GETparameters, "&AnswerKey=", curlEscape(answerkey), sep = "")
    }
    if(!is.null(auto.value)) {
        if (!is.numeric(as.numeric(auto.value))) 
            stop("AutoGrantedValue must be numeric or coercable to numeric")
    }
    if(is.null(auto) & !is.null(auto.value)) 
        GETparameters <- paste(GETparameters, "&AutoGrantedValue=", auto.value, sep = "")
    else if(!is.null(auto) && auto == TRUE) {
        GETparameters <- paste(GETparameters, "&AutoGranted=", "1", sep = "")
        if (is.null(test) & !is.null(auto.value)) 
            GETparameters <- paste(GETparameters, "&AutoGrantedValue=", auto.value, sep = "")
    }
    else if(!is.null(auto) && auto == FALSE) {
        GETparameters <- paste(GETparameters, "&AutoGranted=", "0", sep = "")
        if (is.null(test) & !is.null(auto.value)) 
            GETparameters <- paste(GETparameters, "&AutoGrantedValue=", auto.value, sep = "")
    }
    else if(!is.null(auto) && auto == TRUE & is.null(test) & 
        is.null(auto.value)) 
        GETparameters <- paste(GETparameters, "&AutoGranted=", "1", sep = "")
    else if(!is.null(auto) && auto == FALSE & is.null(test) & 
        is.null(auto.value)) 
        GETparameters <- paste(GETparameters, "&AutoGranted=", "0", sep = "")
    else if(!is.null(auto) && !is.null(test)) 
        warning("AutoGranted Ignored! Test and AutoGranted cannot be declared together")
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
            sandbox = sandbox, validation.test = validation.test)
        if(validation.test)
			return(invisible(request))
		if(request$valid == TRUE) {
            QualificationType <- QualificationTypesToDataFrame(xml = request$xml)
            if(print == TRUE) {
                message("QualificationType ", QualificationType$QualificationTypeId[1],
                        " Updated")
            }
            return(QualificationType)
        }
        else if(request$valid == FALSE & print == TRUE)
            warning("Invalid Request")
        return(NULL)
    }
}
