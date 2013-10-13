CreateQualificationType <-
createqual <-
function (name, description, status, keywords = NULL, retry.delay = NULL, 
    test = NULL, answerkey = NULL, test.duration = NULL,
	validate.test = FALSE, validate.answerkey = FALSE,
	auto = NULL, auto.value = NULL, keypair = credentials(),
    print = getOption('MTurkR.print'), browser = getOption('MTurkR.browser'),
    log.requests = getOption('MTurkR.log'), sandbox = getOption('MTurkR.sandbox'),
    validation.test = getOption('MTurkR.test')) {
    if(!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else
        stop("No keypair provided or 'credentials' object not stored")
    operation <- "CreateQualificationType"
    if(!status %in% c("Active", "Inactive")) 
        stop("QualificationTypeStatus must be Active or Inactive")
    GETparameters <- paste("&Name=", curlEscape(name), "&Description=", 
        curlEscape(description), "&QualificationTypeStatus=", 
        status, sep = "")
    if(!is.null(keywords)) 
        GETparameters <- paste(GETparameters, "&Keywords=", curlEscape(keywords), sep = "")
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
		GETparameters <- paste(GETparameters, "&Test=", curlEscape(test), 
								"&TestDurationInSeconds=", test.duration, sep = "")
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
			t.temp <- QuestionFormToDataFrame(test)$Questions$QuestionIdentifier
            a.temp <- AnswerKeyToDataFrame(answerkey)$Questions$QuestionIdentifier
            if(!sum(a.temp %in% t.temp) == length(a.temp)) 
                stop("One or more QuestionIdentifiers in AnswerKey not in QuestionForm")
            if(!sum(t.temp %in% a.temp) == length(t.temp)) 
                stop("One or more QuestionIdentifiers in QuestionForm not in AnswerKey")
            GETparameters <- paste(GETparameters, "&AnswerKey=", 
                curlEscape(answerkey), sep = "")
        }
    }
    if(!is.null(retry.delay)) 
        GETparameters <- paste(GETparameters, "&RetryDelayInSeconds=", retry.delay, sep = "")
    if(!is.null(auto) && auto == TRUE & is.null(test) & !is.null(auto.value)) 
        GETparameters <- paste(GETparameters, "&AutoGranted=", "true",
								"&AutoGrantedValue=", auto.value, sep = "")
    else if(!is.null(auto) && auto == FALSE & is.null(test) & !is.null(auto.value)) 
        GETparameters <- paste(GETparameters, "&AutoGranted=", "false",
								"&AutoGrantedValue=", auto.value, sep = "")
    else if(!is.null(auto) && auto == TRUE & is.null(test) & is.null(auto.value)) 
        GETparameters <- paste(GETparameters, "&AutoGranted=", "true", sep = "")
    else if(!is.null(auto) && auto == FALSE & is.null(test) & is.null(auto.value)) 
        GETparameters <- paste(GETparameters, "&AutoGranted=", "false", sep = "")
    else if(!is.null(auto) && !is.null(test)) 
        warning("AutoGranted Ignored! Test and AutoGranted cannot be declared together")
    auth <- authenticate(operation, secret)
    if(browser == TRUE) {
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
        if(request$valid == TRUE) {
            QualificationType <- QualificationTypesToDataFrame(xml = request$xml)
            if(print == TRUE)
                message("QualificationType Created: ", QualificationType$QualificationTypeId[1])
            invisible(QualificationType)
        }
        else if(request$valid == FALSE) {
            if(print == TRUE) 
                warning("Invalid request")
            invisible(NULL)
        }
    }
}
