request <-
function (keyid = getOption('MTurkR.keypair')[1], operation, GETparameters, 
    secret = getOption('MTurkR.keypair')[2],
    version = "2012-03-25", service = "AWSMechanicalTurkRequester", 
    browser = getOption('MTurkR.browser'), log.requests = getOption('MTurkR.log'),
    sandbox = getOption('MTurkR.sandbox'), xml.parse = FALSE,
    print.errors = TRUE, validation.test = getOption('MTurkR.test')) {
    if(sandbox == TRUE) 
        host <- "https://mechanicalturk.sandbox.amazonaws.com/"
    else
        host <- "https://mechanicalturk.amazonaws.com/"
    host <- paste(host, "?Service=", service, sep='')
    timestamp <- format(Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    signature <- base64Encode(hmac(secret, paste(service, operation, 
            timestamp, sep = ""), algo = "sha1", serialize = FALSE, raw = TRUE))[1]
    urlparameters <- paste("&AWSAccessKeyId=", 
        keyid, "&Version=", version, "&Operation=", operation, 
        "&Timestamp=", timestamp, "&Signature=", curlEscape(signature), 
        GETparameters, sep = "")
    request.url <- paste(host, urlparameters, sep='')
    if(validation.test){
        message("Request URL: ",request.url,'\n')
        return(invisible(list(request.url=request.url)))
    }
    else {
        if(browser == TRUE)
            browseURL(request.url)
        else {
            h <- basicTextGatherer()
            #curlPerform(url=request.url,
            curlPerform(url=host,
                        httpheader=c('Content-Type'='application/x-www-form-urlencoded'),
                        postfields=urlparameters,
                        followlocation = 1L, ssl.verifypeer = 1L, ssl.verifyhost = 2L, 
                        cainfo = system.file("CurlSSL",
                                             "cacert.pem",
                                             package = "RCurl"),
                        writefunction=h$update)
            response <- h$value()

            # Additional filters, added by Solomon Messing 6/9/2013:
            clean <- function(x, pattern, replacement){
                res <- gsub( iconv(pattern, "", "ASCII", "byte"), replacement, x, fixed=T)
                return(res)
            }
            response <- clean(response, "\342\200\235", "'")
            response <- clean(response, "\342\200\234", "'")
            response <- clean(response, "\342\200\176" , "'")
            response <- clean(response, "\342\200\177" , "'")
            response <- clean(response, "\342\200\230" , "'")
            response <- clean(response, "\342\200\231" , "'")
            response <- clean(response, "\342\200\232" , ',')
            response <- clean(response, "\342\200\233" , "'")
            response <- clean(response, "\342\200\234" , '"')
            response <- clean(response, "\342\200\235" , '"')
            response <- clean(response, "\342\200\224" , '--')
            response <- clean(response, "\342\200\225" , '--')
            response <- clean(response, "\342\200\042" , '--')
            response <- clean(response, "\342\200\246" , '...')
            response <- clean(response, "\342\200\041" , '-')
            response <- clean(response, "\342\200\174" , '-')
            response <- clean(response, "\342\200\220" , '-')
            response <- clean(response, "\342\200\223" , '-')
            
            request.id <-
                strsplit(strsplit(response, "<RequestId>")[[1]][2],
                    "</RequestId>")[[1]][1]
            valid.test <-
                strsplit(strsplit(response, "<Request><IsValid>")[[1]][2],
                    "</IsValid>")[[1]][1]
            if(!is.na(valid.test) && valid.test == "True") 
                valid <- TRUE
            else if(!is.na(valid.test) && valid.test == "False") 
                valid <- FALSE
            else
                valid <- FALSE
            if(log.requests == TRUE) {
                towrite <- paste("Timestamp\t",
                                 "RequestId\t",
                                 "Operation\t", 
                                 "Sandbox\t",
                                 "Parameters\t",
                                 "Valid\t",
                                 "URL\t", 
                                 "Response", sep = "")
                logfilename <- file.path(getOption('MTurkR.logdir'),"MTurkRlog.tsv")
                if(!"MTurkRlog.tsv" %in% list.files(path=getOption('MTurkR.logdir'))) {
                    tryCatch(write(towrite, logfilename),
                        error=function(e){
                            warning('Writing to new MTurkR log failed!')
                        })
                }
                response.xml <- response
                response.xml <- gsub(" ", "#!SPACE!#", response.xml, fixed = TRUE)
                response.xml <- gsub("[[:space:]]", "", response.xml)
                response.xml <- gsub("#!SPACE!#", " ", response.xml, fixed = TRUE)
                response.xml <- gsub("&#xa;", "", response.xml, fixed = TRUE)
                response.xml <- gsub("&#xA;", "", response.xml, fixed = TRUE)
                response.xml <- gsub("&#xd;", "", response.xml, fixed = TRUE)
                response.xml <- gsub("&#xD;", "", response.xml, fixed = TRUE)
                response.xml <- gsub("&#x9;", "  ", response.xml, fixed = TRUE)
                response.xml <- gsub("&#09;", "  ", response.xml, fixed = TRUE)
                towrite2 <- paste(timestamp, "\t",
                                  request.id, "\t", 
                                  operation, "\t",
                                  sandbox, "\t",
                                  GETparameters, "\t",
                                  valid, "\t",
                                  request.url, "\t",
                                  response.xml, sep = "")
                tryCatch(write(towrite2, logfilename, append = TRUE),
                        error=function(e){
                            warning(paste('Writing to MTurkR log failed!\n',
                                          'Log contents were:\n',towrite2))
                        })
            }
            if(valid == FALSE) {
                if(print.errors == TRUE) {
                    message("Request ", request.id, " not valid for API request:")
                    temp.url <- gsub('=',' = ',request.url)
                    if(sandbox) {
                        message(paste(paste(strsplit(temp.url, "&")[[1]],
                          collapse="\n                                             &"),'\n\n'))
                    } else {
                        message(paste(paste(strsplit(temp.url, "&")[[1]],
                          collapse="\n                                     &"),'\n\n'))
                    }
                    errors <- tryCatch(ParseErrorCodes(xml = response),
                        error = function(e) e)
                    if(!inherits(errors, 'error')){
                        cat('\n')
                        for (i in 1:dim(errors)[1])
                            message("Error (", errors[i, 1], "): ", errors[i,2])
                    }
                }
            }
            if(xml.parse == TRUE) {
                xml.parsed <- xmlParse(response)
                out <- list(request.url = request.url, request.id = request.id, 
                            valid = valid, xml = response, xml.parsed = xml.parsed)
            }
            else
                out <- list(request.url = request.url, request.id = request.id, 
                            valid = valid, xml = response)
            class(out) <- c('MTurkResponse',class(out))
            attr(out, 'operation') <- operation
            return(invisible(out))
        }
    }
}

print.MTurkResponse <- function(x,...){
    cat('RequestId:   ',x$request.id,'\n')
    cat('Valid?       ',x$valid,'\n')
    cat('Request URL: ',gsub('&','\n',curlUnescape(x$request.url),'\n'))
    cat('XML Response:\n')
    print(xmlParse(x$response))
    invisible(x)
}

as.data.frame.MTurkResponse <- function(x, ...){
    
}