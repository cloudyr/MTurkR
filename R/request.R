request <-
function(operation, GETparameters = NULL,
    keypair = c(Sys.getenv("AWS_ACCESS_KEY_ID"), 
                Sys.getenv("AWS_SECRET_ACCESS_KEY")),
    browser = getOption('MTurkR.browser', FALSE),
    log.requests = getOption('MTurkR.log', TRUE), 
    sandbox = getOption('MTurkR.sandbox', FALSE),
    verbose = getOption('MTurkR.verbose', TRUE),
    validation.test = getOption('MTurkR.test', FALSE),
    service = "AWSMechanicalTurkRequester",
    version = NULL)
{
    if (sandbox) {
        host <- "https://mechanicalturk.sandbox.amazonaws.com/"
    } else {
        host <- "https://mechanicalturk.amazonaws.com/"
    }
    if (is.null(keypair) || identical(keypair, c("", "")) || keypair == "") {
        g <- getOption("MTurkR.keypair")
        if (!is.null(g)) {
            keypair <- g
            warning("Credentials must be set in environment variables: AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY")
        } else {
            stop("No keypair provided.\nPlease set environment variables: AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY")
        }
    }
    keyid <- keypair[1]
    secret <- keypair[2]
    host <- paste(host, "?Service=", service, sep='')
    timestamp <- format(Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    signature <- base64encode(hmac(secret, paste(service, operation, 
            timestamp, sep = ""), algo = "sha1", serialize = FALSE, raw = TRUE))[1]
    urlparameters <- paste("&AWSAccessKeyId=", keyid, 
                           if(!is.null(version)) paste0("&Version=", version) else "", 
                           "&Operation=", operation, 
                           "&Timestamp=", timestamp, 
                           "&Signature=", curl_escape(signature), 
                           GETparameters, sep = "")
    request.url <- paste(host, urlparameters, sep='')
    if (validation.test) {
        message("Request URL: ",request.url,'\n')
        return(structure(list(operation = operation,
                              request.id = NULL,
                              request.url = request.url,
                              valid = NULL,
                              xml = NULL),
                         class='MTurkResponse'))
    } else {
        if (browser == TRUE) {
            browseURL(request.url)
            return(structure(list(operation = operation,
                                  request.id = NULL,
                                  request.url = request.url,
                                  valid = NULL,
                                  xml = NULL),
                             class='MTurkResponse'))
        } else {
            h <- new_handle(postfields = urlparameters)
            fetch <- curl_fetch_memory(url = host, handle = h)
            response <- rawToChar(fetch$content)

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
            if (!is.na(valid.test) && valid.test == "True")  {
                valid <- TRUE
            } else if(!is.na(valid.test) && valid.test == "False") {
                valid <- FALSE
            } else {
                valid <- FALSE
            }
            if (log.requests == TRUE) {
                towrite <- paste("Timestamp\t",
                                 "RequestId\t",
                                 "Operation\t", 
                                 "Sandbox\t",
                                 "Parameters\t",
                                 "Valid\t",
                                 "URL\t", 
                                 "Response", sep = "")
                logfilename <- file.path(getOption('MTurkR.logdir', getwd()),"MTurkRlog.tsv")
                if (!"MTurkRlog.tsv" %in% list.files(path=getOption('MTurkR.logdir', getwd()))) {
                    tryCatch(write(towrite, logfilename),
                             error = function(e){ warning('Writing to new MTurkR log failed!') })
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
                         error = function(e){
                            warning(paste('Writing to MTurkR log failed!\n',
                                          'Log contents were:\n', towrite2))
                        })
            }
            if (!valid && verbose) {
                message("Request ", request.id, " not valid for API request:")
                temp_url <- request.url
                temp_url <- gsub("AWSAccessKeyId=.+(?=&)", "AWSAccessKeyId=REDACTED", temp_url, perl = TRUE)
                temp_url <- gsub('=',' = ', temp_url)
                if (sandbox) {
                    message(paste(paste(strsplit(temp_url, "&")[[1]],
                      collapse="\n                                             &"),'\n\n'))
                } else {
                    message(paste(paste(strsplit(temp_url, "&")[[1]],
                      collapse="\n                                     &"),'\n\n'))
                }
                ParseErrorCodes <- function(xml) {
                    xml.errors <- xpathApply(xmlParse(xml), "//Error")
                    errors <- emptydf(nrow = length(xml.errors), ncol = 2, c("Code", "Message"))
                    for(i in 1:length(xml.errors)) {
                        errors[i,] <- c(xmlValue(xpathApply(xml.errors[[i]], "//Code")[[1]]),
                                        xmlValue(xpathApply(xml.errors[[i]], "//Message")[[1]]))
                    }
                    return(invisible(errors))
                }
                errors <- tryCatch(ParseErrorCodes(xml = response), error = function(e) e)
                if (!inherits(errors, 'error')) {
                    for (i in 1:dim(errors)[1]) {
                        message("Error (", errors[i, 1], "):\n  ", errors[i,2])
                    }
                }
            }
            return(structure(list(operation = operation,
                                  request.id = request.id, 
                                  valid = valid, 
                                  request.url = host,
                                  body = urlparameters,
                                  xml = response), 
                             class = 'MTurkResponse'))
        }
    }
}

print.MTurkResponse <- function(x,...){
    if (!is.null(x$operation)) {
        cat('API Operation: ',x$operation,'\n')
    }
    if (!is.null(x$request.id)) {
        cat('RequestId:     ',x$request.id,'\n')
    }
    if (!is.null(x$valid)) {
        cat('Valid?         ',x$valid,'\n')
    }
    if (!is.null(x$request.url)) {
        cat('Service:       ', curl_unescape(x$request.url),'\n')
    }
    if (!is.null(x$body)) {
        temp <- x$body
        temp <- gsub("AWSAccessKeyId=.+(?=&)", "AWSAccessKeyId=REDACTED", temp, perl = TRUE)
        cat('Parameters:    ',gsub('(?<!^)&','\n                 ',curl_unescape(temp), perl = TRUE),'\n')
    }
    invisible(x)
}

as.data.frame.MTurkResponse <- function(x, ...){
    xml.parsed <- xmlParse(x$xml)
    n <- xmlName(xmlRoot(xml.parsed))
    out <- as.data.frame(xml.parsed)
    invisible(out)
}
