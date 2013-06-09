request <-
function (keyid, operation, signature, timestamp, GETparameters, 
    version = "2012-03-25", service = "AWSMechanicalTurkRequester", 
    browser = FALSE, log.requests = TRUE, sandbox = FALSE, xml.parse = FALSE, 
    print.errors = TRUE, validation.test = FALSE) 
{
    if (sandbox == TRUE) 
        host <- "https://mechanicalturk.sandbox.amazonaws.com/"
    else host <- "https://mechanicalturk.amazonaws.com/"
    request.url <- paste(host, "?Service=", service, "&AWSAccessKeyId=", 
        keyid, "&Version=", version, "&Operation=", operation, 
        "&Timestamp=", timestamp, "&Signature=", curlEscape(signature), 
        GETparameters, sep = "")
    if (validation.test == TRUE) {
        cat("Request URL: ", request.url, "\n", sep = "")
        invisible(request.url)
    }
    else {
        if (browser == TRUE) {
            browseURL(request.url)
        }
        else {
            response <- getURL(request.url, followlocation = TRUE, 
                ssl.verifypeer = TRUE, ssl.verifyhost = TRUE, 
                cainfo = system.file("CurlSSL", "cacert.pem", 
                  package = "RCurl"))
            request.id <- strsplit(strsplit(response, "<RequestId>")[[1]][2], 
                "</RequestId>")[[1]][1]
            valid.test <- strsplit(strsplit(response, "<Request><IsValid>")[[1]][2], 
                "</IsValid>")[[1]][1]
            if (!is.na(valid.test) && valid.test == "True") 
                valid <- TRUE
            else if (!is.na(valid.test) && valid.test == "False") 
                valid <- FALSE
            else valid <- FALSE
            if (log.requests == TRUE) {
                logfilename <- "MTurkRlog.tsv"
                if (!logfilename %in% list.files()) 
                  write(paste("Timestamp\t", "RequestId\t", "Operation\t", 
                    "Sandbox\t", "Parameters\t", "Valid\t", "URL\t", 
                    "Response", sep = ""), logfilename)
                response.xml <- response
                response.xml <- gsub(" ", "#!SPACE!#", response.xml, 
                  fixed = TRUE)
                response.xml <- gsub("[[:space:]]", "", response.xml)
                response.xml <- gsub("#!SPACE!#", " ", response.xml, 
                  fixed = TRUE)
                response.xml <- gsub("&#xa;", "", response.xml, 
                  fixed = TRUE)
                response.xml <- gsub("&#xA;", "", response.xml, 
                  fixed = TRUE)
                response.xml <- gsub("&#xd;", "", response.xml, 
                  fixed = TRUE)
                response.xml <- gsub("&#xD;", "", response.xml, 
                  fixed = TRUE)
                response.xml <- gsub("&#x9;", "  ", response.xml, 
                  fixed = TRUE)
                response.xml <- gsub("&#09;", "  ", response.xml, 
                  fixed = TRUE)
                write(paste(timestamp, "\t", request.id, "\t", 
                  operation, "\t", sandbox, "\t", GETparameters, 
                  "\t", valid, "\t", request.url, "\t", response.xml, 
                  sep = ""), logfilename, append = TRUE)
            }
            if (valid == FALSE) {
                if (print.errors == TRUE) {
                  cat("Request ", request.id, " not valid for API request:\n", 
                    sep = "")
                  cat(strsplit(request.url, "&")[[1]], sep = "\n                                     &")
                  errors <- ParseErrorCodes(xml = response)
                  for (i in 1:dim(errors)[1]) {
                    cat("Error (", errors[i, 1], "): ", errors[i, 
                      2], "\n", sep = "")
                  }
                }
            }
            if (xml.parse == TRUE) {
                xml.parsed <- xmlParse(response)
                invisible(list(request.url = request.url, request.id = request.id, 
                  valid = valid, xml = response, xml.parsed = xml.parsed))
            }
            else invisible(list(request.url = request.url, request.id = request.id, 
                valid = valid, xml = response))
        }
    }
}
