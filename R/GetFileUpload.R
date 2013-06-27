GetFileUpload <-
geturls <-
function (assignment, questionIdentifier, download = FALSE, file.ext = NULL, 
    open.file.in.browser = FALSE, keypair = credentials(), print = TRUE, 
    browser = FALSE, log.requests = TRUE, sandbox = FALSE) 
{
    if (!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else stop("No keypair provided or 'credentials' object not stored")
    operation <- "GetFileUploadURL"
    FileUploadURL <- data.frame(matrix(nrow = length(assignment), 
        ncol = 3))
    names(FileUploadURL) <- c("Assignment", "RequestURL", "Valid")
    for (i in 1:length(assignment)) {
        GETparameters <- paste("&AssignmentId=", curlEscape(assignment), 
            "&QuestionIdentifier=", curlEscape(questionIdentifier), 
            sep = "")
        auth <- authenticate(operation, secret)
        if (browser == TRUE) {
            request <- request(keyid, auth$operation, auth$signature, 
                auth$timestamp, GETparameters, browser = browser, 
                sandbox = sandbox)
            if (open.file.in.browser == TRUE) 
                warning("Request to open file in browser ignored")
            if (download == TRUE) 
                warning("Request to download file ignored")
        }
        else {
            request <- request(keyid, auth$operation, auth$signature, 
                auth$timestamp, GETparameters, log.requests = log.requests, 
                sandbox = sandbox)
            if (request$valid == TRUE) {
                url <- strsplit(strsplit(request$xml, "<FileUploadURL>")[[1]][2], 
                  "</FileUploadURL>")[[1]][1]
                FileUploadURL[i, ] <- c(assignment[i], url, request$valid)
                if (print == TRUE) 
                  message("FileUploadURL for Assignment ", assignment[i], 
                    " Retrieved: ", url)
                if (open.file.in.browser == TRUE) 
                  browseURL(url)
                if (download.file == TRUE) {
                  if (is.null(file.ext)) 
                    file.ext = ""
                  download.file(url, paste(assignment, "file", 
                    file.ext, sep = ""), mode = "wb")
                }
            }
            else if (request$valid == FALSE) {
                if (print == TRUE) 
                  message("Request for Assignment ", assignment[i], " failed")
            }
        }
    }
    if (browser == FALSE) {
        if (print == TRUE) 
            return(FileUploadURL)
        else invisible(FileUploadURL)
    }
}
