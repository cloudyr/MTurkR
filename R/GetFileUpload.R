GetFileUpload <-
geturls <-
function (assignment, questionIdentifier, download = FALSE, file.ext = NULL, 
    open.file.in.browser = FALSE, keypair = getOption('MTurkR.keypair'),
    print = getOption('MTurkR.print'), 
    log.requests = getOption('MTurkR.log'),
    sandbox = getOption('MTurkR.sandbox'), validation.test = getOption('MTurkR.test')) {
    if(is.null(keypair))
        stop("No keypair provided or 'credentials' object not stored")
    operation <- "GetFileUploadURL"
    FileUploadURL <- setNames(data.frame(matrix(nrow = length(assignment), ncol = 3)),
                        c("Assignment", "RequestURL", "Valid"))
    for(i in 1:length(assignment)) {
        GETparameters <- paste("&AssignmentId=", curlEscape(assignment), 
            "&QuestionIdentifier=", curlEscape(questionIdentifier), sep = "")        
        request <- request(keypair[1], operation, secret=keypair[2],
            GETparameters = GETparameters, log.requests = log.requests, 
            sandbox = sandbox, validation.test = validation.test)
        if(validation.test)
            return(invisible(request))
        if(request$valid == TRUE) {
            url <- strsplit(strsplit(request$xml, "<FileUploadURL>")[[1]][2], "</FileUploadURL>")[[1]][1]
            FileUploadURL[i, ] <- c(assignment[i], url, request$valid)
            if(print == TRUE) 
              message("FileUploadURL for Assignment ", assignment[i], " Retrieved: ", url)
            if(open.file.in.browser == TRUE) 
              browseURL(url)
            if(download.file == TRUE) {
                if(is.null(file.ext)) 
                    file.ext = ""
                download.file(url, paste(assignment, "file", file.ext, sep = ""), mode = "wb")
            }
        }
        else if(request$valid == FALSE) {
            if(print == TRUE) 
                message("Request for Assignment ", assignment[i], " failed")
        }
    }
    FileUploadURL$Valid <- factor(FileUploadURL$Valid, levels=c('TRUE','FALSE'))
    return(FileUploadURL)
}
