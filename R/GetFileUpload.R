GetFileUpload <-
geturls <-
function (assignment, questionIdentifier, download = FALSE, file.ext = NULL, 
    open.file.in.browser = FALSE, verbose = getOption('MTurkR.verbose'), ...) {
    # temporary check for `print` argument (remove after v1.0)
    if('print' %in% names(list(...)) && is.null(verbose))
        verbose <- list(...)$print
    operation <- "GetFileUploadURL"
    FileUploadURL <- setNames(data.frame(matrix(nrow = length(assignment), ncol = 3)),
                        c("Assignment", "RequestURL", "Valid"))
    for(i in 1:length(assignment)) {
        GETparameters <- paste("&AssignmentId=", curlEscape(assignment), 
            "&QuestionIdentifier=", curlEscape(questionIdentifier), sep = "")        
        request <- request(operation, GETparameters = GETparameters, ...)
        if(is.null(request$valid))
            return(request)
        if(request$valid == TRUE) {
            url <- strsplit(strsplit(request$xml, "<FileUploadURL>")[[1]][2], "</FileUploadURL>")[[1]][1]
            FileUploadURL[i, ] <- c(assignment[i], url, request$valid)
            if(verbose) 
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
            if(verbose) 
                message("Request for Assignment ", assignment[i], " failed")
        }
    }
    FileUploadURL$Valid <- factor(FileUploadURL$Valid, levels=c('TRUE','FALSE'))
    return(FileUploadURL)
}
