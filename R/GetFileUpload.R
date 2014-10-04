GetFileUpload <-
geturls <-
function (assignment, questionIdentifier, download = FALSE, file.ext = NULL, 
    open.file.in.browser = FALSE, verbose = getOption('MTurkR.verbose', TRUE), ...) {
    # temporary check for `print` argument (remove after v1.0)
    if('print' %in% names(list(...)) && is.null(verbose))
        verbose <- list(...)$print
    operation <- "GetFileUploadURL"
    FileUploadURL <- setNames(data.frame(matrix(nrow = length(assignment), ncol = 4)),
                        c("Assignment", "questionIdenfier", "RequestURL", "Valid"))
    for(i in 1:length(assignment)) {
        GETparameters <- paste("&AssignmentId=", curlEscape(assignment), 
                               "&QuestionIdentifier=", curlEscape(questionIdentifier), 
                               sep = "")        
        request <- request(operation, GETparameters = GETparameters, ...)
        if(is.null(request$valid))
            return(request)
        if(request$valid) {
            u <- strsplit(strsplit(request$xml, "<FileUploadURL>")[[1]][2], "</FileUploadURL>")[[1]][1]
            FileUploadURL[i, ] <- c(assignment[i], questionIdentifier, u, request$valid)
            if(verbose) 
                message("FileUploadURL for Assignment ", assignment[i], " Retrieved: ", u)
            if(open.file.in.browser) 
                browseURL(u)
            if(download.file)
                download.file(u, paste(questionIdentifer, "_", 
                                       assignment, "_", basename(u), 
                                       sep = ""), mode = "wb")
        } else if(verbose) {
            message("Request for Assignment ", assignment[i], " failed")
        }
    }
    FileUploadURL$Valid <- factor(FileUploadURL$Valid, levels=c('TRUE','FALSE'))
    return(FileUploadURL)
}
