GetFileUpload <-
geturls <-
function(assignment, 
         questionIdentifier, 
         download = FALSE, 
         open.file.in.browser = FALSE, 
         verbose = getOption('MTurkR.verbose', TRUE), ...) {
    operation <- "GetFileUploadURL"
    FileUploadURL <- emptydf(length(assignment), 4, c("Assignment", "questionIdenfier", "RequestURL", "Valid"))
    for (i in 1:length(assignment)) {
        GETparameters <- paste("&AssignmentId=", curl_escape(assignment), 
                               "&QuestionIdentifier=", curl_escape(questionIdentifier), 
                               sep = "")        
        request <- request(operation, GETparameters = GETparameters, ...)
        if (is.null(request$valid)) {
            return(request)
        }
        if (request$valid) {
            u <- strsplit(strsplit(request$xml, "<FileUploadURL>")[[1]][2], "</FileUploadURL>")[[1]][1]
            FileUploadURL[i, ] <- c(assignment[i], questionIdentifier, u, request$valid)
            if (isTRUE(verbose)) {
                message("FileUploadURL for Assignment ", assignment[i], " Retrieved: ", u)
            }
            if (isTRUE(open.file.in.browser)) {
                browseURL(u)
            } else if (isTRUE(download)) {
                download.file(u, paste(questionIdentifier, "_", 
                                       assignment, "_", basename(u), 
                                       sep = ""), mode = "wb")
            }
        } else if (isTRUE(verbose)) {
            message("Request for Assignment ", assignment[i], " failed")
        }
    }
    FileUploadURL$Valid <- factor(FileUploadURL$Valid, levels=c('TRUE','FALSE'))
    return(FileUploadURL)
}
