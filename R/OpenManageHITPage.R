OpenManageHITPage <-
function (hit = NULL) {
    if (is.null(hit)) {
        browseURL("https://requester.mturk.com/mturk/manageHITs")
    } else {
        browseURL(paste("https://requester.mturk.com/mturk/manageHIT?HITId=",hit,sep = ""))
    }
    invisible(NULL)
}
