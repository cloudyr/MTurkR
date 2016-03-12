OpenDownloadPage <-
function (hit, download = FALSE) {
    if (!download) {
        browseURL(paste("https://requester.mturk.com/mturk/manageHIT?HITId=", 
                        hit, "&viewableEditPane=manageHIT_downloadResults", sep = ""))
    } else {
        browseURL(paste("https://requester.mturk.com/mturk/manageHIT?HITId=", hit, sep = ""))
    }
    invisible(NULL)
}
