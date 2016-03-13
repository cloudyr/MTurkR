OpenWorkerPage <-
function (workerid = NULL) {
    if (is.null(workerid)) {
        browseURL("https://requester.mturk.com/workers/")
    }
    if (!is.null(workerid)) {
        browseURL(paste("https://requester.mturk.com/workers/", workerid, sep = ""))
    }
    invisible(NULL)
}
