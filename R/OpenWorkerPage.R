OpenWorkerPage <-
function (workerid = NULL) 
{
    if (is.null(workerid)) 
        shell.exec("https://requester.mturk.com/workers/")
    if (!is.null(workerid)) 
        shell.exec(paste("https://requester.mturk.com/workers/", 
            workerid, sep = ""))
}
