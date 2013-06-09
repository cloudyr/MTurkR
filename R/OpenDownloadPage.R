OpenDownloadPage <-
function (hit, download = FALSE) 
{
    if (download == FALSE) 
        shell.exec(paste("https://requester.mturk.com/mturk/manageHIT?HITId=", 
            hit, "&viewableEditPane=manageHIT_downloadResults", 
            sep = ""))
    else shell.exec(paste("https://requester.mturk.com/mturk/manageHIT?HITId=", 
        hit, sep = ""))
}
