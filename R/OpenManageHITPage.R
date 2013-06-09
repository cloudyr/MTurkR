OpenManageHITPage <-
function (hit = NULL) 
{
    if (is.null(hit)) 
        shell.exec("https://requester.mturk.com/mturk/manageHITs")
    if (!is.null(hit)) 
        shell.exec(paste("https://requester.mturk.com/mturk/manageHIT?HITId=", 
            hit, sep = ""))
}
