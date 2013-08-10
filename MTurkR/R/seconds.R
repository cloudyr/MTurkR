seconds <-
function (days = NULL, hours = NULL, minutes = NULL, seconds = NULL) 
{
    if (!is.null(days)) 
        s1 <- days * 24 * 60 * 60
    else s1 <- 0
    if (!is.null(hours)) 
        s2 <- hours * 60 * 60
    else s2 <- 0
    if (!is.null(minutes)) 
        s3 <- minutes * 60
    else s3 <- 0
    if (!is.null(seconds)) 
        s4 <- seconds
    else s4 <- 0
    return(sum(c(s1, s2, s3, s4)))
}
