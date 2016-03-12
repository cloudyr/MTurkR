seconds <-
function (days = NULL, hours = NULL, minutes = NULL, seconds = NULL) {
    s1 <- if (!is.null(days)) as.numeric(days) * 24 * 60 * 60 else 0
    s2 <- if (!is.null(hours)) as.numeric(hours) * 60 * 60 else 0
    s3 <- if (!is.null(minutes)) as.numeric(minutes) * 60 else 0
    s4 <- if (!is.null(seconds)) as.numeric(seconds) else 0
    return(sum(c(s1, s2, s3, s4)))
}
