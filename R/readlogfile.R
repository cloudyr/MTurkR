readlogfile <-
function(path = getOption('MTurkR.logdir'), filename = "MTurkRlog.tsv"){
    if (!filename %in% list.files(path=path)) {
        stop("No Log File Found")
    }
    logfile <- read.delim(file.path(path,filename), header = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)
    invisible(logfile)
}
