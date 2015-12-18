.onLoad <- function(libname, pkgname){
    if(is.null(getOption("MTurkR.logdir")))
        options(MTurkR.logdir = getwd())    # MTurkRlog.tsv directory
    if(is.null(getOption("MTurkR.sandbox")))
        options(MTurkR.sandbox = FALSE)     # sandbox logical
    if(is.null(getOption("MTurkR.verbose")))
        options(MTurkR.verbose = TRUE)      # print logical
    if(is.null(getOption("MTurkR.browser")))
        options(MTurkR.browser = FALSE)     # browser logical
    if(is.null(getOption("MTurkR.log")))
        options(MTurkR.log = TRUE)          # log logical
    if(is.null(getOption("MTurkR.test")))
        options(MTurkR.test = FALSE)        # validation.test logical
    if (Sys.getenv("AWS_ACCESS_KEY_ID") == "") {
        message("Environment variable AWS_ACCESS_KEY_ID not set!")
    }
    if (Sys.getenv("AWS_SECRET_ACCESS_KEY") == "") {
        message("Environment variable AWS_SECRET_ACCESS_KEY not set!")
    }
}
