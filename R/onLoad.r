.onLoad <- function(libname, pkgname){
    options(MTurkR.logdir = getwd())    # MTurkRlog.tsv directory
    options(MTurkR.sandbox = FALSE)     # sandbox logical
    options(MTurkR.print = TRUE)        # print logical
    options(MTurkR.browser = FALSE)     # browser logical
    options(MTurkR.log = TRUE)          # log logical
    options(MTurkR.test = FALSE)        # validation.test logical
}