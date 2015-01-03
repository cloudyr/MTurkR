.onLoad <- function(libname, pkgname){
    options(MTurkR.logdir = getwd())    # MTurkRlog.tsv directory
    options(MTurkR.sandbox = FALSE)     # sandbox logical
    options(MTurkR.verbose = TRUE)      # print logical
    options(MTurkR.browser = FALSE)     # browser logical
    options(MTurkR.log = TRUE)          # log logical
    options(MTurkR.test = FALSE)        # validation.test logical
    a <- Sys.getenv("AWS_ACCESS_KEY_ID")
    s <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
    if(a != "" & s != "") {
        options(MTurkR.keypair = c(a,s))
    }
}