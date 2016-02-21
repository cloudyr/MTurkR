library("testthat")
k1 <- Sys.getenv("AWS_ACCESS_KEY_ID")
k2 <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
if ((k1 != "") && (k2 != "")) {
    library("MTurkR")
    options("MTurkR.sandbox" = TRUE)
    options('MTurkR.log' = FALSE)
    test_check("MTurkR")
}
