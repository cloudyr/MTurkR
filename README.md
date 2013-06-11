MTurkR: Access to Amazon Mechanical Turk Requester API via R
========

This package provides access to the Amazon Mechanical Turk (MTurk) Requester API via authenticated HTTP requests executed with the RCurl package and a number of functions to translate API calls into R data structures (using the XML package).

Read more about the package:
* On CRAN: http://cran.r-project.org/web/packages/MTurkR/index.html
* On my website: http://thomasleeper.com/MTurkR/index.html

## To install the latest development version from GitHub:

    # install.packages("devtools")
    library(devtools)
    install_github(repo="MTurkR", username = "leeper")

## Examples
* Create and manage qualifications: http://www.thomasleeper.com/MTurkR/qualification_test_example.r, associated xml files: http://www.thomasleeper.com/MTurkR/questionform_example.xml and http://www.thomasleeper.com/MTurkR/answerkey_example.xml. 
