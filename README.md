# Access the MTurk Requester API via R #

[![Build Status](https://travis-ci.org/leeper/MTurkR.png?branch=master)](https://travis-ci.org/leeper/MTurkR)

The **MTurkR** package provides access to the [Amazon Mechanical Turk](https://requester.mturk.com/) (MTurk) [Requester API](http://docs.aws.amazon.com/AWSMechTurk/latest/AWSMturkAPI/Welcome.html) via authenticated HTTP requests executed with [RCurl](http://cran.r-project.org/web/packages/RCurl/index.html) and a number of functions to translate API calls into R data structures (using the [XML package](http://cran.r-project.org/web/packages/XML/index.html)).

Read more about the package:
* [On CRAN](http://cran.r-project.org/web/packages/MTurkR/index.html)
* [On the MTurkR website](http://leeper.github.io/MTurkR)

---
## Install latest version ##

To install the latest development version from GitHub:

    # install.packages("devtools")
    library(devtools)
    install_github(repo="MTurkR", username = "leeper")

---
## Code examples ##

The MTurkR documentation files contain minimal examples for all functions. If you would like to view or contribute additional examples. Please visit the [the MTurkR wiki](https://github.com/leeper/MTurkR/wiki).

Some additional off-site examples using MTurkR are listed there, as well.
