# MTurkR
## Access to Amazon Mechanical Turk Requester API via R

This is the repository branch for the MTurkR website. You can see the website at http://leeper.github.io/MTurkR and download the package from [CRAN](http://cran.r-project.org/web/packages/MTurkR/index.html).

The **MTurkR** package provides access to the [Amazon Mechanical Turk](https://requester.mturk.com/) (MTurk) [Requester API](http://docs.aws.amazon.com/AWSMechTurk/latest/AWSMturkAPI/Welcome.html) via authenticated HTTP requests executed with [RCurl](http://cran.r-project.org/web/packages/RCurl/index.html) and a number of functions to translate API calls into R data structures (using the [XML package](http://cran.r-project.org/web/packages/XML/index.html)).

### To install the latest development version from GitHub:

    # install.packages("devtools")
    library(devtools)
    install_github(repo="MTurkR", username = "leeper")
