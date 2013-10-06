---
layout: default
title: MTurkR
ghurl: https://github.com/leeper/MTurkR/blob/gh-pages
---

# MTurkR #

Amazon Mechanical Turk (MTurk) is a crowdsourcing platform that provides researchers and developers with the ability to leverage human intelligence for tasks that cannot easily be automated. The **MTurkR** package provides access to the [MTurk](https://requester.mturk.com/) [Requester API](http://docs.aws.amazon.com/AWSMechTurk/latest/AWSMturkAPI/Welcome.html) via authenticated HTTP requests executed with [RCurl](http://cran.r-project.org/web/packages/RCurl/index.html) and a number of functions to translate API calls into R data structures (using the [XML package](http://cran.r-project.org/web/packages/XML/index.html)).

* You can [download a PDF of the package manual here](http://cran.r-project.org/web/packages/MTurkR/MTurkR.pdf)
* [Download the latest package release from CRAN](http://cran.r-project.org/web/packages/MTurkR/index.html)
* [Find the latest development version on GitHub](https://github.com/leeper/MTurkR) and/or install it directly from R using:


```
# install.packages("devtools")
library(devtools)
install_github(repo="MTurkR", username = "leeper")
```
