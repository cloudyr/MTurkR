# Access the MTurk Requester API via R #

[![Build Status](https://travis-ci.org/leeper/MTurkR.png?branch=master)](https://travis-ci.org/leeper/MTurkR)

The **MTurkR** package provides access to the [Amazon Mechanical Turk](https://requester.mturk.com/) (MTurk) [Requester API](http://docs.aws.amazon.com/AWSMechTurk/latest/AWSMturkAPI/Welcome.html), with a number of functions to translate API calls into R data structures (using the [XML package](http://cran.r-project.org/web/packages/XML/index.html)).

Obvious applications of the package include:
* Survey or experimental research using MTurk workers as subjects
* Large-scale human coding or categorization of text, images, video, audio, or other files
* Collection of unstructured data from the web
* Audio and video transcription
* Preprocessing of "messy" prior to statistical procedures

Read more about the package:
* [On CRAN](http://cran.r-project.org/web/packages/MTurkR/index.html)
* [On the MTurkR website](http://leeper.github.io/MTurkR)
* [On the MTurkR wiki](http://github.com/leeper/MTurkR/wiki)

---
## Install latest version ##

To install the latest version from CRAN:

    install.packages("MTurkR")

To install the latest development version from GitHub:

    # install.packages("devtools")
    library("devtools")
    install_github("leeper/MTurkR")

---
## Code examples ##

The MTurkR documentation files contain minimal examples for all functions. Further examples of how to use MTurkR are provided in [the wiki](https://github.com/leeper/MTurkR/wiki). Users can contribute their own examples or further documentation there, or via pull requests to the GitHub repository.
