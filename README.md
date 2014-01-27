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
    library(devtools)
    install_github("leeper/MTurkR")

---
## Some code examples ##
* Examples in an introductory article: "[Crowdsourcing with R and the MTurk API](http://polmeth.wustl.edu/methodologist/tpm_v20_n2.pdf)" in *The Political Methodologist* 20(1):2-7.
* Examples from a blog post by Solomon Messing: "[Streamline Your Mechanical Turk Workflow with MTurkR](http://solomonmessing.wordpress.com/2013/06/24/streamline-your-mechanical-turk-workflow-with-mturkr/)
* Sample code to [create and manage qualifications](http://leeper.github.io/MTurkR/qualification_test_example.r), with associated xml files for [QuestionForm](http://leeper.github.io/MTurkR/questionform_example.xml) and [AnswerKey](http://leeper.github.io/MTurkR/answerkey_example.xml)
* Workflow and sample code to [mimick the Requester UI's display of input HITLayout parameters with assignment results](https://github.com/leeper/MTurkR/blob/gh-pages/batchinputs.md).
* [Access your Turkopticon requester ratings from R](https://github.com/leeper/MTurkR/blob/gh-pages/turkopticon.md).
