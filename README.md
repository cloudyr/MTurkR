# R Client for the MTurk Requester API #

**MTurkR** provides programmatic access to the crowdsourcing functionality of the [Amazon Mechanical Turk](https://requester.mturk.com/) (MTurk) [Requester API](http://docs.aws.amazon.com/AWSMechTurk/latest/AWSMturkAPI/Welcome.html).

Amazon provides [a helpful chart for deciding whether an API client (like MTurkR) is useful for your project](https://requester.mturk.com/tour/choose_the_right_tool).

Possible applications of the package include:

 * Survey or experimental research using MTurk workers as human subjects
 * Large-scale human coding or categorization of text, images, video, audio, or other files
 * Collection of unstructured data from the web
 * Audio and video transcription
 * Preprocessing of "messy" data prior to statistical procedures

The MTurkR documentation files contain minimal examples for all functions. Further examples of how to use MTurkR are provided in [the MTurkR GitHub wiki](https://github.com/cloudyr/MTurkR/wiki). Users can contribute their own examples or further documentation there, or via pull requests to the GitHub repository.

Using MTurkR requires setting two environment variables: `AWS_ACCESS_KEY_ID` and `AWS_SECRET_ACCESS_KEY`. Specify them on the command-line before initializing the R session, or use `Sys.setenv()` within R, or place these values in an `.Renviron` or `Rprofile.site` file. (Note: Environment variables are more secure than the `credentials()` function or `options("MTurkR.keypair")` from earlier versions of MTurkR.)

As a supplemental feature to MTurkR, there is also a graphical user interface available for the package. A simple, command-line version of this is available using the `wizard.simple()` function in MTurkR. A more advanced GUI, built using Tcl/Tk, is available in [**MTurkRGUI**](https://github.com/cloudyr/MTurkRGUI) package.

If you experience problems using MTurkR, you can:
  
  - [Report issues on GitHub](https://github.com/cloudyr/MTurkR/issues)
  - Contact the package maintainer [via email](mailto:thosjleeper@gmail.com) or on [Twitter](https://twitter.com/thosjleeper)

## Installation ##

[![CRAN Version](https://www.r-pkg.org/badges/version/MTurkR)](https://cran.r-project.org/package=MTurkR)
![Downloads](https://cranlogs.r-pkg.org/badges/MTurkR)
[![Travis-CI Build Status](https://travis-ci.org/cloudyr/MTurkR.png?branch=master)](https://travis-ci.org/cloudyr/MTurkR)
[![codecov.io](https://codecov.io/github/cloudyr/MTurkR/coverage.svg?branch=master)](https://codecov.io/github/cloudyr/MTurkR?branch=master)

To install the latest version from CRAN, simply use:

```R
install.packages("MTurkR")
```

To install the latest development version of **MTurkR** from GitHub:

```R
# latest stable version
install.packages("MTurkR", repos = c(getOption("repos"), "http://cloudyr.github.io/drat"))

# latest (unstable) version from GitHub
if (!require("remotes")) {
    install.packages("remotes")
}
remotes::install_github("cloudyr/MTurkR")
```


[![cloudyr project logo](http://i.imgur.com/JHS98Y7.png)](https://github.com/cloudyr)
