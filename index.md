---
layout: default
title: MTurkR
ghurl: https://github.com/leeper/MTurkR/blob/gh-pages
---

# MTurkR #

Amazon Mechanical Turk (MTurk) is a crowdsourcing platform that provides researchers and developers with the ability to leverage human intelligence for tasks that cannot easily be automated. The **MTurkR** package provides access to the [MTurk](https://requester.mturk.com/) [Requester API](http://docs.aws.amazon.com/AWSMechTurk/latest/AWSMturkAPI/Welcome.html) via authenticated HTTP requests executed with [RCurl](http://cran.r-project.org/web/packages/RCurl/index.html) and a number of functions to translate API calls into R data structures (using the [XML package](http://cran.r-project.org/web/packages/XML/index.html)).

You can [download the latest package release from CRAN](http://cran.r-project.org/web/packages/MTurkR/index.html) and find [a PDF of the package manual here](http://cran.r-project.org/web/packages/MTurkR/MTurkR.pdf).

You can also find [the latest development version of the package on GitHub](https://github.com/leeper/MTurkR).

Comments, feedback, bug reports, and suggestions on MTurkR are also welcome [via email](mailto:thosjleeper@gmail.com?subject=MTurkR) and [on GitHub](https://github.com/leeper/MTurkR/issues)

---
## Features ##

* Free, open-source ([GPL-2](http://www.gnu.org/licenses/gpl-2.0.html))
* MTurkR was built under R for Windows, but should be platform-independent
* Access to full functionality of MTurk (more functionality than Requester UI or MTurk command-line tools)
* A sophisticated graphical user interface (GUI) for managing most MTurk operations through R
* Access, approve, and reject assignments and automatically convert MTurk assignment data to R dataframes
* Create and manage Qualifications and assign them to MTurk workers in a variety of ways (see some example [R code]({{ page.ghurl }}/qualification_test_example.r) and associated .xml files ([1]({{ page.ghurl }}/questionform_example.xml), [2]({{ page.ghurl }}/answerkey_example.xml)) for creating a Qualification Test and AnswerKey).
* Pay bonuses, contact workers via email, and restrict HITs (to manage panels, for example)
* Automatically log all MTurk requests for later reference (in a .tsv in your working directory, or another location)
* Access to live MTurk server and MTurk Sandbox for practicing with code

---
## Getting Help ##

* Visit the [Amazon Mechanical Turk Blog](http://mechanicalturk.typepad.com/)
* For help from workers or to advertise HITs/recruit workers, visit [MTurk Forum](http://mturkforum.com/)
* For feedback on you and your HITs from workers, visit [TurkOpticon](http://turkopticon.differenceengines.com/)
* Check out the [MTurkR documentation](http://cran.r-project.org/web/packages/MTurkR/MTurkR.pdf) on CRAN
* Post your question on [Stack Exchange](http://stackoverflow.com/questions/tagged/mechanicalturk+r) or the [AWS Developer Forum](https://forums.aws.amazon.com/forum.jspa?forumID=11)
* Comments, feedback, bug reports, and suggestions on MTurkR are also welcome [via email](mailto:thosjleeper@gmail.com?subject=MTurkR) and [on GitHub](https://github.com/leeper/MTurkR/issues)
* You can also follow the development of MTurkR on [GitHub](https://github.com/leeper/MTurkR)

---
## MTurkR Code Examples ##

* Examples in an introductory article: "[Crowdsourcing with R and the MTurk API](http://polmeth.wustl.edu/methodologist/tpm_v20_n2.pdf)" in *The Political Methodologist* 20(1):2-7.
* Examples from a blog post by Solomon Messing: "[Streamline Your Mechanical Turk Workflow with MTurkR](http://solomonmessing.wordpress.com/2013/06/24/streamline-your-mechanical-turk-workflow-with-mturkr/)"
* Sample code to [create and manage qualifications]({{ page.ghurl }}/qualification_test_example.r), with associated xml files for [QuestionForm]({{ page.ghurl }}/questionform_example.xml) and [AnswerKey]({{ page.ghurl }}/answerkey_example.xml)
* Workflow and sample code to [mimic the Requester UI's display of input HITLayout parameters with assignment results](/batchinputs).

---
## JavaScript-enhanced HTML code for MTurk ##

Below code can be used and modified in order to randomize or restrict access to HITs. Some of the code may be useful in combination and is presented here as simple examples. The code is made available here to assist requesters in streamlining the HIT creation process (e.g., piping the WorkerId to an off-site tool, randomizing) and avoid common mistakes (e.g., allowing workers to complete multiple HITs, constructing multiple separate assignments for conditions because randomization is thought impossible, rejecting workers for completing studies they have worked on previously, and so forth).

* [urlpipe]({{ page.ghurl }}/javascript/mturk-urlpipe.html) ([view as .html](javascript/mturk-urlpipe.html)): Pipe MTurk parameters (AssignmentId, HITId, WorkerId) to off-site survey tool
* [randomlink]({{ page.ghurl }}/javascript/mturk-randomlink.html) ([view as .html](javascript/mturk-randomlink.html)): Randomize workers to condition (e.g., a link to a survey)
* [nonrandomlink]({{ page.ghurl }}/javascript/mturk-nonrandomlink.html) ([view as .html](javascript/mturk-nonrandomlink.html)): Nonrandomly assign workers to condition based on MTurk WorkerID (e.g., to create different versions of multi-assignment HITs for different workers)
* [workercheck]({{ page.ghurl }}/javascript/mturk-workercheck.html) ([view as .html](javascript/mturk-workercheck.html)): Allow workers to check whether they are ineligible to complete a HIT (e.g., because they completed a similar study previously)
* [softblock]({{ page.ghurl }}/javascript/mturk-softblock.html) ([view as .html](javascript/mturk-softblock.html)): &ldquo;Soft block&rdquo; workers by preventing them from seeing the link to an off-site survey. (Better functionality is provided by the MTurk RUI and API, which can prevent workers from even seeing the HIT in the first place, but this is a relatively simple hack.)
