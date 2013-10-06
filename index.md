---
layout: default
title: MTurkR
ghurl: https://github.com/leeper/MTurkR/blog/gh-pages
---

# MTurkR: Access to Amazon Mechanical Turk Requester API via R #

## Package Information ##

* [Download the latest release from CRAN](http://cran.r-project.org/web/packages/MTurkR/index.html)
* [Find the latest development version on GitHub](https://github.com/leeper/MTurkR)
* MTurkR was built under R for Windows, but should be platform-independent
* [Download package manual (.pdf)](MTurkR-manual.pdf)

## Features ##

* Free, open-source ([GPL-2](http://www.gnu.org/licenses/gpl-2.0.html))
* Access to full functionality of MTurk (more functionality than Requester UI or MTurk command-line tools)
* A sophisticated graphical user interface (GUI) for managing most MTurk operations through R
* Access, approve, and reject assignments and automatically convert MTurk assignment data to R dataframes
* Create and manage Qualifications and assign them to MTurk workers in a variety of ways

  * Here is some example [R code](qualification_test_example.r) and associated .xml files ([1](questionform_example.xml), [2](answerkey_example.xml)) for creating a Qualification Test and AnswerKey.

* Pay bonuses, contact workers via email, and restrict HITs (to manage panels, for example)
* Automatically log all MTurk requests for later reference (in a .tsv in your working directory, or another location)
* Access to live MTurk server and MTurk Sandbox for practicing with code

## Getting Help ##

* Visit the <a href="http://mechanicalturk.typepad.com/">Amazon Mechanical Turk Blog</a>
* For help from workers or to advertise HITs/recruit workers, visit [MTurk Forum](http://mturkforum.com/)
* For feedback on you and your HITs from workers, visit [TurkOpticon](http://turkopticon.differenceengines.com/)
* Check out the [MTurkR documentation](http://cran.r-project.org/web/packages/MTurkR/MTurkR.pdf) on CRAN
* Post your question on [Stack Exchange](http://stackoverflow.com/questions/tagged/mechanicalturk+r) or the [AWS Developer Forum](https://forums.aws.amazon.com/forum.jspa?forumID=11)
* Comments, feedback, bug reports, and suggestions on MTurkR are also welcome [via email](mailto:thosjleeper@gmail.com?subject=MTurkR) and [on GitHub](https://github.com/leeper/MTurkR/issues)
* You can also follow the development of MTurkR on <a href="https://github.com/leeper/MTurkR">GitHub</a><

## Some useful JavaScript-enhanced HTML code ##

Below code can be used and modified in order to randomize or restrict access to HITs. Some of the code may be useful in combination and is presented here as simple examples. The code is made available here to assist requesters in streamlining the HIT creation process (e.g., piping the WorkerId to an off-site tool, randomizing) and avoid common mistakes (e.g., allowing workers to complete multiple HITs, constructing multiple separate assignments for conditions because randomization is thought impossible, rejecting workers for completing studies they have worked on previously, and so forth).

* [urlpipe]({{ page.ghurl }}/javascript/mturk-urlpipe.html) ([view as .html](javascript/mturk-urlpipe.html)): Pipe MTurk parameters (AssignmentId, HITId, WorkerId) to off-site survey tool
* [randomlink]({{ page.ghurl }}/javascript/mturk-randomlink.html) ([view as .html](javascript/mturk-randomlink.html)): Randomize workers to condition (e.g., a link to a survey)
* [nonrandomlink]({{ page.ghurl }}/javascript/mturk-nonrandomlink.html) ([view as .html](javascript/mturk-nonrandomlink.html)): Nonrandomly assign workers to condition based on MTurk WorkerID (e.g., to create different versions of multi-assignment HITs for different workers)
* [workercheck]({{ page.ghurl }}/javascript/mturk-workercheck.html) ([view as .html](javascript/mturk-workercheck.html)): Allow workers to check whether they are ineligible to complete a HIT (e.g., because they completed a similar study previously)
* [softblock]({{ page.ghurl }}/javascript/mturk-softblock.html) ([view as .html](javascript/mturk-softblock.html)): &ldquo;Soft block&rdquo; workers by preventing them from seeing the link to an off-site survey. (Better functionality is provided by the MTurk RUI and API, which can prevent workers from even seeing the HIT in the first place, but this is a relatively simple hack.)
