---
layout: default
title: MTurkR
ghurl: https://github.com/leeper/MTurkR/tree/gh-pages
---

		<span style="font-weight:bold;font-size:1.8em;">MTurkR: </span>
		<span style="font-style:italic;font-size:1.8em;">Access to Amazon Mechanical Turk Requester API via R</span>
		<p style="font-size:.3em;">&nbsp;</p>
		<p style="font-size:.3em;">&nbsp;</p>
		<p style="font-style:italic;font-size:1.2em;">Package Information</p>
			<ul>
				<li style="font-weight:bold;">
					<a href="http://cran.r-project.org/web/packages/MTurkR/index.html">Download the latest release from CRAN</a></li>
				<li style="font-weight:bold;">
					<a href="https://github.com/leeper/MTurkR">Find the latest development version on GitHub</a></li>
				<li>Package is currently in beta release 0.3.5 (as of March, 2013)</li>
				<li>MTurkR was built under R-2.15.3 for Windows, but should be platform-independent</li>
				<li><a href="MTurkR-manual.pdf">Download package manual (.pdf)</a></li>
				
			</ul>
		<p style="font-style:italic;font-size:1.2em;">Features</p>
			<ul>
				<li>Free, open-source (<a href="http://www.gnu.org/licenses/gpl-2.0.html">GPL-2</a>)</li>
				<li>Access to full functionality of MTurk (more functionality than Requester UI or MTurk command-line tools)</li>
				<li>A sophisticated graphical user interface (GUI) for managing most MTurk operations through R</li>
				<li>Access, approve, and reject assignments and automatically convert MTurk assignment data to R dataframes</li>
				<li>Create and manage Qualifications and assign them to MTurk workers in a variety of ways</li>
				<ul>
					<li>Here is some example <a href="qualification_test_example.r">R code</a> and associated .xml files (<a href="questionform_example.xml">1</a>,<a href="answerkey_example.xml">2</a>) for creating a Qualification Test and AnswerKey.</li>
				</ul>
				<li>Pay bonuses, contact workers via email, and restrict HITs (to manage panels, for example)</li>
				<li>Automatically log all MTurk requests for later reference (in a .tsv in your working directory)</li>
				<li>Access to live MTurk server and MTurk Sandbox for practicing with code</li>
			</ul>
		<p style="font-style:italic;font-size:1.2em;">Getting Help</p>
			<ul>
				<li>Visit the <a href="http://mechanicalturk.typepad.com/">Amazon Mechanical Turk Blog</a></li>
				<li>For help from workers or to advertise HITs/recruit workers, visit <a href="http://mturkforum.com/">MTurk Forum</a></li>
				<li>For feedback on you and your HITs from workers, visit <a href="http://turkopticon.differenceengines.com/">TurkOpticon</a></li>
				<li>Check out the <a href="http://cran.r-project.org/web/packages/MTurkR/MTurkR.pdf">MTurkR documentation</a> on CRAN</li>
				<li>Post your question on <a href="http://stackoverflow.com/questions/tagged/mechanicalturk+r">Stack Exchange</a> or the <a href="https://forums.aws.amazon.com/forum.jspa?forumID=11">AWS Developer Forum</a></li>
				<li>Comments, feedback, bug reports, and suggestions on MTurkR are also welcome <a href="mailto:thosjleeper@gmail.com?subject=MTurkR">via email</a> and <a href="https://github.com/leeper/MTurkR/issues">on GitHub</a></li>
				<li>You can also follow the development of MTurkR on <a href="https://github.com/leeper/MTurkR">GitHub</a></li>
			</ul>
		<p style="font-style:italic;font-size:1.2em;">Some helpful JavaScript-enhanced HTML for running research studies on MTurk</p>
			<p style="margin-left:1em;">Below code can be used and modified in order to randomize or restrict access to HITs. Some of the code may be useful in combination and is presented here as simple examples. The code is made available here to assist requesters in streamlining the HIT creation process (e.g., piping the WorkerId to an off-site tool, randomizing) and avoid common mistakes (e.g., allowing workers to complete multiple HITs, constructing multiple separate assignments for conditions because randomization is thought impossible, rejecting workers for completing studies they have worked on previously, and so forth).</p>
			<ul>
				<li><a href="javascript/mturk-urlpipe.txt">urlpipe</a> 
					(<a href="javascript/mturk-urlpipe.html">view as .html</a>): 
					Pipe MTurk parameters (AssignmentId, HITId, WorkerId) to off-site survey tool</li>
				<li><a href="javascript/mturk-randomlink.txt">randomlink</a> 
					(<a href="javascript/mturk-randomlink.html">view as .html</a>): 
					Randomize workers to condition (e.g., a link to a survey)</li>
				<li><a href="javascript/mturk-nonrandomlink.txt">nonrandomlink</a> 
					(<a href="javascript/mturk-nonrandomlink.html">view as .html</a>): 
					Nonrandomly assign workers to condition based on MTurk WorkerID (e.g., to create different versions of multi-assignment HITs for different workers)</li>
				<li><a href="javascript/mturk-workercheck.txt">workercheck</a> 
					(<a href="javascript/mturk-workercheck.html">view as .html</a>): 
					Allow workers to check whether they are ineligible to complete a HIT (e.g., because they completed a similar study previously)</li>
				<li><a href="javascript/mturk-softblock.txt">softblock</a> 
					(<a href="javascript/mturk-softblock.html">view as .html</a>): 
					&ldquo;Soft block&rdquo; workers by preventing them from seeing the link to an off-site survey. (Better functionality is provided by the MTurk RUI and API, which can prevent workers from even seeing the HIT in the first place, but this is a relatively simple hack.)</li>
			</ul>
		<!--<p style="font-style:italic;font-size:1.2em;">Contribute Data on Academic Use of MTurk</p>
			<ul>
				<li>Add data regarding workers (e.g., blocked workers, high-quality workers) to a collective database <a href="https://docs.google.com/spreadsheet/viewform?formkey=dDZHQU5aRm95X05UenVaXzQ4UUFYU1E6MQ#gid=0">here</a></li>
				<li>Data can be downloaded using the <span style="font-family:monospace;font-size:.7em;">DownloadWorkerData()</span> function</li>
				<li>More sophisticated data sharing is currently under development</li>
			</ul>
		-->
		<br/>