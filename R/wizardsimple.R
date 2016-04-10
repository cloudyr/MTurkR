wizard.simple <-
function (graphics = FALSE, sandbox = NULL, ...) 
{
    message("MTurkR Wizard loading...\n")
    log.requests <- TRUE
    internet.test <- try(curl_fetch_memory(url="http://www.example.com"), silent = TRUE)
    if (class(internet.test) == "try-error") {
        message("An internet connection does not appear to be available!\n")
    }
    setKeypair <- function() {
        message("Retrieve your AWS access keys from https://aws-portal.amazon.com/gp/aws/securityCredentials")
        accesskey <- readline(prompt = "AWS/MTurk Access Key ID: ")
        secretkey <- readline(prompt = "AWS/MTurk Secret Access Key: ")
        Sys.setenv("AWS_ACCESS_KEY_ID" = accesskey)
        Sys.setenv("AWS_SECRET_ACCESS_KEY" = secretkey)
        return(NULL)
    }
    accesskey <- Sys.getenv("AWS_ACCESS_KEY_ID")
    secretkey <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
    if (accesskey == "") {
        setKeypair()
    }
    else {
        message("Your AWS/MTurk Access Key ID is: ", accesskey)
        message("Your AWS/MTurk Secret Access Key is: ", secretkey)
        resetkeys <- readline(prompt = "Set new credentials? (Y/N): ")
        if (tolower(resetkeys) %in% c("y", "ye", "yes", "true", "1", TRUE)) {
            setKeypair()
        } 
    }
    if (is.null(sandbox)) {
        sandbox <- readline(prompt = "Use Sandbox? (Y/N): ")
    }
    if (tolower(sandbox) %in% c("y", "ye", "yes", "true", "1", TRUE)) {
        options(MTurkR.sandbox = TRUE)
    } else {
        options(MTurkR.sandbox = FALSE)
    }
    wizard.menu <- function() {
        menu.opts <- c("Check Account Balance", "Check Sufficient Funds", 
            "Create HIT", "Check HIT Status", "Get Assignment(s)", 
            "Extend HIT", "Expire HIT", "Approve Assignment(s)", 
            "Reject Assignment(s)", "Grant Bonus(es)", "Contact Worker(s)", 
            "Block Worker(s)", "Unblock Worker(s)", "Manage Qualifications", 
            "Requester Statistics", "Worker Statistics", "Open MTurk RUI Pages", 
            "Load MTurkR Log File/Entries", "Exit")
        selection <- menu(menu.opts, 
                          graphics = graphics,
                          title = "MTurkR Operations")
        choice <- selection
        if (choice == 0) 
            choice <- 20
        else if (choice == 1) {
            balance <- try(AccountBalance(verbose = TRUE, ...), silent = TRUE)
            if (class(balance) == "try-error") 
                warning("An error occurred: ", balance)
            message()
            wizard.menu()
        }
        else if (choice == 2) {
            pay <- readline("How much do you want to pay per assignment (in US Dollars): ")
            hit <- readline("How many HITs do you plan to offer (usually 1): ")
            number <- readline("How many assignments per HIT: ")
            funds <- try(SufficientFunds(amount = pay, assignments = number, 
                         hits = hit, verbose = TRUE, ...), silent = TRUE)
            if(class(funds) == "try-error") 
                warning("An error occurred: ", funds)
            else
                print(funds)
            message()
            wizard.menu()
        }
        else if (choice == 3) {
            message("Enter HIT Information below:")
            title <- readline(prompt = "HIT Title (Workers will see this): ")
            description <- readline(prompt = "HIT Description (Workers will see this): ")
            keywords <- readline(prompt = "HIT Keywords (Workers will see this): ")
            reward <- readline(prompt = "HIT Reward (Amount to pay workers per assignment in US Dollars): ")
            duration <- readline(prompt = "HIT Duration (How long HIT should be available, in seconds): ")
            question.type <- menu(c("MTurk RUI HITLayoutId", "External HTML Question"), 
                                  graphics = graphics, 
                                  title = "How do you want load question data?")
            if (question.type == 0) 
                wizard.menu()
            else if (question.type == 1) {
                message("Retrieve HIT LayoutId from https://requester.mturk.com/hit_templates")
                layoutid <- readline(prompt = "HIT LayoutId: ")
            }
            else if (question.type == 2) {
                message("Enter URL for External HIT")
                external.url <- readline(prompt = "URL: ")
                question <- GenerateExternalQuestion(external.url, 400)
            }
            quals <- menu(c("Yes", "No"), 
                          graphics = graphics,
                          title = "Do you want to restrict the HIT with a QualificationRequirement?")
            if (quals == 1) {
                qual.types <- as.data.frame(rbind(
                    c("Worker_PercentAssignmentsSubmitted", "00000000000000000000"),
                    c("Worker_PercentAssignmentsAbandoned", "00000000000000000070"),
                    c("Worker_PercentAssignmentsReturned", "000000000000000000E0"), 
                    c("Worker_PercentAssignmentsApproved", "000000000000000000L0"), 
                    c("Worker_PercentAssignmentsRejected", "000000000000000000S0"), 
                    c("Worker_NumberHITsApproved", "00000000000000000040"), 
                    c("Worker_Locale", "00000000000000000071"),
                    c("Worker_Adult", "00000000000000000060"), 
                    c("Categorization Masters (Sandbox)", "2F1KVCNHMVHV8E9PBUB2A4J79LU20F"),
                    c("Categorization Masters (Production)", "2NDP2L92HECWY8NS8H3CK0CP5L9GHO"),
                    c("Photo Moderation Masters (Sandbox)", "2TGBB6BFMFFOM08IBMAFGGESC1UWJX"),
                    c("Photo Moderation Masters (Production)", "21VZU98JHSTLZ5BPP4A9NOBJEK3DPG"),
                    c("Other", "")))
                names(quals) <- c("Qualification", "QualificationTypeId")
                qual.to.add <- menu(qual.types$Qualification, 
                  graphics = graphics,
                  title = "Which QualificationRequirement would you like to add?")
                if (qual.to.add %in% c(1:dim(qual.types[1]))) 
                  qual.to.add <- qual.types$QualificationTypeId[qual.to.add]
                else qualreqs <- NULL
            }
            else qualreqs <- NULL
            hit <- try(CreateHIT(title = title, description = description, 
                reward = reward, duration = duration, keywords = keywords, 
                qual.req = qualreqs, verbose = TRUE, ...), silent = TRUE)
            if (class(hit) == "try-error") 
                warning("An error occurred: ", hit)
            else
                print(hit)
            message()
            wizard.menu()
        }
        else if (choice == 4) {
            hittocheck <- readline(prompt = "HITId to Check: ")
            status <- try(HITStatus(hit = hittocheck, verbose = TRUE, ...), 
                silent = TRUE)
            if (class(status) == "try-error") 
                warning("An error occurred: ", status)
            else
                print(status)
            message()
            wizard.menu()
        }
        else if (choice == 5) {
            assign.or.hit <- menu(c("AssignmentId", "HITId"), 
                graphics = graphics,
                title = "Get Assignments by AssignmentId or HITId")
            if (assign.or.hit == 0) 
                wizard.menu()
            else if (assign.or.hit == 1) {
                id <- readline("Enter one AssignmentId: ")
                assignment <- try(GetAssignment(assignment = id, verbose = TRUE, ...),
                                  silent = TRUE)
                if (class(assignment) == "try-error") 
                  warning("An error occurred: ", assignment)
                else print(assignment)
            }
            else if (assign.or.hit == 2) {
                id <- readline("Enter one HITId: ")
                assignment <- try(GetAssignment(hit = id, verbose = TRUE, ...),
                                  silent = TRUE)
                if (class(assignment) == "try-error") 
                  warning("An error occurred: ", assignment)
                else print(assignment)
            }
            message()
            wizard.menu()
        }
        else if (choice == 6) {
            hittoextend <- readline(prompt = "HITId to Extend: ")
            action <- menu(c("Add Time", "Add Assignments", "Add Time and Assignments"), 
                graphics = graphics,
                title = "Add Time or Assignments to HIT?")
            addtime <- NULL
            addunits <- NULL
            if (action == 0) 
                wizard.menu()
            else if (action %in% c(1, 3)) 
                addtime <- readline("How many seconds would you like to extend this HIT? ")
            else if (action %in% c(2, 3)) 
                addunits <- readline("How many assignments would you like to add? ")
            extend <- try(ExtendHIT(hit = hittocheck, add.assignments = addunits, 
                add.seconds = addtime, verbose = TRUE, ...), 
                silent = TRUE)
            if (class(extend) == "try-error") 
                warning("An error occurred: ", extend)
            message()
            wizard.menu()
        }
        else if (choice == 7) {
            hittoexpire <- readline(prompt = "HITId to Expire: ")
            expire <- try(ExpireHIT(hit = hittoexpire, verbose = TRUE, ...), 
                          silent = TRUE)
            if (class(expire) == "try-error") 
                warning("An error occurred: ", expire)
            message()
            wizard.menu()
        }
        else if (choice == 8) {
            assign.ct <- menu(c("One Assignment", "Multiple Assignments", "All Assignments for HIT"), 
                graphics = graphics,
                title = "Approve one assignment or multiple assignments?")
            if (assign.ct == 0) {
                wizard.menu()
            } else if (assign.ct == 1) {
                assignment <- readline(prompt = "AssignmentId to Approve: ")
                approve <- try(ApproveAssignment(assignments = assignment, 
                                                 verbose = TRUE, ...), silent = TRUE)
                if (class(approve) == "try-error") 
                    warning("An error occurred: ", approve)
                else
                    print(approve)
            } else if (assign.ct == 2) {
                count <- as.numeric(readline(prompt = "How many assignments to approve: "))
                message("Enter each AssignmentId on own line:")
                assignment <- scan(n = count, what = "character")
                approve <- try(ApproveAssignment(assignments = assignment, 
                                                 verbose = TRUE, ...), silent = TRUE)
                if (class(approve) == "try-error") 
                    warning("An error occurred: ", approve)
                else
                    print(approve)
            } else if (assign.ct == 3) {
                hittoapprove <- readline(prompt = "HITId to Approve: ")
                approve <- try(ApproveAllAssignments(hit = hittoapprove, 
                                                 verbose = TRUE, ...), silent = TRUE)
                if (class(approve) == "try-error") 
                    warning("An error occurred: ", approve)
                else
                    print(approve)
            }
            message()
            wizard.menu()
        }
        else if (choice == 9) {
            assign.ct <- menu(c("One Assignment", "Multiple Assignments"), 
                graphics = graphics,
                title = "Reject one assignment or multiple assignments?")
            if (assign.ct == 0) 
                wizard.menu()
            else if (assign.ct == 1) {
                assignment <- readline(prompt = "AssignmentId to Reject: ")
                reason <- readline(prompt = "Reason to Reject Assignment: ")
            }
            else if (assign.ct == 2) {
                reason <- readline(prompt = "Reason for rejection: ")
                count <- as.numeric(readline(prompt = "How many assignments to reject: "))
                message("Enter each AssignmentId on own line:")
                assignment <- scan(n = count, what = "character")
            }
            reject <- try(RejectAssignment(assignment, reason, verbose = TRUE, ...), 
                          silent = TRUE)
            if (class(reject) == "try-error") 
                warning("An error occurred: ", reject)
            else print(reject)
            message()
            wizard.menu()
        }
        else if (choice == 10) {
            bonus.ct <- menu(c("Single Worker", "Multiple Workers"), 
                graphics = graphics,
                title = "Bonus one worker or multiple workers?")
            if (bonus.ct == 0) 
                wizard.menu()
            else if (bonus.ct == 1) {
                worker <- readline(prompt = "WorkerId to Bonus: ")
                assignment <- readline(prompt = "AssignmentId to Bonus: ")
                amount <- readline(prompt = "Amount of Bonus in US Dollars: ")
                reason <- readline(prompt = "Reason for Bonus: ")
            }
            else if (bonus.ct == 2) {
                amount <- readline(prompt = "Amount of Bonus in US Dollars: ")
                reason <- readline(prompt = "Reason for Bonus: ")
                count <- as.numeric(readline(prompt = "How many bonuses to pay: "))
                message("Enter each WorkerId on own line:")
                worker <- scan(n = count, what = "character")
                message("Enter corresponding AssignmentId for each Worker on own line:")
                assignment <- scan(n = count, what = "character")
            }
            bonus <- try(GrantBonus(workers = worker, assignments = assignment, 
                         amounts = amount, reasons = reason, verbose = TRUE, ...), 
                         silent = TRUE)
            if (class(bonus) == "try-error") 
                warning("An error occurred: ", bonus)
            else print(bonus)
            message()
            wizard.menu()
        }
        else if (choice == 11) {
            contact.ct <- menu(c("Single Worker", "Multiple Workers"), 
                graphics = graphics,
                title = "Contact one worker or multiple workers?")
            if (contact.ct == 0) 
                wizard.menu()
            else if (contact.ct == 1) {
                worker <- readline(prompt = "WorkerId to Notify: ")
                subject <- readline(prompt = "Email Subject Line: ")
                txt <- readline(prompt = "Email body text: ")
            }
            else if (contact.ct == 2) {
                subject <- readline(prompt = "Email Subject Line: ")
                txt <- readline(prompt = "Email body text: ")
                count <- as.numeric(readline(prompt = "How many workers to notify: "))
                message("Enter each WorkerId on own line:")
                worker <- scan(n = count, what = "character")
            }
            txt <- gsub("\\n","\n",txt, fixed = TRUE)
            txt <- gsub("\\t","\t",txt, fixed = TRUE)
            txtToPrint <- 
                c("Your message will look like this:\n\n",
                "Subject:",subject,"\n\nBody:\n\n",
                "Message from [Your Requester Name]\n",
                "---------------------------------\n",
                txt,
                "\n---------------------------------\n",
                "\n",
                "Greetings from Amazon Mechanical Turk,\n",
                "\n",
                "The message above was sent by an Amazon Mechanical Turk user.\n",
                "Please review the message and respond to it as you see fit.\n",
                "\n",
                "Sincerely,\n",
                "Amazon Mechanical Turk\n",
                "https://workersandbox.mturk.com\n",
                "410 Terry Avenue North\n",
                "SEATTLE, WA 98109-5210 USA\n")
            message(txtToPrint)
            continue <- menu(c("Accept Message and Proceed", "Cancel and Return to Main Menu"), 
                graphics = graphics,
                title = "What would you like to do?")
            if(continue == 1) {
                notify <- try(ContactWorkers(subjects = subject, 
                                             msgs = txt,
                                             workers = worker, 
                                             verbose = FALSE, ...), 
                              silent = TRUE)
                if (class(notify) == "try-error") 
                    warning("An error occurred: ", notify)
                else
                    print(notify)
            }
            message()
            wizard.menu()
        }
        else if (choice == 12) {
            message("Which worker do you want to block?")
            workerid <- readline(prompt = "WorkerId: ")
            reason <- readline(prompt = "Reason for block: ")
            block <- try(BlockWorker(workerid, reason, verbose = TRUE, ...), 
                silent = TRUE)
            if (class(block) == "try-error") 
                warning("An error occurred: ", block)
            message()
            wizard.menu()
        }
        else if (choice == 13) {
            message("Which worker do you want to unblock?")
            workerid <- readline(prompt = "WorkerId: ")
            reason <- readline(prompt = "Reason for unblock: ")
            unblock <- try(UnblockWorker(workerid, reason, verbose = TRUE, ...), 
                silent = TRUE)
            if (class(unblock) == "try-error") 
                warning("An error occurred: ", unblock)
            message()
            wizard.menu()
        }
        else if (choice == 14) {
            qual.opts <- c("Get Worker(s) By Qualification", 
                "Assign a Qualification", "Get a Qualification Score", 
                "Update Worker(s) Qualification Score", 
                "Create a QualificationType", "Update a QualificationType", 
                "View a QualificationType", "Search QualificationTypes", 
                "List Built-In QualificationTypes")
            qual.choice <- menu(qual.opts,
                                graphics = graphics,
                                title = "What would you like to do?")
            if (qual.choice == 0) {
                wizard.menu()
            } else if (qual.choice == 1) {
                message("For which QualificationType do you want to retrieve workers?")
                qual <- readline(prompt = "QualificationTypeId: ")
                if (qual == "") 
                  wizard.menu()
                getqual <- try(GetQualifications(qual, verbose = TRUE, ...), silent = TRUE)
                if (class(getqual) == "try-error") 
                    warning("An error occurred: ", getqual)
                else
                    print(getqual)
            } else if (qual.choice == 2) {
                message("Which QualificationType do you want to assign to a worker?")
                qual <- readline(prompt = "QualificationTypeId: ")
                    w.ct <- menu(c("Single Worker", "Multiple Workers"), 
                    graphics = graphics,
                    title = "Assign (same score) to one worker or multiple workers?")
                if (w.ct == 0) {
                    wizard.menu()
                } else if (w.ct == 1) {
                    worker <- readline(prompt = "WorkerId to Notify: ")
                } else if (w.ct == 2) {
                    count <- as.numeric(readline(prompt = "How many workers to notify: "))
                    message("Enter each WorkerId on own line:")
                    worker <- scan(n = count, what = "character")
                    worker <- worker[worker != ""]
                }
                message("What value do you want to assign for the qualification?")
                value <- readline(prompt = "Value: ")
                if (qual == "" | worker == "") 
                    wizard.menu()
                getqual <- try(AssignQualification(qual, worker, value, verbose = TRUE, ...), 
                               silent = TRUE)
                if (class(getqual) == "try-error") 
                    warning("An error occurred: ", getqual)
                else
                    print(getqual)
            } else if (qual.choice == 3) {
                message("For which QualificationType do you want to retrieve workers?")
                qual <- readline(prompt = "QualificationTypeId: ")
                message("For what worker do you want to retrieve the score?")
                worker <- readline(prompt = "WorkerId: ")
                if (qual == "" | worker == "") 
                    wizard.menu()
                getqual <- try(GetQualificationScore(qual, worker, verbose = TRUE, ...),
                               silent = TRUE)
                if (class(getqual) == "try-error") 
                    warning("An error occurred: ", getqual)
                else
                    print(getqual)
            } else if (qual.choice == 4) {
                message("Which QualificationType do you want to update for a worker?")
                qual <- readline(prompt = "QualificationTypeId: ")
                message("For what worker do you want to retrieve the score?")
                worker <- readline(prompt = "WorkerId: ")
                message("What value do you want to assign for the qualification?")
                value <- readline(prompt = "Value: ")
                if (qual == "" | worker == "") 
                    wizard.menu()
                getqual <- try(UpdateQualificationScore(qual, worker, value, verbose = TRUE, ...), 
                               silent = TRUE)
                if (class(getqual) == "try-error") 
                    warning("An error occurred: ", getqual)
                else
                    print(getqual)
            } else if (qual.choice == 5) {
                message("Please enter information for QualificationType below")
                name <- readline(prompt = "Name for QualificationType (Workers can see this): ")
                description <- readline(prompt = "Description QualificationType (Workers can see this): ")
                if (nchar(curl_escape(description)) > 2000) {
                  message("Description must be less than ~2000 characters")
                  description <- readline(prompt = "Description QualificationType (Workers can see this): ")
                }
                keywords <- readline(prompt = "Keywords (enter comma-separated list or leave blank): ")
                if (keywords == "") 
                  keywords <- NULL
                getqual <- try(CreateQualificationType(name, description,
                               status = "Active", keywords, verbose = TRUE, ...), silent = TRUE)
                if (class(getqual) == "try-error") 
                    warning("An error occurred: ", getqual)
                else
                    print(getqual)
            } else if (qual.choice == 6) {
                message("Which QualificationType do you want to update?")
                qual <- readline(prompt = "QualificationTypeId: ")
                update.opts <- c("Description", "Status", "Whether Qualification is Automatically Granted")
                update.choice <- menu(update.opts, 
                                      graphics = graphics,
                                      title = "What parameters do you want to update?")
                if (update.choice == 0) 
                  wizard.menu()
                else if (update.choice == 1) {
                  description <- readline(prompt = "Description QualificationType (Workers can see this): ")
                  getqual <- try(UpdateQualificationType(qual, description, verbose = TRUE, ...), 
                                 silent = TRUE)
                  if (class(getqual) == "try-error") 
                    warning("An error occurred: ", getqual)
                  else
                    print(getqual)
                  message()
                  wizard.menu()
                } else if (update.choice == 2) {
                  status.choice <- menu(c("Active", "Inactive"), 
                    graphics = graphics,
                    title = "Should QualificationType be Active or Inactive?")
                  if (status.choice == 0) 
                    wizard.menu()
                  else if (status.choice == 1) 
                    status.update <- "Active"
                  else if (status.choice == 2) 
                    status.update <- "Inactive"
                  getqual <- try(UpdateQualificationType(qual, status = status.update, 
                                 verbose = TRUE, ...), silent = TRUE)
                  if (class(getqual) == "try-error") 
                    warning("An error occurred: ", getqual)
                  else print(getqual)
                  message()
                  wizard.menu()
                } else if (update.choice == 3) {
                  auto.choice <- menu(c("Yes", "No"), 
                                      graphics = graphics,
                                      title = "Should QualificationType be Automatically Granted?")
                  if (auto.choice == 0) 
                    wizard.menu()
                  else if (auto.choice == 1) {
                    auto.update <- TRUE
                    value.update <- readline("What (integer) value should be automatically assigned? ")
                  }
                  else if (auto.choice == 2) {
                    auto.update <- FALSE
                    value.update <- NULL
                  }
                  getqual <- try(UpdateQualificationType(qual, auto = auto.update, 
                                 auto.value = value.update, verbose = TRUE, ...), silent = TRUE)
                  if (class(getqual) == "try-error") 
                    warning("An error occurred: ", getqual)
                  else print(getqual)
                  message()
                  wizard.menu()
                }
            } else if (qual.choice == 7) {
                message("Which QualificationType do you want to view?")
                qual <- readline(prompt = "QualificationTypeId: ")
                if (qual == "") 
                  wizard.menu()
                getqual <- try(GetQualificationType(qual, verbose = TRUE, ...), silent = TRUE)
                if (class(getqual) == "try-error") 
                  warning("An error occurred: ", getqual)
                else print(getqual)
            } else if (qual.choice == 8) {
                searchq <- menu(c("Only Qualifications I Created", "All Qualifications"), 
                                graphics = graphics,
                                title = "Which Qualifications do you want to search?")
                if (searchq == 0) 
                  wizard.menu()
                else if (searchq == 1) 
                  search <- TRUE
                else if (searchq == 2) 
                  searchq <- FALSE
                getqual <- try(SearchQualificationTypes(only.mine = searchq, 
                               verbose = TRUE, ...), silent = TRUE)
                if (class(getqual) == "try-error") 
                  warning("An error occurred: ", getqual)
                else print(getqual)
            } else if (qual.choice == 9) {
                getqual <- try(ListQualificationTypes(), silent = TRUE)
                if (class(getqual) == "try-error") 
                  warning("An error occurred: ", getqual)
                else print(getqual)
            }
            message()
            wizard.menu()
        }
        else if (choice == 15) {
            periods <- c("OneDay", "SevenDays", "ThirtyDays", "LifeToDate")
            period.choice <- menu(periods, 
                                  graphics = graphics,
                                  title = "For what period do you want Requester Statistics?")
            if (period.choice == 0) 
                wizard.menu()
            else {
                statistics <- try(RequesterReport(period = periods[period.choice], ...), silent = TRUE)
                print(statistics)
                if (class(statistics) == "try-error") 
                  warning("An error occurred: ", statistics)
                message()
                wizard.menu()
            }
        }
        else if (choice == 16) {
            message("For which worker do you want statistics?")
            workerid <- readline(prompt = "WorkerId: ")
            periods <- c("OneDay", "SevenDays", "ThirtyDays", "LifeToDate")
            period.choice <- menu(periods, 
                                  graphics = graphics,
                                  title = "For what period do you want Worker Statistics?")
            if (period.choice == 0) 
                wizard.menu()
            else {
                statistics <- try(WorkerReport(worker = workerid, 
                                  period = periods[period.choice], ...), silent = TRUE)
                print(statistics)
                if (class(statistics) == "try-error") 
                  warning("An error occurred: ", statistics)
                message()
                wizard.menu()
            }
        }
        else if (choice == 17) {
            rui.page <- menu(c("Worker Page", "HIT Management Page", 
                "Qualifications", "API Reference", "Available HITs"), 
                graphics = graphics,
                title = "Which page do you want to open? ")
            if (rui.page == 0) 
                wizard.menu()
            else if (rui.page == 1) {
                page.id <- readline("Specify WorkerId or leave blank: ")
                if (page.id == "") 
                  page.id <- NULL
                OpenWorkerPage(page.id)
            }
            else if (rui.page == 2) {
                page.id <- readline("Specify HITId or leave blank: ")
                if (page.id == "") 
                  page.id <- NULL
                OpenManageHITPage(page.id)
            }
            else if (rui.page == 3) {
                OpenQualificationPage()
            }
            else if (rui.page == 5) {
                page.id <- readline("Specify search terms or leave blank: ")
                ViewAvailableHITs(page.id)
            }
            message()
            wizard.menu()
        }
        else if (choice == 18) {
            if (file.exists("MTurkRLog.tsv")) 
                mturkrlog <- readlogfile()
            else {
                logfile <- readline(prompt = "Filename: ")
                mturkrlog <- readlogfile(filename = logfile)
            }
            log.opts <- c("View most recent MTurk response", 
                "View another MTurk response")
            log.choice <- menu(log.opts, graphics = graphics, title = "What to do next?")
            if (log.choice == 0) 
                wizard.menu()
            else if (log.choice == 1) 
                print(xmlParse(mturkrlog$Response[dim(mturkrlog)[1]]))
            else if (log.choice == 2) {
                num.entries <- dim(mturkrlog)[1]
                last.ten <- c(mturkrlog$Operation[num.entries:(num.entries - 
                  9)], "Specify a Log Entry")
                entry <- menu(last.ten, 
                              graphics = graphics,
                              title = "Pick from 10 most recent requests or choose 'Specific Entry'")
                if (entry == 0) 
                  wizard.menu()
                else if (entry == 11) {
                  message("Specify an log entry number between 1 and ",num.entries)
                  entry.number <- readline("Entry number: ")
                  print(xmlParse(mturkrlog$Response[as.numeric(entry.number)]))
                }
                else {
                  entry.number <- num.entries - (entry - 1)
                  print(xmlParse(mturkrlog$Response[as.numeric(entry.number)]))
                }
            }
            message()
            wizard.menu()
        }
        if (choice == 19) {
            message("Thanks for using the MTurkR Wizard!\n")
            invisible()
        }
    }
    wizard.menu()
}
