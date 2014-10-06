MTurkR.Wizard <-
mturkr.wizard <-
function(style="tcltk", sandbox=getOption('MTurkR.sandbox')) {
    if(style=="simple"){
        wizard.simple(graphics=FALSE, sandbox=sandbox)
    }
    else if(style=="simpleGUI"){
        wizard.simple(graphics=TRUE, sandbox=sandbox)
    }
    else { # by default, style=="tcltk"
        
        # setup some things for the wizard
        # temporary environment to store things when I want to use them across functions
        wizardenv <- new.env()
        
        # exit wizard procedure
        exitWiz <- function() {
            exit <- tkmessageBox(message = "Are you sure you want to exit the wizard?", icon = "question", type = "yesno", default = "yes")
            if(tclvalue(exit)=="yes"){
                tkdestroy(wizard)
                bringToTop(-1)
            }
            else
                tkfocus(wizard)
        }
        
        ##----------------------##
        ## FUNCTIONS FOR WIZARD ##
        ##----------------------##
        
        # sandbox setting
        sandboxWiz <- function(){
            sandboxQ <- tkmessageBox(message="Use MTurk Requester Sandbox (for practice)?", type="yesno", icon="question", default="no")
            if(tclvalue(sandboxQ)=="yes")
                sandbox <<- TRUE    
            if(tclvalue(sandboxQ)=="no")
                sandbox <<- FALSE
            tkfocus(wizard)
        }
            
        # enter credentials
        credentialsWiz <- function() {
            credOK <- function() {
                options(MTurkR.keypair = c(tclvalue(accesskey),tclvalue(secretkey)))
                tkdestroy(credDialog)
                tkfocus(wizard)
            }
            # layout
            credDialog <- tktoplevel()
            tkwm.title(credDialog, "Enter/Confirm MTurk Requester Credentials")
            tkfocus(credDialog)
            if(is.null(getOption('MTurkR.keypair'))){
                accesskey <- tclVar("")
                secretkey <- tclVar("")
            } else {
                accesskey <- tclVar(getOption('MTurkR.keypair')[1])
                secretkey <- tclVar(getOption('MTurkR.keypair')[2])
            }
            entryform <- tkframe(credDialog, relief="groove", borderwidth=2)
                accesskey.entry <- tkentry(credDialog, width = 50, textvariable=accesskey)
                secretkey.entry <- tkentry(credDialog, width = 50, textvariable=secretkey)
                tkgrid(tklabel(entryform, text = "       "))
                tkgrid(tklabel(entryform, text = "MTurk Access Key ID"), accesskey.entry)
                tkgrid(tklabel(entryform, text = "MTurk Secret Key"), secretkey.entry)
                tkgrid(tklabel(entryform, text = "       "))
            tkgrid(entryform)
            # buttons
            buttons <- tkframe(credDialog)
                r <- 1
                OKbutton <- tkbutton(buttons, text = "   OK   ", command = credOK)
                Cancelbutton <- tkbutton(buttons, text = " Cancel ", command = function(){tkdestroy(credDialog); tkfocus(wizard)})
                tkgrid(OKbutton, row = r, column = 1)
                tkgrid(Cancelbutton, row=r, column = 2)
            tkgrid(buttons)
            
            tkfocus(credDialog)
        }
        
        # function to save file (e.g., assignment data)
        savetofile <- function(object) {
            filename <- tclvalue(tkgetSaveFile(initialfile="MTurkRAssignmentData.csv", 
                                                filetypes="{{Comma-separated values} {.csv}} {{Tab-separated values} {.tsv}} {{All files} *}"))
            if (!nchar(filename))
                tkmessageBox(message="No file was selected!")
            else {
                n <- nchar(filename)
                ext <- substring(filename,n-2,n)
                if(ext=="csv")
                    write.csv(object, file=filename)
                if(ext=="tsv")
                    write.table(object, file=filename, sep="\t")
                else
                    write.csv(object, file=filename)
            }
        }
        
        
        # check balance
        balanceWiz <- function() {
            tempbal <- as.numeric(AccountBalance(verbose=FALSE, sandbox=sandbox)) # MTurkR getbalance() function
            tkmessageBox(title = "Current Account Balance", message = paste("Balance: $",round(tempbal,2),sep=""), type="ok")
            tkfocus(wizard)
        }
        
        # sufficient funds
        fundcheckWiz <- function() {
            # function
            checksufficient <- function(){
                # convert `masters` variable
                if(tclvalue(masters)=="0")
                    mastercheck <- FALSE
                else
                    mastercheck <- TRUE
                funds <- SufficientFunds(amount=tclvalue(amt),
                                         assignments=tclvalue(assignct),
                                         hits=tclvalue(hitct),
                                         bonus.ct=tclvalue(bonusct),
                                         bonus.amount=tclvalue(bonusamt),
                                         masters=mastercheck,
                                         verbose=FALSE, sandbox = sandbox)
                # report results
                results <- tktoplevel()
                tkwm.title(results, "MTurk Account Balance Check")
                r <- 1
                tkgrid(ttklabel(results, text = "     "), row=r, column=1)
                tkgrid(ttklabel(results, text = "     "), row=r, column=3)
                r <- r + 1
                tkgrid(ttklabel(results, text = paste("Previous Balance: $",round(funds$OldBalance,2), sep = "")), row=r, column=2)
                r <- r + 1
                tkgrid(ttklabel(results, text = paste("Total Cost: $",round(funds$Total,2), sep = "")), row=r, column=2)
                r <- r + 1
                tkgrid(ttklabel(results, text = paste("New Balance: $",round(funds$NewBalance,2), sep = "")), row=r, column=2)
                r <- r + 1
                tkgrid(ttklabel(results, text = "     "), row=r, column=1)
                r <- r + 1
                if(funds$SufficientFunds==TRUE)
                    sufficient <- "Yes"
                else
                    sufficient <- "No"
                tkgrid(ttklabel(results, text = paste("Sufficient Funds? ",sufficient, sep = "")), row=r, column=2)
                r <- r + 1
                tkgrid(ttklabel(results, text = "     "), row=r, column=1)
                r <- r + 1
                OKbutton <- tkbutton(results, text = "   OK   ", command = function() {tkdestroy(results); tkfocus(wizard)})
                tkgrid(OKbutton, row=r, column=2)
                tkfocus(results)
            }
            # layout
            balDialog <- tktoplevel()
            tkwm.title(balDialog, "MTurk Account Balance Check")
            entryform <- tkframe(balDialog, relief="groove", borderwidth=2)
                amt <- tclVar("0")
                assignct <- tclVar("0")
                hitct <- tclVar("1")
                bonusct <- tclVar("0")
                bonusamt <- tclVar("0")
                masters <- tclVar("0") # default to zero
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=4)
                r <- r + 1
                entry.amt <- tkentry(entryform, width = 5, textvariable=amt)
                tkgrid(tklabel(entryform, text = "Payment per assignment: $"), entry.amt, row=r, sticky="e")
                r <- r + 1
                entry.assignct <- tkentry(entryform, width = 5, textvariable=assignct)
                tkgrid(tklabel(entryform, text = "Number of assignments: "), entry.assignct, row=r, sticky="e")
                r <- r + 1
                entry.hitct <- tkentry(entryform, width = 5, textvariable=hitct)
                tkgrid(tklabel(entryform, text = "Number of HITs: "), entry.hitct, row=r, sticky="e")
                r <- r + 1
                entry.bonusct <- tkentry(entryform, width = 5, textvariable=bonusct)
                tkgrid(tklabel(entryform, text = "Number of bonuses: "), entry.bonusct, row=r, sticky="e")
                r <- r + 1
                entry.bonusamt <- tkentry(entryform, width = 5, textvariable=bonusamt)
                tkgrid(tklabel(entryform, text = "Payment per bonus: $"), entry.bonusamt, row=r, sticky="e")
                r <- r + 1
                entry.masters <- tkcheckbutton(entryform, variable=masters)
                tkgrid(tklabel(entryform,text="Use MTurk Masters Workers? "), entry.masters, row=r, sticky="e")
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
            tkgrid(entryform)
            # buttons
            buttons <- tkframe(balDialog)
                OKbutton <- tkbutton(buttons,text="   OK   ",command=function() {tkdestroy(balDialog); checksufficient()})
                Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {tkdestroy(balDialog); tkfocus(wizard)})
                r <- 1
                tkgrid(OKbutton, row = r, column = 1)
                tkgrid(Cancelbutton, row=r, column = 2)
            tkgrid(buttons)
            
            tkfocus(balDialog)
        }
        
        # load log entries
        loadlogWiz <- function(){
            # load log file
            mturkrlog <- readlogfile()
            
            # create function to display contents of an entry
            displaylogentry <- function(n){
                logentry <- tkframe(logDialog, relief="groove", borderwidth=2)
                # layout variables
                #tkwm.title(logentry, paste("Log Entry ",n,sep=""))
                tkgrid(ttklabel(logentry, text="    "),row=1, column=1, columnspan=2)
                tkgrid(ttklabel(logentry, text="Timestamp: "),row=2, column=1, sticky="e")
                tkgrid(ttklabel(logentry, text="RequestId: "),row=3, column=1, sticky="e")
                tkgrid(ttklabel(logentry, text="Operation: "),row=4, column=1, sticky="e")
                tkgrid(ttklabel(logentry, text="Sandbox? "),row=5, column=1, sticky="e")
                tkgrid(ttklabel(logentry, text="Valid? "),row=6, column=1, sticky="e")
                tkgrid(ttklabel(logentry, text="    "),row=7, column=1, columnspan=2)
                #tkgrid(ttklabel(logentry, text="For results, see R console."),row=8, column=1, columnspan=2)
                # layout log values
                tkgrid(ttklabel(logentry, text=mturkrlog$Timestamp[n]),row=2, column=2, sticky="w")
                tkgrid(ttklabel(logentry, text=mturkrlog$RequestId[n]),row=3, column=2, sticky="w")
                tkgrid(ttklabel(logentry, text=mturkrlog$Operation[n]),row=4, column=2, sticky="w")
                if(as.character(mturkrlog$Sandbox[n])=="TRUE")
                    sandboxyes <- "Yes"
                else
                    sandboxyes <- "No"
                tkgrid(ttklabel(logentry, text=sandboxyes),row=5, column=2, sticky="w")
                tkgrid(ttklabel(logentry, text=as.character(mturkrlog$Valid[n])),row=6, column=2, sticky="w")
                tkgrid(logentry, row=1, column=2)
                # write response to console
                printxml <- function(){
                    message("MTurk XML Response:")
                    print(xmlParse(mturkrlog$Response[n]))
                }
                tkgrid(tkbutton(logentry, text="Show API Response XML in Console", command=printxml),row=8, column=1, columnspan=2)
            }
            
            # layout
            # create listbox with entries
            logDialog <- tktoplevel()
            tkwm.title(logDialog, "MTurkR Log Entries")
            listframe <- tkframe(logDialog, relief="groove", borderwidth=2)
                scr <- tkscrollbar(listframe, repeatinterval=5, command=function(...) tkyview(loglist,...))
                loglist <- tklistbox(listframe, width=50, height=15, selectmode="single", yscrollcommand=function(...) tkset(scr,...), background="white")
                tkgrid(loglist,scr)
                tkgrid.configure(scr, sticky="nsw")
            tkgrid(listframe, row = 1, column = 1)
            for (i in 1:dim(mturkrlog)[1]) {
                tkinsert(loglist,"end",paste(mturkrlog$Operation[i]," (",mturkrlog$Timestamp[i],")",sep=""))
            }
            # buttons
            buttons <- tkframe(logDialog)
                r <- 1
                displaybutton <- tkbutton(buttons, text=" Display entry ", command=function() displaylogentry(n=as.numeric(tkcurselection(loglist))+1))
                cancelbutton <- tkbutton(buttons, text=" Close ", command=function() tkdestroy(logDialog) )
                tkgrid(displaybutton, row=r, column=1)
                tkgrid(cancelbutton, row=r, column=2)
            tkgrid(buttons, row = 2, column = 1)
                    
            tkfocus(logDialog)
        }
        
        # register hittype
        registerWiz <- function(){
            assign("qualreq",NULL,envir=wizardenv) # empty qualification requirements from 'wizardenv'
            # function
            gethit <- function(){
                if(tclvalue(title)==""){
                    tkmessageBox(message="Please enter a title!", type="ok")
                    tkfocus(registerDialog)
                }
                else if(tclvalue(description)==""){
                    tkmessageBox(message="Please enter a description!", type="ok")
                    tkfocus(registerDialog)
                }
                else if(tclvalue(reward)==""){
                    tkmessageBox(message="Please enter a reward amount (in US dollars)!", type="ok")
                    tkfocus(registerDialog)
                }
                else if(tclvalue(daysd)=="" && tclvalue(hoursd)=="" && tclvalue(minsd)=="" && tclvalue(secsd)==""){
                    tkmessageBox(message="Please enter a duration (that workers have to complete an assignment)!", type="ok")
                    tkfocus(registerDialog)
                }
                else if(tclvalue(keywords)==""){
                    tkmessageBox(message="Please enter some keywords!", type="ok")
                    tkfocus(registerDialog)
                }
                else if(tclvalue(daysa)=="" && tclvalue(hoursa)=="" && tclvalue(minsa)=="" && tclvalue(secsa)==""){
                    tkmessageBox(message="Please enter a delay (after which assignments are automatically approved)!", type="ok")
                    tkfocus(registerDialog)
                }
                else {
                    newhittype <- RegisterHITType(  title = tclvalue(title),
                                                    description = tclvalue(description),
                                                    reward = tclvalue(reward),
                                                    duration=seconds(as.numeric(tclvalue(daysd)),
                                                                     as.numeric(tclvalue(hoursd)),
                                                                     as.numeric(tclvalue(minsd)),
                                                                     as.numeric(tclvalue(secsd))),
                                                    keywords = tclvalue(keywords),
                                                    auto.approval.delay=seconds(as.numeric(tclvalue(daysa)),
                                                                                as.numeric(tclvalue(hoursa)),
                                                                                as.numeric(tclvalue(minsa)),
                                                                                as.numeric(tclvalue(secsa))),
                                                    qual.req = wizardenv$qualreq, # retrieve stored qualreq from wizardenv
                                                    sandbox = sandbox,
                                                    verbose = TRUE)
                    if(newhittype$Valid==TRUE){
                        assign("newHITTypeId", hittype$HITTypeId, envir=wizardenv) # write newHITTypeId to wizardenv environment
                        tkdestroy(registerDialog)
                        tkfocus(wizard)
                    }
                    else{
                        tkmessageBox(message="RegisterHITType() failed for some reason. See console.",type="ok")
                        tkfocus(registerDialog)
                    }
                }
            }
            addqualreq <- function(){
                # function
                genqual <- function() {
                    if(tclvalue(qualid)==""){
                        tkmessageBox(message="Please enter a QualificationTypeId!", type="ok")
                        tkfocus(qualreqDialog)
                    }
                    else if(is.null(tkcurselection(complist))){
                        tkmessageBox(message="Please enter a comparator!", type="ok")
                        tkfocus(qualreqDialog)
                    }
                    else {
                        pos <- as.numeric(as.character(tkcurselection(complist)))+1 # listbox index starts at 0
                        selection <- complistitems[pos]
                        if(selection=="Exists")
                            qualvalue <- NULL
                        else
                            qualvalue <- tclvalue(qualvalue)
                        if(tclvalue(required)=="1")
                            required <- TRUE
                        else
                            required <- FALSE
                        req <- GenerateQualificationRequirement(qual=tclvalue(qualid),
                                                                comparator=selection,
                                                                value=qualvalue,
                                                                preview=required,
                                                                qual.number=as.integer(tclvalue(nqualreqs))+1
                                                                )
                        tkdestroy(qualreqDialog)
                        # adjust focus
                        assign("qualreq",paste(wizardenv$qualreq,req,sep=""),envir=wizardenv) # assign req to wizardenv
                        tclvalue(nqualreqs) <<- as.integer(tclvalue(nqualreqs)) + 1 # increment 'nqualreqs'
                        tkfocus(registerDialog)
                    }
                }
                    
                # layout
                qualreqDialog <- tktoplevel()
                tkwm.title(qualreqDialog, "Generate QualificationRequirement")
                entryform <- tkframe(qualreqDialog, relief="groove", borderwidth=2)
                    qualvalue <- tclVar()
                    required <- tclVar("0")
                    r <- 1
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=8)
                    r <- r + 1
                    qualid <- tclVar()
                    qualid.entry <- tkentry(entryform, width = 50, textvariable=qualid)
                    tkgrid(tklabel(entryform, text = "QualificationTypeId: "), row=r, column=2, columnspan=4, sticky="e")
                    tkgrid(qualid.entry, row=r, column=6, columnspan=2, sticky="w")
                    r <- r + 1
                    tkgrid(tklabel(entryform,text="Qualification Required for HIT Preview: "), row=r, column=2, columnspan=4, sticky="e")
                    required.entry <- tkcheckbutton(entryform, variable=required)
                    tkgrid(required.entry, row=r, column=6, sticky="w")
                    r <- r + 2
                    tkgrid(tklabel(entryform,text="Score is: "), row=r, column=2, rowspan=2, sticky="e")
                    scr <- tkscrollbar(entryform, repeatinterval=4, command=function(...) tkyview(complist,...))
                    complist <- tklistbox(    entryform, height=3, width=7, selectmode="single",
                                            yscrollcommand=function(...) tkset(scr,...), background="white")
                    tkgrid(complist, scr, row=r, column=3, rowspan=2, sticky="e")
                    tkgrid.configure(scr, column=4, rowspan=2, sticky="w")
                    complistitems <- c("<","<=",">",">=","==","!=","Exists")
                    for (i in 1:length(complistitems)) {
                        tkinsert(complist,"end",complistitems[i])
                    }
                    tkselection.set(complist,0)
                    tkgrid(tklabel(entryform,text="value: "), row=r, column=5, rowspan=2, sticky="w")
                    qual.entry <- tkentry(entryform, width=10, textvariable=qualvalue)
                    tkgrid(qual.entry, row=r, column=6, rowspan=2, sticky="w")
                    tkgrid(tklabel(entryform,text="(Required except for 'Exists')"), row=r, column=7, rowspan=2, sticky="w")
                    r <- r + 2
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(entryform)
                # buttons
                buttons <- tkframe(qualreqDialog)
                    populate <- function(){
                        result <- searchqualsWiz()
                        tclvalue(qualid) <<- wizardenv$qualresult$QualificationTypeId # retrieve qualid from wizardenv environment
                    }
                    r <- 1
                    populatebutton <- tkbutton(buttons, text="Search for QualificationTypes", command=populate)
                    OKbutton <- tkbutton(buttons, text="   OK   ", command=genqual)
                    Cancelbutton <- tkbutton(buttons, text=" Cancel ", command=function(){tkdestroy(qualreqDialog); tkfocus(wizard)})
                    tkgrid(populatebutton, row=r, column=1)
                    tkgrid(OKbutton, row=r, column=2)
                    tkgrid(Cancelbutton, row=r, column=3)
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(buttons)
            
                tkfocus(qualreqDialog)
            }
            
            assign("qualreq",NULL,envir=wizardenv) # clear any stored value of 'qualreq' in wizardenv
            # layout
            registerDialog <- tktoplevel()
            tkwm.title(registerDialog, "Register HITType")
            title <- tclVar()
            description <- tclVar()
            reward <- tclVar()
            keywords <- tclVar()
            autoapprovaldelay <- tclVar()
            regqualreq <- NA
            entryform <- tkframe(registerDialog, relief="groove", borderwidth=2)
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=10)
                r <- r + 1
                title.entry <- tkentry(entryform, width = 50, textvariable=title)
                desc.entry <- tkentry(entryform, width = 50, textvariable=description)
                keywords.entry <- tkentry(entryform, width = 50, textvariable=keywords)
                reward.entry <- tkentry(entryform, width = 10, textvariable=reward)
                tkgrid(tklabel(entryform, text = "Title: "), row=r, column=2)
                tkgrid(title.entry, row=r, column=3, columnspan=7)
                r <- r + 1
                tkgrid(tklabel(entryform, text = "Description: "), row=r, column=2)
                tkgrid(desc.entry, row=r, column=3, columnspan=7)
                r <- r + 1
                tkgrid(tklabel(entryform, text = "Keywords: "), row=r, column=2)
                tkgrid(keywords.entry, row=r, column=3, columnspan=7)
                r <- r + 1
                tkgrid(tklabel(entryform, text = "Reward: $"), row=r, column=2)
                tkgrid(reward.entry, row=r, column=3, columnspan=7, sticky="w")
                r <- r + 1
                tkgrid(tklabel(entryform, text = "Time alloted for worker to complete an assignment: "), row=r, column=2, columnspan=6)
                r <- r + 1
                daysd <- tclVar("0")
                hoursd <- tclVar("0")
                minsd <- tclVar("0")
                secsd <- tclVar("0")
                daysd.entry <- tkentry(entryform, width = 5, textvariable=daysd)
                hoursd.entry <- tkentry(entryform, width = 5, textvariable=hoursd)
                minsd.entry <- tkentry(entryform, width = 5, textvariable=minsd)
                secsd.entry <- tkentry(entryform, width = 5, textvariable=secsd)
                tkgrid(tklabel(entryform, text = "Days: "), row=r, column=2)
                tkgrid(daysd.entry, row=r, column=3)
                tkgrid(tklabel(entryform, text = "Hours: "), row=r, column=4)
                tkgrid(hoursd.entry, row=r, column=5)
                tkgrid(tklabel(entryform, text = "Minutes: "), row=r, column=6)
                tkgrid(minsd.entry, row=r, column=7)
                tkgrid(tklabel(entryform, text = "Seconds: "), row=r, column=8)
                tkgrid(secsd.entry, row=r, column=9)
                r <- r + 1
                tkgrid(tklabel(entryform, text = "Delay before automatically approving assignments: "), row=r, column=2, columnspan=6)
                r <- r + 1
                daysa <- tclVar("30")
                hoursa <- tclVar("0")
                minsa <- tclVar("0")
                secsa <- tclVar("0")
                daysa.entry <- tkentry(entryform, width = 5, textvariable=daysa)
                hoursa.entry <- tkentry(entryform, width = 5, textvariable=hoursa)
                minsa.entry <- tkentry(entryform, width = 5, textvariable=minsa)
                secsa.entry <- tkentry(entryform, width = 5, textvariable=secsa)
                tkgrid(tklabel(entryform, text = "Days: "), row=r, column=2)
                tkgrid(daysa.entry, row=r, column=3)
                tkgrid(tklabel(entryform, text = "Hours: "), row=r, column=4)
                tkgrid(hoursa.entry, row=r, column=5)
                tkgrid(tklabel(entryform, text = "Minutes: "), row=r, column=6)
                tkgrid(minsa.entry, row=r, column=7)
                tkgrid(tklabel(entryform, text = "Seconds: "), row=r, column=8)
                tkgrid(secsa.entry, row=r, column=9)
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=2)
                r <- r + 1
                tkgrid(tklabel(entryform, text = "Qualification Requirements: "), row=r, column=2, columnspan=3)
                addbutton <- tkbutton(entryform, text=" Add ", command=addqualreq)
                viewbutton <- tkbutton(entryform, text=" View ", command=function() {
                    if(!is.null(wizardenv$qualreq)){
                        qreqsplit <- strsplit(wizardenv$qualreq,"&")[[1]]
                        for(i in 2:length(qreqsplit)){
                            message(strsplit(qreqsplit[i],"=")[[1]][1],": ",strsplit(qreqsplit[i],"=")[[1]][2])
                        }
                    }
                })
                delbutton <- tkbutton(entryform, text=" Clear ", command=function() assign("qualreq",NULL,envir=wizardenv))
                tkgrid(addbutton, row=r, column=5)
                tkgrid(viewbutton, row=r, column=6)
                tkgrid(delbutton, row=r, column=7)
                nqualreqs <- tclVar("0")
                tkgrid(tklabel(entryform, text = "Currently:"), row=r, column=8)
                n <- tklabel(entryform, text=tclvalue(nqualreqs))
                tkconfigure(n,textvariable=nqualreqs)
                tkgrid(n, row=r, column=9)
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=2)
            tkgrid(entryform)
            # buttons
            buttons <- tkframe(registerDialog)
                r <- 1
                OKbutton <- tkbutton(buttons, text="   OK   ", command=gethit)
                Cancelbutton <- tkbutton(buttons, text=" Cancel ", command=function(){tkdestroy(registerDialog); tkfocus(wizard)})
                tkgrid(OKbutton, row=r, column=2)
                tkgrid(Cancelbutton, row=r, column=3)
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
            tkgrid(buttons)
            
            tkfocus(registerDialog)
        }
        
        
        # createhit
        createWiz <- function(){
            # function to add ExternalQuestion structure
            addexquestion <- function(){
                # function
                store <- function(){
                    if(tclvalue(question)==""){
                        tkmessageBox(message="Please enter an ExternalQuestion URL!", type="ok")
                        tkfocus(addqDialog)
                    }
                    else if(tclvalue(height)==""){
                        tkmessageBox(message="Please enter a frame height for the HIT!", type="ok")
                        tkfocus(addqDialog)
                    }
                    else {
                        exquestion <- GenerateExternalQuestion(url=question, frame.height=tclvalue(height))
                        assign("question",exquestion,envir=wizardenv) # assign 'question' to wizardenv
                    }
                }
                # layout
                addqDialog <- tktoplevel()
                tkwm.title(addqDialog, "Add External Question to HIT")
                entryform <- tkframe(addqDialog, relief="groove", borderwidth=2)
                    question <- tclVar()
                    height <- tclVar("450")
                    r <- 1
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=4)
                    r <- r + 1
                    question.entry <- tkentry(entryform, width = 50, textvariable=question)
                    tkgrid(tklabel(entryform, text = "Question URL: "), row=r, column=2, sticky="e")
                    tkgrid(question.entry, row=r, column=3, sticky="w")
                    r <- r + 1
                    height.entry <- tkentry(entryform, width = 10, textvariable=height)
                    tkgrid(tklabel(entryform, text = "Frame Height (pixels): "), row=r, column=2, sticky="e")
                    tkgrid(height.entry, row=r, column=3, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = "     "), row=r)
                tkgrid(entryform)
                # buttons
                buttons <- tkframe(addqDialog)
                    OKbutton <- tkbutton(buttons,text="   OK   ",command=store)
                    Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {tkdestroy(addqDialog); tkfocus(wizard)})
                    r <- 1
                    tkgrid(OKbutton, row = r, column = 1)
                    tkgrid(Cancelbutton, row=r, column = 2)
                tkgrid(buttons)
                
                tkfocus(addqDialog)
            }
            # function to add QuestionForm structure
            addinquestion <- function(){
                # function
                store <- function(){
                    question <- tclvalue(tkget(question.entry,"0.0","end"))
                    if(question==""){
                        tkmessageBox(message="Please enter a question data structure!", type="ok")
                        tkfocus(addqDialog)
                    }
                    else {
                        assign("question",question,envir=wizardenv) # assign 'question' to wizardenv
                    }
                }
                # layout
                addqDialog <- tktoplevel()
                tkwm.title(addqDialog, "Add QuestionForm to HIT")
                entryform <- tkframe(addqDialog, relief="groove", borderwidth=2)
                    question <- tclVar()
                    r <- 1
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=5)
                    r <- r + 1
                    question.entry <- tktext(entryform, height = 10, width = 75)
                    tkmark.set(question.entry,"insert","0.0")
                    tkgrid(tklabel(entryform, text = "QuestionForm data structure: "), row=r, column=2, columnspan=3, sticky="w")
                    r <- r + 1
                    tkgrid(question.entry, row=r, column=2, columnspan=3)
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = "     "), row=r)
                tkgrid(entryform)
                # buttons
                buttons <- tkframe(addqDialog)
                    OKbutton <- tkbutton(buttons,text="   OK   ",command=store)
                    Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {tkdestroy(addqDialog); tkfocus(wizard)})
                    r <- 1
                    tkgrid(OKbutton, row = r, column = 1)
                    tkgrid(Cancelbutton, row=r, column = 2)
                tkgrid(buttons)
                
                tkfocus(addqDialog)
            }
            
            # function to add HTMLQuestion structure
            addhtmlquestion <- function(){
                # function
                store <- function(){
                    question <- tclvalue(tkget(question.entry,"0.0","end"))
                    if(question==""){
                        tkmessageBox(message="Please enter a question data structure!", type="ok")
                        tkfocus(addqDialog)
                    }
                    else {
                        assign("question",question,envir=wizardenv) # assign 'question' to wizardenv
                    }
                }
                # layout
                addqDialog <- tktoplevel()
                tkwm.title(addqDialog, "Add HTMLQuestion to HIT")
                entryform <- tkframe(addqDialog, relief="groove", borderwidth=2)
                    question <- tclVar()
                    r <- 1
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=5)
                    r <- r + 1
                    question.entry <- tktext(entryform, height = 10, width = 75)
                    tkmark.set(question.entry,"insert","0.0")
                    tkgrid(tklabel(entryform, text = "HTMLQuestion data structure: "), row=r, column=2, columnspan=3, sticky="w")
                    r <- r + 1
                    tkgrid(question.entry, row=r, column=2, columnspan=3)
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = "     "), row=r)
                tkgrid(entryform)
                # buttons
                buttons <- tkframe(addqDialog)
                    OKbutton <- tkbutton(buttons,text="   OK   ",command=store)
                    Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {tkdestroy(addqDialog); tkfocus(wizard)})
                    r <- 1
                    tkgrid(OKbutton, row = r, column = 1)
                    tkgrid(Cancelbutton, row=r, column = 2)
                tkgrid(buttons)
                
                tkfocus(addqDialog)
            }
            
            # function to add HITLayout structure
            addlayout <- function(){
                # function
                store <- function(){
                    # convert 'layoutnames' and 'layoutvalues' to R character vectors
                    lnames <- strsplit(gsub(" ","",tclvalue(layoutnames)),",")[[1]]                    
                    lvalues <- strsplit(tclvalue(layoutvalues),",")[[1]]
                    # check entries
                    if(tclvalue(layoutid)==""){
                        tkmessageBox(message="Please enter a HITLayoutId!", type="ok")
                        tkfocus(addqDialog)
                    }
                    else if(!length(lnames)==length(lvalues)){
                        tkmessageBox(message="Layout Names and Layout Values must have same length!", type="ok")
                        tkfocus(addqDialog)
                    }
                    else {
                        assign("layout",tclvalue(layoutid),envir=wizardenv) # assign 'layout' to wizardenv
                        if(!lnames==""){
                            layoutparameters <- GenerateHITLayoutParameter(lnames,lvalues)
                            assign("layoutparameters",layoutparameters,envir=wizardenv)
                        }
                        tkdestroy(addqDialog)
                    }
                }
                # layout
                addqDialog <- tktoplevel()
                tkwm.title(addqDialog, "Add HIT Layout Parameters")
                entryform <- tkframe(addqDialog, relief="groove", borderwidth=2)
                    r <- 1
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=4)
                    r <- r + 1
                    website <- ttklabel(entryform, text = "Retrieve HITLayoutId and Layout Parameters from MTurk Requester Site", foreground="blue")
                    tkgrid(website, row=r, column=2, columnspan=2)
                    tkbind(website, "<ButtonPress>", function() browseURL("https://requester.mturk.com/create/projects"))
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text= "     "), row=r)
                    r <- r + 1
                    layoutid <- tclVar()
                    id.entry <- tkentry(entryform, width = 50, textvariable=layoutid)
                    tkgrid(tklabel(entryform, text = "HITLayoutId: "), row=r, column=2, sticky="e")
                    tkgrid(id.entry, row=r, column=3, sticky="w")
                    r <- r + 1
                    layoutnames <- tclVar()
                    names.entry <- tkentry(entryform, width = 50, textvariable=layoutnames)
                    tkgrid(tklabel(entryform, text = "Layout Parameter Names (comma-separated): "), row=r, column=2, sticky="e")
                    tkgrid(names.entry, row=r, column=3, sticky="w")
                    r <- r + 1
                    layoutvalues <- tclVar()
                    values.entry <- tkentry(entryform, width = 50, textvariable=layoutvalues)
                    tkgrid(tklabel(entryform, text = "Layout Parameter Values (comma-separated): "), row=r, column=2, sticky="e")
                    tkgrid(values.entry, row=r, column=3, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = "     "), row=r)
                tkgrid(entryform)
                # buttons
                buttons <- tkframe(addqDialog)
                    OKbutton <- tkbutton(buttons,text="   OK   ",command=store)
                    Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {tkdestroy(addqDialog); tkfocus(wizard)})
                    r <- 1
                    tkgrid(OKbutton, row = r, column = 1)
                    tkgrid(Cancelbutton, row=r, column = 2)
                tkgrid(buttons)
                
                tkfocus(addqDialog)
            }
            
            # function to add HIT and/or Assignment Review Policies
            addreviewpolicy <- function(){
                storepolicy <- function()    {
                    hittowrite <- tclvalue(tkget(hit.entry,"0.0","end"))
                    assigntowrite <- tclvalue(tkget(assign.entry,"0.0","end"))
                    if(hittowrite=="")
                        assign("hitreviewpolicy", hittowrite, envir=wizardenv) # assign 'reviewpolicy' to wizardenv
                    if(assigntowrite=="")
                        assign("assignreviewpolicy", assigntowrite, envir=wizardenv) # assign 'reviewpolicy' to wizardenv
                    tkdestroy(reviewpolicyDialog)
                }
                # layout
                reviewpolicyDialog <- tktoplevel()
                tkwm.title(reviewpolicyDialog, "Add HIT/Assignment Review Policy")
                entryform <- tkframe(reviewpolicyDialog, relief="groove", borderwidth=2)
                    r <- 1
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=4)
                    r <- r + 1
                    hit.entry <- tktext(entryform, height = 6, width = 75)
                    tkmark.set(hit.entry,"insert","0.0")
                    tkgrid(tklabel(entryform, text = "HIT Review Policy (optional): "), row=r, column=2, columnspan=2, sticky="w")
                    r <- r + 1
                    tkgrid(hit.entry, row=r, column=2, columnspan=2)
                    r <- r + 1
                    assign.entry <- tktext(entryform, height = 6, width = 75)
                    tkmark.set(assign.entry,"insert","0.0")
                    tkgrid(tklabel(entryform, text = "Assignment Review Policy (optional): "), row=r, column=2, columnspan=2, sticky="w")
                    r <- r + 1
                    tkgrid(assign.entry, row=r, column=2, columnspan=2)
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = "     "), row=r)
                tkgrid(entryform)
                # buttons
                buttons <- tkframe(reviewpolicyDialog)
                    OKbutton <- tkbutton(buttons,text="   OK   ",command=storepolicy)
                    Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {tkdestroy(reviewpolicyDialog); tkfocus(wizard)})
                    r <- 1
                    tkgrid(OKbutton, row = r, column = 2)
                    tkgrid(Cancelbutton, row=r, column = 3)
                tkgrid(buttons)
                
                tkfocus(reviewpolicyDialog)
            }
            
            # function to create HIT
            create <- function(){
                if(tclvalue(hittype)==""){
                    tkmessageBox(message="Please enter a HITTypeId!", type="ok")
                    tkfocus(createDialog)
                }
                else if(tclvalue(annotate)==""){
                    tkmessageBox(message="Please enter a name (private) for the HIT!", type="ok")
                    tkfocus(createDialog)
                }
                else if(tclvalue(assigns)==""){
                    tkmessageBox(message="Please enter the number of assignments for the HIT!", type="ok")
                    tkfocus(createDialog)
                }
                else if(tclvalue(days)=="" && tclvalue(hours)=="" && tclvalue(mins)=="" && tclvalue(secs)==""){
                    tkmessageBox(message="Please enter the amount of time the HIT should be available!", type="ok")
                    tkfocus(createDialog)
                }
                else if(is.null(wizardenv$layoutid) && is.null(wizardenv$question)){
                    tkmessageBox(message="Specify Question or HITLayout Parameters!", type="ok")
                    tkfocus(createDialog)
                }
                else {
                    if(is.null(wizardenv$hitreviewpolicy))
                        hitpolicy <- NULL
                    else
                        hitpolicy <- wizardenv$hitreviewpolicy
                    if(is.null(wizardenv$assignreviewpolicy))
                        assignpolicy <- NULL
                    else
                        assignpolicy <- wizardenv$assignreviewpolicy
                    newhit <- CreateHIT(    hit.type=tclvalue(hittype),
                                            question=wizardenv$question,
                                            expiration=seconds(as.numeric(tclvalue(days)),
                                                               as.numeric(tclvalue(hours)),
                                                               as.numeric(tclvalue(mins)),
                                                               as.numeric(tclvalue(secs))),
                                            assignments=tclvalue(assigns),
                                            annotation=tclvalue(annotate),
                                            assignment.review.policy=assignpolicy,
                                            hit.review.policy=hitpolicy,
                                            hitlayoutid=wizardenv$layoutid,
                                            hitlayoutparameters=wizardenv$layoutparameters,
                                            verbose=FALSE,
                                            sandbox=sandbox
                                        )
                    if(newhit$Valid==TRUE){
                        assign("newHITId",newhit$HITId,envir=wizardenv) # assign newHITId to wizardenv
                        tkdestroy(createDialog)
                        tkfocus(wizard)
                    }
                    else{
                        tkmessageBox(message="CreateHIT() failed for some reason. See console.",type="ok")
                        tkfocus(createDialog)
                    }
                }
            }
            
            assign("question",NULL,envir=wizardenv) # make value of 'question' NULL, unless changed in dialog
            assign("layoutid",NULL,envir=wizardenv) # make value of 'layoutid' NULL, unless changed in dialog
            assign("layoutparameters",NULL,envir=wizardenv) # make value of 'layoutparameters' NULL, unless changed in dialog
            assign("reviewpolicy",NULL,envir=wizardenv) # make value of 'reviewpolicy' NULL, unless changed in dialog
            # dialog
            createDialog <- tktoplevel()
            tkwm.title(createDialog, "Create HIT")
            entryform <- tkframe(createDialog, relief="groove", borderwidth=2)
                # hitid
                hittypeid <- tclVar()
                annotate <- tclVar()
                assigns <- tclVar()
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=11)
                r <- r + 1
                hittype.entry <- tkentry(entryform, width = 50, textvariable=hittypeid)
                tkgrid(tklabel(entryform, text = "Enter HITTypeId: "), row=r, column=2)
                tkgrid(hittype.entry, row=r, column=3, columnspan=8, sticky="w")
                r <- r + 1
                annotate.entry <- tkentry(entryform, width = 50, textvariable=annotate)
                tkgrid(tklabel(entryform, text = "Enter name for this HIT (visible only to you): "), row=r, column=2)
                tkgrid(annotate.entry, row=r, column=3, columnspan=8, sticky="w")
                r <- r + 1
                assigns.entry <- tkentry(entryform, width = 10, textvariable=assigns)
                tkgrid(tklabel(entryform, text = "How many assignments should be available? "), row=r, column=2)
                tkgrid(assigns.entry, row=r, column=3, columnspan=2, sticky="w")
                r <- r + 1
                tkgrid(tklabel(entryform, text = "How long should HIT remain available?"), row=r, column=2)
                days <- tclVar("0")
                hours <- tclVar("0")
                mins <- tclVar("0")
                secs <- tclVar("0")
                days.entry <- tkentry(entryform, width = 5, textvariable=days)
                hours.entry <- tkentry(entryform, width = 5, textvariable=hours)
                mins.entry <- tkentry(entryform, width = 5, textvariable=mins)
                secs.entry <- tkentry(entryform, width = 5, textvariable=secs)
                tkgrid(tklabel(entryform, text = "Days: "), row=r, column=3)
                tkgrid(days.entry, row=r, column=4)
                tkgrid(tklabel(entryform, text = "Hours: "), row=r, column=5)
                tkgrid(hours.entry, row=r, column=6)
                tkgrid(tklabel(entryform, text = "Minutes: "), row=r, column=7)
                tkgrid(mins.entry, row=r, column=8)
                tkgrid(tklabel(entryform, text = "Seconds: "), row=r, column=9)
                tkgrid(secs.entry, row=r, column=10)
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
                r <- r + 1
                tkgrid(tklabel(entryform, text = "Specify an ExternalQuestion URL, QuestionForm, or Layout Parameters:"), row=r, column=2, columnspan=9)
                r <- r + 1
                questionframe <- tkframe(entryform)
                    s <- 1
                    externalbutton <- tkbutton(questionframe, text=" Add ExternalQuestion ", command=addexquestion)
                    internalbutton <- tkbutton(questionframe, text=" Add QuestionForm ", command=addinquestion)
                    htmlbutton <- tkbutton(questionframe, text=" Add HTMLQuestion ", command=addhtmlquestion)
                    layoutbutton <- tkbutton(questionframe, text=" Add HITLayout data ", command=addlayout)
                    tkgrid(externalbutton, row=s, column=1)
                    tkgrid(internalbutton, row=s, column=2)
                    tkgrid(htmlbutton, row=s, column=3)
                    tkgrid(layoutbutton, row=s, column=4)
                    s <- s + 1
                    tkgrid(ttklabel(questionframe, text = "     "), row=s)
                tkgrid(questionframe, column=2, row=r, columnspan=9)
                r <- r + 1
                reviewbutton <- tkbutton(entryform, text=" Add ReviewPolicy (optional) ", command=addreviewpolicy)
                tkgrid(reviewbutton, row=r, column=2, columnspan=9)
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
            tkgrid(entryform)
            # buttons
            buttons <- tkframe(createDialog)
                populate <- function(){
                    registerWiz()
                    tclvalue(hittypeid) <<- wizardenv$newHITTypeId # retrieve hitid from wizardenv environment
                }
                populatebutton <- tkbutton(buttons, text="Register New HITType", command=populate)
                OKbutton <- tkbutton(buttons, text="   OK   ", command=create)
                Cancelbutton <- tkbutton(buttons, text=" Cancel ", command=function() {tkdestroy(createDialog); tkfocus(wizard)})
                r <- 1
                tkgrid(populatebutton, row=r, column=1)
                tkgrid(OKbutton, row=r, column=2)
                tkgrid(Cancelbutton, row=r, column=3)
            tkgrid(buttons)
            
            tkfocus(createDialog)
        }
        
        # changehittypeofhit
        changetypeWiz <- function() {
            # function to change HITType
            change <- function(){
                if(tclvalue(hittype)==""){
                    tkmessageBox(message="Please enter a new HITTypeId!", type="ok")
                    tkfocus(changeDialog)
                }
                if(tclvalue(oldhittype)=="" && tclvalue(hitid)==""){
                    tkmessageBox(message="Please enter either a HITType or HITId whose type should be changed!", type="ok")
                    tkfocus(changeDialog)
                } else if(tclvalue(oldhittype)=="" && tclvalue(hitid)==""){
                    tkmessageBox(message="Only an old HITType or old HITId can be specified. Not both!", type="ok")
                    tkfocus(changeDialog)
                } else if(!tclvalue(oldhittype)==""){
                    changed <- ChangeHITType(old.hit.type = tclvalue(oldhittype),
                                             new.hit.type = tclvalue(hittypeid),
                                             verbose = FALSE,
                                             sandbox = sandbox)
                    tkdestroy(changeDialog)
                    tkfocus(wizard)
                } else if(!tclvalue(hitid)==""){
                    h <- strsplit(tclvalue(hitid), ",")[[1]]
                    changed <- ChangeHITType(hit = h,
                                             new.hit.type = tclvalue(hittypeid),
                                             verbose = FALSE,
                                             sandbox = sandbox)
                    tkdestroy(changeDialog)
                    tkfocus(wizard)
                }
            }
            
            # dialog
            changeDialog <- tktoplevel()
            tkwm.title(changeDialog, "Change HITType of HIT(s)")
            entryform <- tkframe(changeDialog, relief="groove", borderwidth=2)
                # hitid
                hittypeid <- tclVar()
                oldhittype <- tclVar()
                hitid <- tclVar()
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=11)
                r <- r + 1
                hittype.entry <- tkentry(entryform, width = 50, textvariable=hittypeid)
                tkgrid(tklabel(entryform, text = "Enter New HITTypeId (or Register a HITType, below): "), row=r, column=2)
                tkgrid(hittype.entry, row=r, column=3, columnspan=8, sticky="w")
                r <- r + 1
                tkgrid(tklabel(entryform, text = "Enter old HITTypeId OR old HITId(s) you want to Change"), row=r, column=2)
                r <- r + 1
                oldhittype.entry <- tkentry(entryform, width = 50, textvariable=oldhittype)
                tkgrid(tklabel(entryform, text = "Old HITTypeId to Change: "), row=r, column=2)
                tkgrid(oldhittype.entry, row=r, column=3, columnspan=8, sticky="w")
                r <- r + 1
                hit.entry <- tkentry(entryform, width = 10, textvariable=hitid)
                tkgrid(tklabel(entryform, text = "Old HITId(s) to Change: "), row=r, column=2)
                tkgrid(hit.entry, row=r, column=3, columnspan=2, sticky="w")
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
            tkgrid(entryform)
            # buttons
            buttons <- tkframe(changeDialog)
                populate <- function(){
                    registerWiz()
                    tclvalue(hittypeid) <<- wizardenv$newHITTypeId # retrieve hittypeid from wizardenv environment
                }
                populatebutton <- tkbutton(buttons, text="Register New HITType", command=populate)
                OKbutton <- tkbutton(buttons, text="   OK   ", command=change)
                Cancelbutton <- tkbutton(buttons, text=" Cancel ", command=function() {tkdestroy(changeDialog); tkfocus(wizard)})
                r <- 1
                tkgrid(populatebutton, row=r, column=1)
                tkgrid(OKbutton, row=r, column=2)
                tkgrid(Cancelbutton, row=r, column=3)
            tkgrid(buttons)
            
            tkfocus(changeDialog)
        }
        
        # gethit
        gethitWiz <- function(){
            # function
            gethit <- function(){
                if(tclvalue(hitid)==""){
                    tkmessageBox(message="Please enter a HITId!", type="ok")
                    tkfocus(gethitDialog)
                }
                else {
                    tkdestroy(gethitDialog)
                    hit <- GetHIT(hit=tclvalue(hitid),verbose=FALSE, sandbox=sandbox)
                    #print(t(hit$HITs)[1:18,1])
                    
                    viewhitDialog <- tktoplevel()
                    tkwm.title(viewhitDialog, "HIT Details")
                    entryform <- tkframe(viewhitDialog, relief="groove", borderwidth=2)
                        r <- 1
                        tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                        tkgrid(ttklabel(entryform, text = "     "), row=r, column=3)
                        r <- r + 1
                        tkgrid(tklabel(entryform, text = paste("HITId:",hit$HITs$HITId)), row=r, column=2, sticky="w")
                        r <- r + 1
                        tkgrid(tklabel(entryform, text = paste("HITTypeId:",hit$HITs$HITTypeId)), row=r, column=2, sticky="w")
                        r <- r + 1
                        tkgrid(tklabel(entryform, text = paste("Title:",hit$HITs$Title)), row=r, column=2, sticky="w")
                        r <- r + 1
                        tkgrid(tklabel(entryform, text = paste("Description:",hit$HITs$Description)), row=r, column=2, sticky="w")
                        r <- r + 1
                        tkgrid(tklabel(entryform, text = paste("Annotation:",hit$HITs$RequesterAnnotation)), row=r, column=2, sticky="w")
                        r <- r + 1
                        tkgrid(tklabel(entryform, text = paste("NumberOfSimilarHITs:",hit$HITs$NumberOfSimilarHITs)), row=r, column=2, sticky="w")
                        r <- r + 1
                        tkgrid(tklabel(entryform, text = paste("CreationTime:",hit$HITs$CreationTime)), row=r, column=2, sticky="w")
                        r <- r + 1
                        tkgrid(tklabel(entryform, text = paste("HITStatus:",hit$HITs$HITStatus)), row=r, column=2, sticky="w")
                        r <- r + 1
                        tkgrid(tklabel(entryform, text = paste("HITReviewStatus:",hit$HITs$HITReviewStatus)), row=r, column=2, sticky="w")
                        r <- r + 1
                        tkgrid(tklabel(entryform, text = paste("MaxAssignments:",hit$HITs$MaxAssignments)), row=r, column=2, sticky="w")
                        r <- r + 1
                        tkgrid(tklabel(entryform, text = paste("Amount:",hit$HITs$Amount)), row=r, column=2, sticky="w")
                        r <- r + 1
                        tkgrid(tklabel(entryform, text = paste("AutoApprovalDelayInSeconds:",hit$HITs$AutoApprovalDelayInSeconds)), row=r, column=2, sticky="w")
                        r <- r + 1
                        tkgrid(tklabel(entryform, text = paste("AssignmentDurationInSeconds:",hit$HITs$AssignmentDurationInSeconds)), row=r, column=2, sticky="w")
                        r <- r + 1
                        tkgrid(tklabel(entryform, text = paste("Expiration:",hit$HITs$Expiration)), row=r, column=2, sticky="w")
                        r <- r + 1
                        tkgrid(tklabel(entryform, text = "     "), row=r, column=1)
                    tkgrid(entryform)
                    buttons <- tkframe(viewhitDialog)
                        OKbutton <- tkbutton(buttons, text="   OK   ", command=function() {tkdestroy(viewhitDialog); tkfocus(wizard)})
                        r <- 1
                        tkgrid(OKbutton, row=r, column=2)
                    tkgrid(buttons)
                    
                    tkfocus(viewhitDialog)
                }
            }
            # layout
            gethitDialog <- tktoplevel()
            tkwm.title(gethitDialog, "View HIT Details")
            entryform <- tkframe(gethitDialog, relief="groove", borderwidth=2)
                # hitid
                hitid <- tclVar()
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
                hit.entry <- tkentry(entryform, width = 50, textvariable=hitid)
                r <- r + 1
                tkgrid(tklabel(entryform, text = "Enter HITId: "), row=r, column=1)
                tkgrid(hit.entry, row=r, column=2, columnspan=4)
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
            tkgrid(entryform)
            # buttons
            buttons <- tkframe(gethitDialog)
                populate <- function(){
                    searchWiz()
                    tclvalue(hitid) <<- wizardenv$searchresult$HITId # retrieve hitid from wizardenv environment
                }
                populatebutton <- tkbutton(buttons, text="Search for HITs", command=populate)
                OKbutton <- tkbutton(buttons, text="   OK   ", command=gethit)
                Cancelbutton <- tkbutton(buttons, text=" Cancel ", command=function() {tkdestroy(gethitDialog); tkfocus(wizard)})
                r <- 1
                tkgrid(populatebutton, row=r, column=1)
                tkgrid(OKbutton, row=r, column=2)
                tkgrid(Cancelbutton, row=r, column=3)
            tkgrid(buttons)
            
            tkfocus(gethitDialog)
        }
        
        # check hit status
        statusWiz <- function(){
            # function
            status <- function(){
                if(tclvalue(hitid)==""){
                    tkmessageBox(message="Please enter a HITId!", type="ok")
                    tkfocus(statusDialog)
                }
                else {
                    tkdestroy(statusDialog)
                    status <- HITStatus(hit=tclvalue(hitid), verbose=FALSE, sandbox=sandbox)
                    
                    viewhitDialog <- tktoplevel()
                    tkwm.title(viewhitDialog, "HIT Status")
                    entryform <- tkframe(viewhitDialog, relief="groove", borderwidth=2)
                        r <- 1
                        tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                        tkgrid(ttklabel(entryform, text = "     "), row=r, column=3)
                        r <- r + 1
                        tkgrid(tklabel(entryform, text = paste("HITId:",status$HITId)), row=r, column=2, sticky="w")
                        r <- r + 1
                        tkgrid(tklabel(entryform, text = paste("Annotation:",status$RequesterAnnotation)), row=r, column=2, sticky="w")
                        r <- r + 1
                        tkgrid(tklabel(entryform, text = paste("HITStatus:",status$HITStatus)), row=r, column=2, sticky="w")
                        r <- r + 1
                        tkgrid(tklabel(entryform, text = paste("HITReviewStatus:",status$HITReviewStatus)), row=r, column=2, sticky="w")
                        r <- r + 1
                        tkgrid(tklabel(entryform, text = paste("NumberofAssignmentsPending:",status$NumberofAssignmentsPending)), row=r, column=2, sticky="w")
                        r <- r + 1
                        tkgrid(tklabel(entryform, text = paste("NumberofAssignmentsAvailable:",status$NumberofAssignmentsAvailable)), row=r, column=2, sticky="w")
                        r <- r + 1
                        tkgrid(tklabel(entryform, text = paste("NumberofAssignmentsCompleted:",status$NumberofAssignmentsCompleted)), row=r, column=2, sticky="w")
                        r <- r + 1
                        tkgrid(tklabel(entryform, text = paste("Expiration:",status$Expiration)), row=r, column=2, sticky="w")
                        r <- r + 1
                        tkgrid(tklabel(entryform, text = "     "), row=r, column=1)
                    tkgrid(entryform)
                    buttons <- tkframe(viewhitDialog)
                        OKbutton <- tkbutton(buttons, text="   OK   ", command=function() {tkdestroy(viewhitDialog); tkfocus(wizard)})
                        r <- 1
                        tkgrid(OKbutton, row=r, column=2)
                    tkgrid(buttons)
                    
                    tkfocus(viewhitDialog)
                }
            }
            # layout
            statusDialog <- tktoplevel()
            tkwm.title(statusDialog, "Get Status of HIT")
            entryform <- tkframe(statusDialog, relief="groove", borderwidth=2)
                hitid <- tclVar()
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=5)
                r <- r + 1
                hit.entry <- tkentry(entryform, width = 50, textvariable=hitid)
                tkgrid(tklabel(entryform, text = "HITId: "), row=r, column=1)
                tkgrid(hit.entry, row=r, column=2, columnspan=3)
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
            tkgrid(entryform)
            # buttons
            buttons <- tkframe(statusDialog)
                populate <- function(){
                    searchWiz()
                    tclvalue(hitid) <<- wizardenv$searchresult$HITId # retrieve hitid from wizardenv environment
                }
                populatebutton <- tkbutton(buttons, text="Search for HITs", command=populate)
                OKbutton <- tkbutton(buttons,text="   OK   ",command=status)
                Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {tkdestroy(statusDialog); tkfocus(wizard)})
                r <- 1
                tkgrid(populatebutton, row=r, column=1)
                tkgrid(OKbutton, row=r, column=2)
                tkgrid(Cancelbutton, row=r, column=3)
            tkgrid(buttons)
            
            tkfocus(statusDialog)
        }
        
        # retrieve HIT and/or Assignment ReviewPolicies
        reviewresultsWiz <- function(){
            # function
            getreview <- function(){
                # policylevel selections
                if(hitlevel=="1")
                    selections1 <- "HIT"
                else
                    selections1 <- NULL
                if(assignlevel=="1")
                    selections2 <- "Assignments"
                else
                    selections2 <- NULL
                selections <- c(selections1, selections2)
                # other variables
                if(tclvalue(assignmentid)=="")
                    assignmentid <- NULL
                else
                    assignmentid <- tclvalue(assignmentid)
                if(tclvalue(hitid)==""){
                    tkmessageBox(message="Please enter a HITId!", type="ok")
                    tkfocus(resultsDialog)
                }
                else {
                    tkdestroy(resultsDialog)
                    GetReviewResultsForHIT( hit=tclvalue(hitid),
                                            assignment=assignmentid,
                                            policy.level=selections,
                                            verbose=TRUE, sandbox=sandbox)
                    tkfocus(wizard)
                }
            }
            # layout
            resultsDialog <- tktoplevel()
            tkwm.title(resultsDialog, "Get ReviewResults for HIT")
            entryform <- tkframe(resultsDialog, relief="groove", borderwidth=2)
                hitlevel <- tclVar("1")
                assignlevel <- tclVar("1")
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=6)
                r <- r + 1
                hitid <- tclVar()
                hit.entry <- tkentry(entryform, width = 50, textvariable=hitid)
                tkgrid(tklabel(entryform, text = "HITId: "), row=r, column=2, sticky="e")
                tkgrid(hit.entry, row=r, column=3, columnspan=2, sticky="w")
                r <- r + 1
                assignmentid <- tclVar()
                assign.entry <- tkentry(entryform, width = 50, textvariable=assignmentid)
                tkgrid(tklabel(entryform, text = "AssignmentId: "), row=r, column=2, sticky="e")
                tkgrid(assign.entry, row=r, column=3, columnspan=2, sticky="w")
                r <- r + 1
                tkgrid(tklabel(entryform, text = "Policy Level(s): "), row=r, column=2, sticky="e")
                tkgrid(tklabel(entryform, text = "HIT: "), row=r, column=3, sticky="w")
                hitlevel.entry <- tkcheckbutton(entryform, variable=hitlevel)
                tkgrid(hitlevel.entry, row=r, column=4, sticky="w")
                r <- r + 1
                tkgrid(tklabel(entryform, text = "Assignment: "), row=r, column=3, sticky="w")
                assignlevel.entry <- tkcheckbutton(entryform, variable=assignlevel)
                tkgrid(assignlevel.entry, row=r, column=4, sticky="w")
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
            tkgrid(entryform)
            # button
            buttons <- tkframe(resultsDialog)
                populate <- function(){
                    searchWiz()
                    tclvalue(hitid) <<- wizardenv$searchresult$HITId # retrieve hitid from wizardenv environment
                }
                populatebutton <- tkbutton(buttons, text="Search for HITs", command=populate)
                OKbutton <- tkbutton(buttons,text="   OK   ",command=getreview)
                Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {tkdestroy(resultsDialog); tkfocus(wizard)})
                r <- 1
                tkgrid(populatebutton, row=r, column=1)
                tkgrid(OKbutton, row=r, column=2)
                tkgrid(Cancelbutton, row=r, column=3)
            tkgrid(buttons)
            
            tkfocus(resultsDialog)
        }
        
        # set HIT as 'Reviewing', or 'Reviewable'
        reviewingWiz <- function(){
            # function
            chgstatus <- function() {
                # ADD FUNCTIONALITY FOR hit | hit.type
                # revert
                if(tclvalue(revert)=="1")
                    revert <- TRUE
                else
                    revert <- FALSE
                if(tclvalue(hitid)==""){
                    tkmessageBox(message="Please enter a HITId!", type="ok")
                    tkfocus(reviewingDialog)
                }
                else {
                    tkdestroy(reviewingDialog)
                    SetHITAsReviewing(hit = tclvalue(hitid), revert=revert, verbose=TRUE, sandbox=sandbox)
                    tkfocus(wizard)
                }
            }
            # layout
            reviewingDialog <- tktoplevel()
            tkwm.title(reviewingDialog, "Set Status of HIT as Reviewing")
            entryform <- tkframe(reviewingDialog, relief="groove", borderwidth=2)
                hitid <- tclVar()
                revert <- tclVar("0")
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=5)
                r <- r + 1
                hit.entry <- tkentry(entryform, width = 50, textvariable=hitid)
                tkgrid(tklabel(entryform, text = "HITId: "), row=r, column=2, sticky="e")
                tkgrid(hit.entry, row=r, column=3, columnspan=2, sticky="w")
                r <- r + 1
                revert.entry <- tkcheckbutton(entryform, variable=revert)
                tkgrid(tklabel(entryform, text = "Revert to Reviewable? "), row=r, column=3, sticky="e")
                tkgrid(revert.entry, row=r, column=4, sticky="w")
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
            tkgrid(entryform)
            # buttons
            buttons <- tkframe(reviewingDialog)
                populate <- function(){
                    searchWiz()
                    tclvalue(hitid) <<- wizardenv$searchresult$HITId # retrieve hitid from wizardenv environment
                }
                populatebutton <- tkbutton(buttons, text="Search for HITs", command=populate)
                OKbutton <- tkbutton(buttons,text="   OK   ",command=chgstatus)
                Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {tkdestroy(reviewingDialog); tkfocus(wizard)})
                r <- 1
                tkgrid(populatebutton, row=r, column=1)
                tkgrid(OKbutton, row=r, column=2)
                tkgrid(Cancelbutton, row=r, column=3)
            tkgrid(buttons)
            
            tkfocus(reviewingDialog)
        }
        
        # add assignments to hit
        addassignWiz <- function(){
            # function
            addassign <- function(){
                if(tclvalue(hitid)==""){
                    tkmessageBox(message="Please enter a HITId!", type="ok")
                    tkfocus(addassignDialog)
                }
                else if(tclvalue(assignments)==""){
                    tkmessageBox(message="Please enter a number of additional assignments!", type="ok")
                    tkfocus(addassignDialog)
                }
                else {
                    tkdestroy(addassignDialog)
                    ExtendHIT(hit=tclvalue(hitid),add.assignments=tclvalue(assignments), sandbox=sandbox)
                    tkfocus(wizard)
                }
            }
            # layout
            addassignDialog <- tktoplevel()
            tkwm.title(addassignDialog, "Add Assignments to HIT")
            entryform <- tkframe(addassignDialog, relief="groove", borderwidth=2)
                hitid <- tclVar()
                assignments <- tclVar()
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=4)
                r <- r + 1
                hit.entry <- tkentry(entryform, width = 50, textvariable=hitid)
                tkgrid(tklabel(entryform, text = "HITId: "), row=r, column=2)
                tkgrid(hit.entry, row=r, column=3)
                r <- r + 1
                assign.entry <- tkentry(entryform, width = 50, textvariable=assignments)
                tkgrid(tklabel(entryform, text = "Number of Assignments to Add: "), row=r, column=2)
                tkgrid(assign.entry, row=r, column=3)
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
            tkgrid(entryform)
            # buttons
            buttons <- tkframe(addassignDialog)
                populate <- function(){
                    searchWiz()
                    tclvalue(hitid) <<- wizardenv$searchresult$HITId # retrieve hitid from wizardenv environment
                }
                populatebutton <- tkbutton(buttons, text="Search for HITs", command=populate)
                OKbutton <- tkbutton(buttons,text="   OK   ",command=addassign)
                Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {tkdestroy(addassignDialog); tkfocus(wizard)})
                r <- 1
                tkgrid(populatebutton, row=r, column=1)
                tkgrid(OKbutton, row=r, column=2)
                tkgrid(Cancelbutton, row=r, column=3)
            tkgrid(buttons)
            
            tkfocus(addassignDialog)
        }
        
        # add time to hit
        extendWiz <- function(){
            # function
            extend <- function(){
                if(tclvalue(hitid)==""){
                    tkmessageBox(message="Please enter a HITId!", type="ok")
                    tkfocus(extendDialog)
                }
                else if(tclvalue(days)=="" && tclvalue(hours)=="" && tclvalue(mins)=="" && tclvalue(secs)==""){
                    tkmessageBox(message="Please enter an amount of time!", type="ok")
                    tkfocus(extendDialog)
                }
                else {
                    tkdestroy(extendDialog)
                    ExtendHIT(    hit=tclvalue(hitid),
                                add.seconds=seconds(as.numeric(tclvalue(days)), 
                                                    as.numeric(tclvalue(hours)), 
                                                    as.numeric(tclvalue(mins)), 
                                                    as.numeric(tclvalue(secs))),
                                sandbox=sandbox
                            )
                    tkfocus(wizard)
                }
            }
            # layout
            extendDialog <- tktoplevel()
            tkwm.title(extendDialog, "Extend HIT")
            entryform <- tkframe(extendDialog, relief="groove", borderwidth=2)
                # times
                days <- tclVar("0")
                hours <- tclVar("0")
                mins <- tclVar("0")
                secs <- tclVar("0")
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=10)
                r <- r + 1
                days.entry <- tkentry(entryform, width = 5, textvariable=days)
                hours.entry <- tkentry(entryform, width = 5, textvariable=hours)
                mins.entry <- tkentry(entryform, width = 5, textvariable=mins)
                secs.entry <- tkentry(entryform, width = 5, textvariable=secs)
                tkgrid(tklabel(entryform, text = "Days: "), row=r, column=2)
                tkgrid(days.entry, row=r, column=3)
                tkgrid(tklabel(entryform, text = "Hours: "), row=r, column=4)
                tkgrid(hours.entry, row=r, column=5)
                tkgrid(tklabel(entryform, text = "Minutes: "), row=r, column=6)
                tkgrid(mins.entry, row=r, column=7)
                tkgrid(tklabel(entryform, text = "Seconds: "), row=r, column=8)
                tkgrid(secs.entry, row=r, column=9)
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                # hitid
                r <- r + 1
                hitid <- tclVar()
                hit.entry <- tkentry(entryform,text=tclvalue(hitid), width = 50)
                tkconfigure(hit.entry,textvariable=hitid)
                tkgrid(tklabel(entryform, text = "Enter HITId: "), row=r, column=2, columnspan=2)
                tkgrid(hit.entry, row=r, column=4, columnspan=6)
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
            tkgrid(entryform)
            # buttons
            buttons <- tkframe(extendDialog)
                populate <- function(){
                    searchWiz()
                    tclvalue(hitid) <<- wizardenv$searchresult$HITId # retrieve hitid from wizardenv environment
                }
                populatebutton <- tkbutton(buttons, text="Search for HITs", command=populate)
                OKbutton <- tkbutton(buttons, text="   OK   ", command=extend)
                Cancelbutton <- tkbutton(buttons, text=" Cancel ", command=function() {tkdestroy(extendDialog); tkfocus(wizard)})
                r <- 1
                tkgrid(populatebutton, row=r, column=1)
                tkgrid(OKbutton, row=r, column=2)
                tkgrid(Cancelbutton, row=r, column=3)
            tkgrid(buttons)
            
            tkfocus(extendDialog)
        }
        
        # expire hit
        expireWiz <- function(){
            # function
            expire <- function(){
                if(tclvalue(hitid)==""){
                    tkmessageBox(message="Please enter a HITId!", type="ok")
                    tkfocus(expireDialog)
                }
                else{
                    tkdestroy(expireDialog)
                    ExpireHIT(hit=tclvalue(hitid), sandbox=sandbox)
                }
            }
            # layout
            expireDialog <- tktoplevel()
            tkwm.title(expireDialog, "Expire HIT")
            entryform <- tkframe(expireDialog, relief="groove", borderwidth=2)
                # hitid
                hitid <- tclVar()
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=4)
                hit.entry <- tkentry(entryform, width = 50, textvariable=hitid)
                r <- r + 1
                tkgrid(tklabel(entryform, text = "Enter HITId: "), row=r, column=2)
                tkgrid(hit.entry, row=r, column=3)
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
            tkgrid(entryform)
            # buttons
            buttons <- tkframe(expireDialog)
                populate <- function(){
                    searchWiz()
                    tclvalue(hitid) <<- wizardenv$searchresult$HITId # retrieve hitid from wizardenv environment
                }
                populatebutton <- tkbutton(buttons, text="Search for HITs", command=populate)
                OKbutton <- tkbutton(buttons,text="   OK   ",command=expire)
                Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {tkdestroy(expireDialog); tkfocus(wizard)})
                r <- 1
                tkgrid(populatebutton, row = r, column = 1)
                tkgrid(OKbutton, row = r, column = 2)
                tkgrid(Cancelbutton, row=r, column = 3)
            tkgrid(buttons)
            
            tkfocus(expireDialog)
        }
        
        # dispose hit
        disposeWiz <- function(){
            # function
            dispose <- function(){
                if(tclvalue(hitid)==""){
                    tkmessageBox(message="Please enter a HITId!", type="ok")
                    tkfocus(disposeDialog)
                }
                else {
                    exit <- tkmessageBox(message = "Are you sure you want to dispose the HIT? This will delete all HIT and Assignment data.",
                                        icon = "question", type = "yesno", default = "no")
                    if(tclvalue(exit)=="yes"){
                        tkdestroy(disposeDialog)
                        DisposeHIT(hit=tclvalue(hitid), sandbox=sandbox)
                    }
                    else{
                        tkfocus(disposeDialog)
                    }
                }
            }
            # layout
            disposeDialog <- tktoplevel()
            tkwm.title(disposeDialog, "Dispose HIT")
            entryform <- tkframe(disposeDialog, relief="groove", borderwidth=2)
                # hitid
                hitid <- tclVar()
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=4)
                r <- r + 1
                hit.entry <- tkentry(entryform, width = 50, textvariable=hitid)
                tkgrid(tklabel(entryform, text = "HITId: "), row=r, column=2)
                tkgrid(hit.entry, row=r, column=3)
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
            tkgrid(entryform)
            # buttons
            buttons <- tkframe(disposeDialog)
                r <- 1
                populate <- function(){
                    searchWiz()
                    tclvalue(hitid) <<- wizardenv$searchresult$HITId # retrieve hitid from wizardenv environment
                }
                populatebutton <- tkbutton(buttons, text="Search for HITs", command=populate)
                OKbutton <- tkbutton(buttons,text="   OK   ",command=dispose)
                Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {tkdestroy(disposeDialog); tkfocus(wizard)})
                tkgrid(populatebutton, row = r, column = 1)
                tkgrid(OKbutton, row = r, column = 2)
                tkgrid(Cancelbutton, row=r, column = 3)
            tkgrid(buttons)
            
            tkfocus(disposeDialog)
        }
        
        # search HITs
        searchWiz <- function(){
            results <- SearchHITs(verbose=FALSE)
            #print(results$HITs[,c("HITId","HITTypeId","RequesterAnnotation")])
            # populate scrollable listbox
            if(!is.null(results)) {
                # layout
                currenthits <-tktoplevel()
                tkgrab.set(currenthits)
                tkwm.title(currenthits, "Current HITs")
                entryform <- tkframe(currenthits)
                    r <- 1
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=4)
                    r <- r + 1
                    scr <- tkscrollbar(entryform, repeatinterval=5, command=function(...) tkyview(hitlist,...))
                    hitlist <- tklistbox(    entryform, height=8, width=100, selectmode="single",
                                            yscrollcommand=function(...) tkset(scr,...), background="white")
                    tkgrid(hitlist, scr, row=r, column=2, columnspan=2)
                    tkgrid.configure(scr, column=3, sticky="nsw")
                    for (i in 1:dim(results$HITs)[1]) {
                        tkinsert(hitlist,"end",paste(results$HITs$RequesterAnnotation[i]," (HITId: ",results$HITs$HITId[i],")",sep=""))
                    }
                    selecthit <- function(){
                        pos <- as.numeric(as.character(tkcurselection(hitlist)))+1 # listbox index starts at 0
                        selection <- results$HITs[pos,]
                        tkgrab.release(currenthits)
                        tkdestroy(currenthits)
                        assign("searchresult", selection, envir=wizardenv) # write tclvar to wizardenv environment
                    }
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(entryform)
                # buttons
                buttons <- tkframe(currenthits)
                    r <- r + 1
                    OKbutton <- tkbutton(buttons,text="   OK   ",command=selecthit)
                    Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {tkdestroy(currenthits); tkfocus(wizard)})
                    tkgrid(OKbutton, row=r, column=1)
                    tkgrid(Cancelbutton, row=r, column=2)
                tkgrid(buttons)
                
                tkfocus(currenthits)
                tkwait.window(currenthits)
            }
            else {
                tkmessageBox(message="No HITs found!", icon="info", type="ok")
                tkfocus(wizard)
            }        
        }
        
        # get assignments
        getassign1Wiz <- function(){    # by AssignmentId
            getassign <- function(verbose,save){
                if(tclvalue(assignment)==""){
                    tkmessageBox(message="Please enter an AssignmentId!", type="ok")
                    tkfocus(getassign1Dialog)
                }
                else {
                    tkdestroy(getassign1Dialog)
                    if(verbose){
                        results <- GetAssignment(assignment=tclvalue(assignment),verbose=TRUE, sandbox=sandbox)
                        print(results)
                    } else
                        results <- GetAssignment(assignment=tclvalue(assignment),verbose=FALSE, sandbox=sandbox)
                    if(save==TRUE)
                        savetofile(results)
                    tkfocus(wizard)
                }
            }
            # layout
            getassign1Dialog <- tktoplevel()
            tkwm.title(getassign1Dialog, "Get Assignment")
            entryform <- tkframe(getassign1Dialog, relief="groove", borderwidth=2)
                entryform <- tkframe(getassign1Dialog, relief="groove", borderwidth=2)
                assignment <- tclVar()
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
                assign.entry <- tkentry(entryform, width = 50, textvariable=assignment)
                r <- r + 1
                tkgrid(tklabel(entryform, text = "Enter AssignmentId: "), row=r, column=1)
                tkgrid(assign.entry, row=r, column=2, columnspan=4)
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
            tkgrid(entryform)
            # buttons
            buttons <- tkframe(getassign1Dialog)
                printbutton <- tkbutton(buttons, text=" Print to Console ", command=function() getassign(TRUE,FALSE) )
                savebutton <- tkbutton(buttons, text=" Save to File ", command=function() getassign(FALSE,TRUE) )
                Cancelbutton <- tkbutton(buttons, text=" Cancel ", command=function() {tkdestroy(getassign1Dialog); tkfocus(wizard)})
                r <- 1
                tkgrid(printbutton, row = r, column = 1)
                tkgrid(savebutton, row = r, column = 2)
                tkgrid(Cancelbutton, row=r, column = 3)
            tkgrid(buttons)
            
            tkfocus(getassign1Dialog)
        }
        
        getassign2Wiz <- function(){    # by HITId
            getassign <- function(verbose,save){
                if(tclvalue(hitid)==""){
                    tkmessageBox(message="Please enter a HITId!", type="ok")
                    tkfocus(getassign2Dialog)
                }
                else {
                    tkdestroy(getassign2Dialog)
                    if(verbose){
                        results <- GetAssignment(hit=tclvalue(hitid),verbose=TRUE, sandbox=sandbox)
                        print(results)
                    } else
                        results <- GetAssignment(hit=tclvalue(hitid),verbose=FALSE, sandbox=sandbox)
                    if(save==TRUE)
                        savetofile(results)
                    tkfocus(wizard)
                }
            }
            # layout
            getassign2Dialog <- tktoplevel()
            tkwm.title(getassign2Dialog, "Get Assignment")
            entryform <- tkframe(getassign2Dialog,relief="groove",borderwidth=2)
                hitid <- tclVar()
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
                hit.entry <- tkentry(entryform, width = 50, textvariable=hitid)
                r <- r + 1
                tkgrid(tklabel(entryform, text = "Enter HITId: "), row=r, column=1)
                tkgrid(hit.entry, row=r, column=2, columnspan=4)
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
            tkgrid(entryform)
            # buttons
            buttons <- tkframe(getassign2Dialog)
                populate <- function(){
                    searchWiz()
                    tclvalue(hitid) <<- wizardenv$searchresult$hitid # retrieve hitid from wizardenv environment
                }
                populatebutton <- tkbutton(buttons, text="Search for HITs", command=populate)
                printbutton <- tkbutton(buttons, text=" Print to Console ", command=function() getassign(TRUE,FALSE) )
                savebutton <- tkbutton(buttons, text=" Save to File ", command=function() getassign(FALSE,TRUE) )
                Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {tkdestroy(getassign2Dialog); tkfocus(wizard)})
                r <- 1
                tkgrid(populatebutton, row = r, column = 1)
                tkgrid(printbutton, row = r, column = 2)
                tkgrid(savebutton, row = r, column = 3)
                tkgrid(Cancelbutton, row=r, column = 4)
            tkgrid(buttons)
            
            tkfocus(getassign2Dialog)
        }
        
        getassign3Wiz <- function(){    # by HITTypeId
            getassign <- function(verbose,save){
                if(tclvalue(hittype)==""){
                    tkmessageBox(message="Please enter a HITTypeId!", type="ok")
                    tkfocus(getassign3Dialog)
                }
                else {
                    tkdestroy(getassign3Dialog)
                    if(verbose){
                        results <- GetAssignment(hit.type=tclvalue(hittype),verbose=TRUE, sandbox=sandbox)
                        print(results)
                    } else
                        results <- GetAssignment(hit.type=tclvalue(hittype),verbose=FALSE, sandbox=sandbox)
                    if(save==TRUE)
                        savetofile(results)
                    
                    tkfocus(wizard)
                }
            }
            # layout
            getassign3Dialog <- tktoplevel()
            tkwm.title(getassign3Dialog, "Get Assignment")
            entryform <- tkframe(getassign3Dialog,relief="groove",borderwidth=2)
                hittype <- tclVar()
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
                hittype.entry <- tkentry(entryform, width = 50, textvariable=hittype)
                r <- r + 1
                tkgrid(tklabel(entryform, text = "Enter HITTypeId: "), row=r, column=1)
                tkgrid(hittype.entry, row=r, column=2, columnspan=4)
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
            tkgrid(entryform)
            # buttons
            buttons <- tkframe(getassign3Dialog)
                populate <- function(){
                    searchWiz()
                    tclvalue(hittype) <<- wizardenv$searchresult$HITTypeId # retrieve hittypeid from wizardenv environment
                }
                populatebutton <- tkbutton(buttons, text="Search for HITType (by HIT)", command=populate)
                printbutton <- tkbutton(buttons, text=" Print to Console ", command=function() getassign(TRUE,FALSE) )
                savebutton <- tkbutton(buttons, text=" Save to File ", command=function() getassign(FALSE,TRUE) )
                Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {tkdestroy(getassign3Dialog); tkfocus(wizard)})
                r <- 1
                tkgrid(populatebutton, row = r, column = 1)
                tkgrid(printbutton, row = r, column = 2)
                tkgrid(savebutton, row = r, column = 3)
                tkgrid(Cancelbutton, row=r, column = 4)
            tkgrid(buttons)
            
            tkfocus(getassign3Dialog)
        }
        
        
        # approve assignment(s)
        approveWiz <- function(){
            # approveall function
            approveall <- function(){
                # function
                approveallforhit <- function(){
                    if(tclvalue(hitid)==""){
                        tkmessageBox(message="Please enter a HITId!", type="ok")
                        tkfocus(approveallDialog)
                    }
                    else {
                        exit <- tkmessageBox(message = "Are you sure you want to approve all assignments for the HIT?",
                                        icon = "question", type = "yesno", default = "no")
                        if(tclvalue(exit)=="yes"){
                            tkdestroy(approveallDialog)
                            ApproveAllAssignments(hit=tclvalue(hitid), sandbox=sandbox)
                            tkfocus(wizard)
                        }
                        else
                            tkfocus(approveallDialog)
                    }
                }
                # layout
                tkdestroy(approveDialog)
                approveallDialog <- tktoplevel()
                tkwm.title(approveallDialog, "Approve All Assignments")
                entryform <- tkframe(approveallDialog, relief="groove", borderwidth=2)
                    hitid <- tclVar()
                    r <- 1
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=5)
                    r <- r + 1
                    hit.entry <- tkentry(entryform, width = 50, textvariable=hitid)
                    tkgrid(tklabel(entryform, text = "Enter HITId: "), row=r, column=2)
                    tkgrid(hit.entry, row=r, column=3)
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(entryform)
                # buttons
                populate <- function(){
                    searchWiz(tclvar="hitid")
                    tclvalue(hitid) <<- wizardenv$hitid # retrieve hitid from wizardenv environment
                }
                buttons <- tkframe(approveallDialog)
                    populatebutton <- tkbutton(buttons, text="Search for HITs", command=populate)
                    OKbutton <- tkbutton(buttons,text="   OK   ",command=approveallforhit)
                    Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {tkdestroy(approveallDialog); tkfocus(wizard)})
                    tkgrid(populatebutton, OKbutton, Cancelbutton)
                tkgrid(buttons)
                tkfocus(approveallDialog)
            }
            # approve1 function
            approve1 <- function(){
                if(tclvalue(feedback)=="")
                    feedbackvalue <- NULL
                else if(tclvalue(assignment)==""){
                    tkmessageBox(message="Please enter an AssignmentId!", type="ok")
                    tkfocus(approveDialog)
                }
                else {
                    ApproveAssignment(assignments=tclvalue(assignment), feedback=feedbackvalue, verbose=TRUE, sandbox=sandbox)
                    tkdestroy(approveDialog)
                    tkfocus(wizard)
                }
            }
            # layout
            approveDialog <- tktoplevel()
            tkwm.title(approveDialog, "Approve Assignments")
            entryform <- tkframe(approveDialog, relief="groove", borderwidth=2)
                approveallbutton <- tkbutton(entryform,text="Approve All Assignments for a HIT?",command=approveall)
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=4)
                r <- r + 1
                tkgrid(approveallbutton, row=r, columnspan=4)
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
                assignment <- tclVar()
                assign.entry <- tkentry(entryform, width = 50, textvariable=assignment)
                r <- r + 1
                tkgrid(tklabel(entryform, text = "Enter AssignmentId: "), row=r, column=2)
                tkgrid(assign.entry, row=r, column=3)
                r <- r + 1
                feedback <- tclVar()
                feedback.entry <- tkentry(entryform, width = 50, textvariable=feedback)
                tkgrid(tklabel(entryform, text = "Enter Feedback (optional): "), row=r, column=2)
                tkgrid(feedback.entry, row=r, column=3)
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
            tkgrid(entryform)
            # buttons
            buttons <- tkframe(approveDialog)
                OKbutton <- tkbutton(buttons,text="   OK   ",command=approve1)
                Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {tkdestroy(approveDialog); tkfocus(wizard)})
                r <- 1
                tkgrid(OKbutton, row = r, column = 1)
                tkgrid(Cancelbutton, row=r, column = 2)
            tkgrid(buttons)
            
            tkfocus(approveDialog)
        }
        
        # reject assignment(s)
        rejectWiz <- function(){
            
            # function
            reject <- function() {
                if(tclvalue(feedback)=="") {
                    tkmessageBox(message="Please enter a reason for rejection!", type="ok")
                    tkfocus(rejectDialog)
                }
                else if(tclvalue(assignment)==""){
                    tkmessageBox(message="Please enter an AssignmentId!", type="ok")
                    tkfocus(rejectDialog)
                }
                else {
                    RejectAssignment(assignments=tclvalue(assignment), feedback=tclvalue(feedback), verbose=TRUE, sandbox=sandbox)
                    tkdestroy(rejectDialog)
                    tkfocus(wizard)
                }
            }
            
            # layout
            rejectDialog <- tktoplevel()
            tkwm.title(rejectDialog, "Reject Assignments")
            entryform <- tkframe(rejectDialog, relief="groove", borderwidth=2)
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=4)
                r <- r + 1
                assignment <- tclVar()
                assign.entry <- tkentry(entryform, width = 50, textvariable=assignment)
                tkgrid(tklabel(entryform, text = "Enter AssignmentId: "), row=r, column=2)
                tkgrid(assign.entry, row=r, column=3)
                r <- r + 1
                feedback <- tclVar()
                feedback.entry <- tkentry(entryform, width = 50, textvariable=feedback)
                tkgrid(tklabel(entryform, text = "Enter Feedback (required): "), row=r, column=2)
                tkgrid(feedback.entry, row=r, column=3)
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
            tkgrid(entryform)
            # buttons
            buttons <- tkframe(rejectDialog)
                OKbutton <- tkbutton(buttons,text="   OK   ",command=reject)
                Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {tkdestroy(rejectDialog); tkfocus(wizard)})
                r <- 1
                tkgrid(OKbutton, row = r, column = 1)
                tkgrid(Cancelbutton, row=r, column = 2)
            tkgrid(buttons)
                    
            tkfocus(rejectDialog)
        }
        
        
        
        
        
        
        
        # approve/reject assignment dialog
        approverejectWiz <- function(type){
            if(is.null(type))
                invisible(NULL)
            else{
                # function: get assignment data and send to listboxes
                populate <- function(type) {
                    if(type=="hit"){
                        searchWiz()
                        hitid <- wizardenv$searchresult$HITId # retrieve hitid from wizardenv environment
                        results <- GetAssignments(hit=hitid, return.all=TRUE, verbose=FALSE, sandbox=sandbox)
                    }
                    else if(type=="hittype"){
                        searchWiz()
                        hittypeid <- wizardenv$searchresult$HITTypeId # retrieve hittypeid from wizardenv environment
                        results <- GetAssignments(hit.type=hittypeid, return.all=TRUE, verbose=FALSE, sandbox=sandbox)
                    }
                    assign("assignments",results,envir=wizardenv)
                    submitted <- results[results$AssignmentStatus=="Submitted",]
                    approved <- results[results$AssignmentStatus=="Approved",]
                    rejected <- results[results$AssignmentStatus=="Rejected",]
                    assign("submitted",submitted,envir=wizardenv)
                    assign("approved",approved,envir=wizardenv)
                    assign("rejected",rejected,envir=wizardenv)
                    # fill listboxes w/assignment data
                    if(dim(submitted)[1]>0){
                        for (i in 1:dim(submitted)[1]) {
                            tkinsert(sublist,"end",submitted$AssignmentId[i])
                        }
                    }
                    if(dim(approved)[1]>0){
                        for (i in 1:dim(approved)[1]) {
                            tkinsert(isapplist,"end",approved$AssignmentId[i])
                        }
                    }
                    if(dim(rejected)[1]>0){
                        for (i in 1:dim(rejected)[1]) {
                            tkinsert(isrejlist,"end",rejected$AssignmentId[i])
                        }
                    }
                    tkfocus(approverejectDialog)
                }
                # function: view assignment
                view <- function(type){
                    if(type=="submitted"){
                        pos <- as.numeric(tkcurselection(sublist))+1
                        assignid <- wizardenv$submitted$AssignmentId[pos]
                    }
                    if(type=="approved"){
                        pos <- as.numeric(tkcurselection(isapplist))+1
                        assignid <- wizardenv$approved$AssignmentId[pos]
                    }
                    if(type=="rejected"){
                        pos <- as.numeric(tkcurselection(isrejlist))+1
                        assignid <- wizardenv$rejected$AssignmentId[pos]
                    }
                    assignment <- wizardenv$assignments[wizardenv$assignments$AssignmentId==assignid,]
                    
                    # function to print assignment details to console
                    printdetails <- function(){
                        message("Details for Assignment ",assignment$AssignmentId,":")
                        hitvars <- assignment[1,12:(dim(assignment)[2]-1)]
                        for(i in 1:length(hitvars)){
                            message(names(assignment)[11+i], ": ", hitvars[i])
                        }
                        message()
                    }
                    # layout
                    viewassign <- tktoplevel()
                    tkwm.title(viewassign, "View Assignment")
                    entryform <- tkframe(viewassign, relief="groove", borderwidth=2)
                        r <- 1
                        tkgrid(ttklabel(entryform, text="     "), row=r, column=1)
                        tkgrid(ttklabel(entryform, text="     "), row=r, column=4)
                        r <- r + 1
                        tkgrid(ttklabel(entryform, text="AssignmentId: "), row=r, column=2, sticky="e")
                        tkgrid(ttklabel(entryform, text=assignment$AssignmentId), row=r, column=3, sticky="w")
                        r <- r + 1
                        tkgrid(ttklabel(entryform, text="WorkerId: "), row=r, column=2, sticky="e")
                        tkgrid(ttklabel(entryform, text=assignment$WorkerId), row=r, column=3, sticky="w")
                        r <- r + 1
                        tkgrid(ttklabel(entryform, text="HITId: "), row=r, column=2, sticky="e")
                        tkgrid(ttklabel(entryform, text=assignment$HITId), row=r, column=3, sticky="w")
                        r <- r + 1
                        tkgrid(ttklabel(entryform, text="AssignmentStatus: "), row=r, column=2, sticky="e")
                        tkgrid(ttklabel(entryform, text=assignment$AssignmentStatus), row=r, column=3, sticky="w")
                        r <- r + 1
                        tkgrid(ttklabel(entryform, text="AutoApprovalTime: "), row=r, column=2)
                        tkgrid(ttklabel(entryform, text=assignment$AutoApprovalTime), row=r, column=3, sticky="w")
                        r <- r + 1
                        tkgrid(ttklabel(entryform, text="AcceptTime: "), row=r, column=2, sticky="e")
                        tkgrid(ttklabel(entryform, text=assignment$AcceptTime), row=r, column=3, sticky="w")
                        r <- r + 1
                        tkgrid(ttklabel(entryform, text="SubmitTime: "), row=r, column=2, sticky="e")
                        tkgrid(ttklabel(entryform, text=assignment$SubmitTime), row=r, column=3, sticky="w")
                        r <- r + 1
                        tkgrid(ttklabel(entryform, text="Work Time (seconds): "), row=r, column=2, sticky="e")
                        tkgrid(ttklabel(entryform, text=assignment$SecondsOnHIT), row=r, column=3, sticky="w")
                        r <- r + 1
                        tkgrid(ttklabel(entryform, text="ApprovalTime: "), row=r, column=2, sticky="e")
                        tkgrid(ttklabel(entryform, text=assignment$ApprovalTime), row=r, column=3, sticky="w")
                        r <- r + 1
                        tkgrid(ttklabel(entryform, text="RejectionTime: "), row=r, column=2, sticky="e")
                        tkgrid(ttklabel(entryform, text=assignment$RejectionTime), row=r, column=3, sticky="w")
                        r <- r + 1
                        tkgrid(ttklabel(entryform, text="     "), row=r, column=1)
                    tkgrid(entryform)
                    # buttons
                    buttons <- tkframe(viewassign)
                        AnswerDetails <- tkbutton(buttons,text=" View Assignment Answers ",command=printdetails)
                        OKbutton <- tkbutton(buttons,text="   OK   ",command=function() {tkdestroy(viewassign); tkfocus(approverejectDialog)})
                        r <- 1
                        tkgrid(AnswerDetails, row = r, column = 1)
                        tkgrid(OKbutton, row=r, column = 2)
                    tkgrid(buttons)
                    
                    tkfocus(viewassign)
                }
                
                # function: approve
                appfun <- function(){
                    # function
                    app <- function(){
                        if(as.character(tkcurselection(sublist))==""){
                            tkmessageBox(message="Please select an assignment!", type="ok")
                            tkfocus(appreason)
                        }
                        else{
                            if(tclvalue(reason)=="")
                                reason <- NULL
                            tkdestroy(appreason)
                            pos <- as.numeric(tkcurselection(sublist))+1
                            assignid <- wizardenv$submitted$AssignmentId[pos]
                            ApproveAssignment(assignments=assignid, feedback=reason, verbose=FALSE, sandbox=sandbox)
                            tkinsert(sublist,tkcurselection(sublist),"Approved")
                            tkdelete(sublist,tkcurselection(sublist))
                            tkfocus(approverejectDialog)
                        }
                    }
                    # layout
                    appreason <- tktoplevel()
                    tkwm.title(appreason, "Reason for rejection?")
                    r <- 1
                    tkgrid(ttklabel(appreason, text = "     "), row=r, column=1)
                    tkgrid(ttklabel(appreason, text = "     "), row=r, column=4)
                    r <- r + 1
                    reason <- tclVar()
                    reason.entry <- tkentry(appreason, width=50, textvariable=reason)
                    tkgrid(tklabel(appreason, text = "Reason (optional; visible to worker): "), row=r, column=2, sticky="e")
                    tkgrid(reason.entry, row=r, column=3, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(appreason, text = "     "), row=r, column=1)
                    OKbutton <- tkbutton(appreason,text="   OK   ",command=app)
                    Cancelbutton <- tkbutton(appreason,text=" Cancel ",command=function() tkdestroy(appreason) )
                    r <- r + 1
                    tkgrid(OKbutton, row = r, column = 2)
                    tkgrid(Cancelbutton, row=r, column = 3)
                }
                
                # function: reject
                rejfun <- function(){
                    # function
                    rej <- function(){
                        if(as.character(tkcurselection(sublist))==""){
                            tkmessageBox(message="Please select an assignment!", type="ok")
                            tkfocus(rejreason)
                        }
                        if(tclvalue(reason)==""){
                            tkmessageBox(message="Please enter a reason for the rejection!", type="ok")
                            tkfocus(rejreason)
                        }
                        else{
                            tkdestroy(rejreason)
                            pos <- as.numeric(tkcurselection(sublist))+1
                            assignid <- wizardenv$submitted$AssignmentId[pos]
                            RejectAssignment(assignments=assignid, feedback=reason, verbose=FALSE, sandbox=sandbox)
                            tkinsert(sublist,tkcurselection(sublist),"Rejected")
                            tkdelete(sublist,tkcurselection(sublist))
                            tkfocus(approverejectDialog)
                        }
                    }
                    # layout
                    rejreason <- tktoplevel()
                    tkwm.title(rejreason, "Reason for approval?")
                    r <- 1
                    tkgrid(ttklabel(rejreason, text = "     "), row=r, column=1)
                    tkgrid(ttklabel(rejreason, text = "     "), row=r, column=4)
                    r <- r + 1
                    reason <- tclVar()
                    reason.entry <- tkentry(rejreason, width=50, textvariable=reason)
                    tkgrid(tklabel(rejreason, text = "Reason (required; visible to worker): "), row=r, column=2, sticky="e")
                    tkgrid(reason.entry, row=r, column=3, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(rejreason, text = "     "), row=r, column=1)
                    OKbutton <- tkbutton(rejreason,text="   OK   ",command=rej)
                    Cancelbutton <- tkbutton(rejreason,text=" Cancel ",command=function() tkdestroy(rejreason) )
                    r <- r + 1
                    tkgrid(OKbutton, row = r, column = 2)
                    tkgrid(Cancelbutton, row=r, column = 3)
                }
                
                # function: approve rejected
                apprejected <- function(){
                    # function
                    app <- function(){
                        if(as.character(tkcurselection(isrejlist))==""){
                            tkmessageBox(message="Please select an assignment!", type="ok")
                            tkdestroy(appreason)
                            tkfocus(approverejectDialog)
                        }
                        else{
                            if(tclvalue(reason)=="")
                                reason <- NULL
                            tkdestroy(appreason)
                            pos <- as.numeric(tkcurselection(isrejlist))+1
                            assignid <- wizardenv$rejected$AssignmentId[pos]
                            ApproveAssignment(assignments=assignid, feedback=reason, rejected=TRUE, verbose=FALSE, sandbox=sandbox)
                            tkinsert(isrejlist,tkcurselection(isrejlist),"Approved")
                            tkdelete(isrejlist,tkcurselection(isrejlist))
                            tkfocus(approverejectDialog)
                        }
                    }
                    # layout
                    appreason <- tktoplevel()
                    tkwm.title(appreason, "Reason for approval?")
                    r <- 1
                    tkgrid(ttklabel(appreason, text = "     "), row=r, column=1)
                    tkgrid(ttklabel(appreason, text = "     "), row=r, column=4)
                    r <- r + 1
                    reason <- tclVar()
                    reason.entry <- tkentry(appreason, width=50, textvariable=reason)
                    tkgrid(tklabel(appreason, text = "Reason (optional; visible to worker): "), row=r, column=2, sticky="e")
                    tkgrid(reason.entry, row=r, column=3, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(appreason, text = "     "), row=r, column=1)
                    OKbutton <- tkbutton(appreason,text="   OK   ",command=app)
                    Cancelbutton <- tkbutton(appreason,text=" Cancel ",command=function() tkdestroy(appreason) )
                    r <- r + 1
                    tkgrid(OKbutton, row = r, column = 2)
                    tkgrid(Cancelbutton, row=r, column = 3)
                }
                
                # function: pay bonus
                bonusfun <- function(){
                    # function
                    b <- function(){
                        if(as.character(tkcurselection(sublist))==""){
                            tkmessageBox(message="Please select an assignment!", type="ok")
                            tkdestroy(bonusreason)
                            tkfocus(approverejectDialog)
                        } else if(tclvalue(amount)=="") {
                            tkmessageBox(message="Please enter a bonus amount!", type="ok")
                            tkfocus(bonusreason)
                        } else {
                            if(tclvalue(reason)=="")
                                reason <- NULL
                            tkdestroy(bonusreason)
                            pos <- as.numeric(tkcurselection(sublist))+1
                            assignid <- wizardenv$submitted$AssignmentId[pos]
                            workerid <- wizardenv$submitted$WorkerId[pos]
                            GrantBonus(workers = workerid, assignments = assignid, amounts = amount,
                                       reasons = reason, verbose=FALSE, sandbox=sandbox)
                            tkfocus(approverejectDialog)
                        }
                    }
                    # layout
                    bonusreason <- tktoplevel()
                    tkwm.title(bonusreason, "Bonus Amount")
                    r <- 1
                    tkgrid(ttklabel(bonusreason, text = "     "), row=r, column=1)
                    tkgrid(ttklabel(bonusreason, text = "     "), row=r, column=4)
                    r <- r + 1
                    amount <- tclVar()
                    amount.entry <- tkentry(bonusreason, width=50, textvariable=amount)
                    tkgrid(tklabel(bonusreason, text = "Bonus amount: $"), row=r, column=2, sticky="e")
                    tkgrid(amount.entry, row=r, column=3, sticky="w")
                    r <- r + 1
                    reason <- tclVar()
                    reason.entry <- tkentry(bonusreason, width=50, textvariable=reason)
                    tkgrid(tklabel(bonusreason, text = "Bonus message (visible to worker): "), row=r, column=2, sticky="e")
                    tkgrid(reason.entry, row=r, column=3, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(bonusreason, text = "     "), row=r, column=1)
                    OKbutton <- tkbutton(bonusreason,text="   OK   ",command=b)
                    Cancelbutton <- tkbutton(bonusreason,text=" Cancel ",command=function() tkdestroy(bonusreason) )
                    r <- r + 1
                    tkgrid(OKbutton, row = r, column = 2)
                    tkgrid(Cancelbutton, row=r, column = 3)                    
                }
                
                # layout
                approverejectDialog <- tktoplevel()
                tkwm.title(approverejectDialog, "Approve and/or Reject Assignments")
                entryform <- tkframe(approverejectDialog, relief="groove", borderwidth=2)
                    r <- 1
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=11)
                    r <- r + 1
                    tkgrid(tklabel(entryform, text = "Submitted Assignments: "), row=r, column=2, columnspan=2)
                    tkgrid(tklabel(entryform, text = "Approved Assignments: "), row=r, column=6)
                    tkgrid(tklabel(entryform, text = "Rejected Assignments: "), row=r, column=9)
                    scr1 <- tkscrollbar(entryform, repeatinterval=5, command=function(...) tkyview(sublist,...))
                    scr2 <- tkscrollbar(entryform, repeatinterval=5, command=function(...) tkyview(isapplist,...))
                    scr3 <- tkscrollbar(entryform, repeatinterval=5, command=function(...) tkyview(isrejlist,...))
                    sublist <- tklistbox(entryform, height=25, width=35, selectmode="multiple",
                                            yscrollcommand=function(...) tkset(scr1,...), background="white")
                    isapplist <- tklistbox(entryform, height=25, width=35, selectmode="single",
                                            yscrollcommand=function(...) tkset(scr2,...), background="white")
                    isrejlist <- tklistbox(entryform, height=25, width=35, selectmode="single",
                                            yscrollcommand=function(...) tkset(scr3,...), background="white")
                    r <- r + 1
                    tkgrid(sublist, scr1, row=r, column=2, columnspan=2, rowspan=4)
                    tkgrid(isapplist, scr2, row=r, column=6, rowspan=4)
                    tkgrid(isrejlist, scr3, row=r, column=9, rowspan=4)
                    tkgrid.configure(scr1, row=r, column=4, sticky="nsw")
                    tkgrid.configure(scr2, row=r, column=7, sticky="nsw")
                    tkgrid.configure(scr3, row=r, column=10, sticky="nsw")
                    # buttons
                    r <- r + 4
                    viewbutton1 <- tkbutton(entryform,text=" View Assignment ",command=function() view(type="submitted"))
                    tkgrid(viewbutton1, row = r, column = 2, columnspan=3)
                    viewbutton2 <- tkbutton(entryform,text=" View Assignment ",command=function() view(type="approved"))
                    tkgrid(viewbutton2, row = r, column = 6, columnspan=2)
                    viewbutton3 <- tkbutton(entryform,text=" View Assignment ",command=function() view(type="rejected"))
                    tkgrid(viewbutton3, row = r, column = 9, columnspan=2)
                    r <- r + 1
                    appbutton <- tkbutton(entryform,text=" Approve Selected ",command=appfun)
                    tkgrid(appbutton, row = r, column = 2)
                    rejbutton <- tkbutton(entryform,text=" Reject Selected ",command=rejfun)
                    tkgrid(rejbutton, row = r, column = 3)
                    apprejectedbutton <- tkbutton(entryform,text=" Re-Approve Selected ",command=apprejected)
                    tkgrid(apprejectedbutton, row = r, column = 9, columnspan=2, rowspan=2)
                    r <- r + 3
                    popbutton <- tkbutton(entryform,text=" Repopulate Lists ",command=function(){
                        for(i in 0:(length(sublist)-1)){
                            tkdelete(sublist,i)
                        }
                        for(i in 0:(length(isapplist)-1)){
                            tkdelete(isapplist,i)
                        }
                        for(i in 0:(length(isrejlist)-1)){
                            tkdelete(isrejlist,i)
                        }
                        if(type=="hit")
                            populate("hit")
                        else if(type=="hit.type")
                            populate("hit.type")
                        } )
                    tkgrid(popbutton, row = r, column = 2, columnspan=3)
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(entryform)
                
                # fill listboxes with initial assignment statuses
                populate(type)
                
            }
        }
        
        
        # contact worker (single)
        contactWiz <- function(){
            # function
            contact <- function(){
                bodytowrite <- tclvalue(tkget(body.entry,"0.0","end"))
                if(tclvalue(emailsubject)==""){
                    tkmessageBox(message="Please enter an email subject!", type="ok")
                    tkfocus(contactDialog)
                }
                else if(nchar(bodytowrite)<=1){
                    tkmessageBox(message="Please enter an email message body!", type="ok")
                    tkfocus(contactDialog)
                }
                else if(tclvalue(workerid)==""){
                    tkmessageBox(message="Please enter a WorkerId!", type="ok")
                    tkfocus(contactDialog)
                }
                else if(nchar(tclvalue(emailsubject))>200){
                    tkmessageBox(message=paste("Email message body must be less than 200 characters.\nCurrent length is ",
                                        nchar(tclvalue(emailsubject))," characters", sep=""),
                                type="ok")
                    tkfocus(contactDialog)
                }
                else if(nchar(bodytowrite)>4096){
                    tkmessageBox(message=paste("Email message body must be less than 4096 characters.\nCurrent length is ",
                                        nchar(bodytowrite)," characters", sep=""),
                                type="ok")
                    tkfocus(contactDialog)
                }
                else{
                    workerid <- strsplit(workerid,',')[[1]]
                    ContactWorker(subjects = tclvalue(emailsubject),
                                  msgs = bodytowrite, workers = tclvalue(workerid),
                                  verbose = TRUE, batch = TRUE, sandbox = sandbox)
                    tkdestroy(contactDialog)
                    tkfocus(wizard)
                }
            }
            # layout
            contactDialog <- tktoplevel()
            tkwm.title(contactDialog, "Contact MTurk Worker(s) via Email")
            # workerid, emailsubject, msg body
            entryform <- tkframe(contactDialog, relief="groove", borderwidth=2)
                workerid <- tclVar()
                emailsubject <- tclVar()
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=4)
                r <- r + 1
                worker.entry <- tkentry(entryform, width = 50, textvariable=workerid)
                tkgrid(tklabel(entryform, text = "WorkerId(s) (comma-separated): "), row=r, column=2)
                tkgrid(worker.entry, row=r, column=3)
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
                r <- r + 1
                subject.entry <- tkentry(entryform, width = 50, textvariable=emailsubject)
                tkgrid(tklabel(entryform, text = "Email Subject (max 200 char.): "), row=r, column=2)
                tkgrid(subject.entry, row=r, column=3)
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
                r <- r + 1
                chars <- tclVar('')
                body.entry <- tktext(entryform, height = 8, width = 60)
                tkmark.set(body.entry,"insert","0.0")
                tkgrid(tklabel(entryform, text = "Email Body (max 4096 char.): "), row=r, column=2)
                r <- r + 1
                tkgrid(body.entry, row=r, column=2, columnspan=2)
                r <- r + 1
                tkgrid(tklabel(entryform, text='Number of characters:'), row=r, column=2, sticky='e')
                tkgrid(tklabel(entryform, textvariable = chars), row=r, column=3, sticky='w')
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
            tkgrid(entryform)
            # buttons
            buttons <- tkframe(contactDialog)
                OKbutton <- tkbutton(buttons,text="   OK   ",command=contact)
                Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {tkdestroy(contactDialog); tkfocus(wizard)})
                checkbutton <- tkbutton(buttons,text=" Check body length ",
                    command= function() {tclvalue(chars) <- as.character(nchar(curlEscape(tclvalue(tkget(body.entry,"0.0","end")))))})
                r <- 1
                tkgrid(OKbutton, row = r, column = 1)
                tkgrid(Cancelbutton, row=r, column = 2)
                tkgrid(checkbutton, row=r, column = 3)
            tkgrid(buttons)
            
            tkfocus(contactDialog)
        }      
        
        
        # grant bonus(es)
        bonusWiz <- function(){
            # function
            bonus <- function(){
                if(tclvalue(workerid)==""){
                    tkmessageBox(message="Please enter a WorkerId!", type="ok")
                    tkfocus(bonusDialog)
                }
                else if(tclvalue(assignmentid)==""){
                    tkmessageBox(message="Please enter an AssignmentId!", type="ok")
                    tkfocus(bonusDialog)
                }
                else if(tclvalue(amount)==""){
                    tkmessageBox(message="Please enter an amount (in US Dollars)!", type="ok")
                    tkfocus(bonusDialog)
                }
                else if(tclvalue(reason)==""){
                    tkmessageBox(message="Please enter a reason for the bonus!", type="ok")
                    tkfocus(bonusDialog)
                }
                else {
                    tkdestroy(bonusDialog)
                    GrantBonus( workers=tclvalue(workerid),
                                assignments=tclvalue(assignmentid),
                                amounts=tclvalue(amount),
                                reasons=tclvalue(reason),
                                verbose=TRUE, sandbox=sandbox)
                    tkfocus(wizard)
                }
            }
            
            # layout
            bonusDialog <- tktoplevel()
            tkwm.geometry(bonusDialog)
            tkwm.title(bonusDialog, "Bonus MTurk Worker")
            entryform <- tkframe(bonusDialog, relief="groove", borderwidth=2)
                # workerid, assignmentid, msg body
                workerid <- tclVar()
                assignmentid <- tclVar()
                amount <- tclVar()
                reason <- tclVar()
                tkgrid(ttklabel(entryform, text = "     "))
                worker.entry <- tkentry(entryform, width = 50, textvariable=workerid)
                tkgrid(tklabel(entryform, text = "WorkerId: "), worker.entry)
                tkgrid(ttklabel(entryform, text = "     "))
                assignment.entry <- tkentry(entryform, width = 50, textvariable=assignmentid)
                tkgrid(tklabel(entryform, text = "AssignmentId: "), assignment.entry)
                tkgrid(ttklabel(entryform, text = "     "))
                amount.entry <- tkentry(entryform, width = 50, textvariable=amount)
                tkgrid(tklabel(entryform, text = "Bonus amount: $"), amount.entry)
                tkgrid(ttklabel(entryform, text = "     "))
                reason.entry <- tkentry(entryform, width = 50, textvariable=reason)
                tkgrid(tklabel(entryform, text = "Reason for bonus: "), reason.entry)
                tkgrid(ttklabel(entryform, text = "     "))
            tkgrid(entryform)
            # buttons
            buttons <- tkframe(bonusDialog)
                OKbutton <- tkbutton(buttons,text="   OK   ",command=bonus)
                Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {tkdestroy(bonusDialog); tkfocus(wizard)})
                r <- 1
                tkgrid(OKbutton, row = r, column = 1)
                tkgrid(Cancelbutton, row=r, column = 2)
            tkgrid(buttons)
            
            tkfocus(bonusDialog)
        }
        
        # block worker(s)
        blockWiz <- function(){
            # function
            block <- function(){
                if(tclvalue(workerid)==""){
                    tkmessageBox(message="Please enter a WorkerId!", type="ok")
                    tkfocus(blockDialog)
                }
                else if(tclvalue(reason)==""){
                    tkmessageBox(message="Please enter a reason for block!", type="ok")
                    tkfocus(blockDialog)
                }
                else {
                    tkdestroy(blockDialog)
                    BlockWorker(workers=tclvalue(workerid), reasons=tclvalue(reason), verbose=TRUE, sandbox=sandbox)
                    tkfocus(wizard)
                }
            }
            # layout
            blockDialog <- tktoplevel()
            tkwm.title(blockDialog, "Block Worker")
            entryform <- tkframe(blockDialog, relief="groove", borderwidth=2)
                workerid <- tclVar()
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
                worker.entry <- tkentry(entryform, width = 50, textvariable=workerid)
                r <- r + 1
                tkgrid(tklabel(entryform, text = "Enter WorkerId: "), row=r, column=1, columnspan=3)
                tkgrid(worker.entry, row=r, column=4, columnspan=5)
                reason <- tclVar()
                r <- r + 1
                worker.entry <- tkentry(entryform, width = 50, textvariable=reason)
                tkgrid(tklabel(entryform, text = "Enter Reason (required): "), row=r, column=1, columnspan=3)
                tkgrid(worker.entry, row=r, column=4, columnspan=5)
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
            tkgrid(entryform)
            # buttons
            buttons <- tkframe(blockDialog)
                OKbutton <- tkbutton(buttons,text="   OK   ",command=block)
                Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {tkdestroy(blockDialog); tkfocus(wizard)})
                r <- 1
                tkgrid(OKbutton, row = r, column = 1)
                tkgrid(Cancelbutton, row=r, column = 2)
            tkgrid(buttons)
            
            tkfocus(blockDialog)
        }
        
        # unblock worker(s)
        unblockWiz <- function(){
            # function
            unblock <- function(){
                if(tclvalue(workerid)==""){
                    tkmessageBox(message="Please enter a WorkerId!", type="ok")
                    tkfocus(unblockDialog)
                }
                else {
                    tkdestroy(unblockDialog)
                    if(tclvalue(reason)=="")
                        UnblockWorker(workers=tclvalue(workerid), verbose=TRUE, sandbox=sandbox)
                    else
                        UnblockWorker(workers=tclvalue(workerid), reasons=tclvalue(reason), verbose=TRUE, sandbox=sandbox)
                    tkfocus(wizard)
                }
            }
            # layout
            unblockDialog <- tktoplevel()
            tkwm.title(unblockDialog, "Unblock Worker")
            entryform <- tkframe(unblockDialog, relief="groove", borderwidth=2)
                # hitid
                workerid <- tclVar()
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
                r <- r + 1
                worker.entry <- tkentry(entryform, width = 50, textvariable=workerid)
                tkgrid(tklabel(entryform, text = "Enter WorkerId: "), row=r, column=1, columnspan=3)
                tkgrid(worker.entry, row=r, column=4, columnspan=5)
                r <- r + 1
                reason <- tclVar()
                worker.entry <- tkentry(entryform, width = 50, textvariable=reason)
                tkgrid(tklabel(entryform, text = "Enter Reason (optional): "), row=r, column=1, columnspan=3)
                tkgrid(worker.entry, row=r, column=4, columnspan=5)
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
            tkgrid(entryform)
            # buttons
            buttons <- tkframe(unblockDialog)
                OKbutton <- tkbutton(buttons,text="   OK   ",command=unblock)
                Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {tkdestroy(unblockDialog); tkfocus(wizard)})
                r <- 1
                tkgrid(OKbutton, row = r, column = 1)
                tkgrid(Cancelbutton, row=r, column = 2)
            tkgrid(buttons)
            
            tkfocus(unblockDialog)
        }
        
        # get blocked worker(s)
        getblockWiz <- function(){
            blockedworkers <- GetBlockedWorkers(verbose=FALSE, sandbox=sandbox)
            # populate scrollable listbox
            if(!is.null(blockedworkers)) {
                # function
                selectworkers <- function(){
                    tkdestroy(blocklistDialog)
                    pos <- as.numeric(as.character(tkcurselection(workerlist)))-1 # listbox index starts at 0
                    selections <- blockedworkers$WorkerId[pos]
                    #tkmessageBox(message=paste("Check: ",selections,sep=""))
                    if(length(selections)==0)
                        invisible(NULL)
                    else
                        invisible(selections)
                }
                # layout
                blocklistDialog <-tktoplevel()
                tkwm.title(blocklistDialog, "Currently Blocked Workers")
                entryform <- tkframe(blocklistDialog, relief="groove", borderwidth=2)
                scr <- tkscrollbar(entryform, repeatinterval=5, command=function(...) tkyview(workerlist,...))
                workerlist <- tklistbox(entryform, height=8, selectmode="multiple", yscrollcommand=function(...) tkset(scr,...), background="white")
                tkgrid(workerlist,scr)
                tkgrid.configure(scr, rowspan=4, sticky="nsw")
                for (i in 1:dim(blockedworkers)[1]) {
                    tkinsert(workerlist,"end",blockedworkers$WorkerId[i])
                }
                tkgrid(entryform)
                # buttons
                buttons <- tkframe(blocklistDialog)
                    OKbutton <- tkbutton(buttons,text="   OK   ",command=selectworkers)
                    Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {tkdestroy(blocklistDialog); tkfocus(wizard)})
                    r <- 1
                    tkgrid(OKbutton, row = r, column = 1)
                    tkgrid(Cancelbutton, row=r, column = 2)
                tkgrid(buttons)
                
                tkfocus(blocklistDialog)
            }
            else {
                tkmessageBox(message="No blocked workers found!", icon="info", type="ok")
                tkfocus(wizard)
            }
        }
        
        # worker statistics
        workerstatWiz <- function(){
            # function
            getReport <- function(){
                if(tclvalue(workerid)==""){
                    tkmessageBox(message="Please enter a WorkerId!", type="ok")
                    tkfocus(statDialog)
                }
                else {
                    reportperiod <- periods[as.numeric(tkcurselection(period.entry))+1]
                    tkdestroy(statDialog)
                    #tkmessageBox(title = "", message = paste("Check: ",reportperiod,sep=""), type="ok")
                    WorkerReport(worker=tclvalue(workerid), period=reportperiod, sandbox=sandbox)
                    tkfocus(wizard)
                }
            }
            # layout
            statDialog <- tktoplevel()
            tkwm.title(statDialog, "MTurk Worker Statistics")
            entryform <- tkframe(statDialog, relief="groove", borderwidth=2)
                # workerid
                workerid <- tclVar()
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=6)
                r <- r + 1
                tkgrid(tklabel(entryform, text = "WorkerId: "), row=r, column=2, columnspan=2)
                worker.entry <- tkentry(entryform, width = 20, textvariable=workerid)
                tkgrid(worker.entry, row=r, column=4)
                # periods
                r <- r + 1
                periods <- c("OneDay","SevenDays","ThirtyDays","LifeToDate")
                period.entry <- tklistbox(entryform,height=length(periods),width=20, selectmode="single",background="white")
                tkgrid(tklabel(entryform,text="Report Period: "),row=r, column=2, columnspan=2)
                tkgrid(period.entry, row=r, column=4, rowspan=4)
                r <- r + 4
                tkgrid(ttklabel(entryform, text = "     "), row=r)
                for (i in 1:length(periods)) {
                    tkinsert(period.entry,"end",periods[i])
                }
                tkselection.set(period.entry,3)
            tkgrid(entryform)
            
            # buttons
            buttons <- tkframe(statDialog)
                OKbutton <- tkbutton(buttons,text="   OK   ",command=getReport)
                Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {tkdestroy(statDialog); tkfocus(wizard)})
                r <- 1
                tkgrid(OKbutton, row = r, column = 1)
                tkgrid(Cancelbutton, row=r, column = 2)
            tkgrid(buttons)
            
            tkfocus(statDialog)
        }
        
        # qualification test dialog
        qualtest <- function(){
            # function
            storetest <- function()    {
                testtowrite <- tclvalue(tkget(test.entry,"0.0","end"))
                answertowrite <- tclvalue(tkget(answer.entry,"0.0","end"))
                if(answertowrite=="")
                    answertowrite <- NULL
                testduration <- seconds(as.numeric(tclvalue(days)),
                                        as.numeric(tclvalue(hours)),
                                        as.numeric(tclvalue(mins)),
                                        as.numeric(tclvalue(secs)))
                test <- list(    Test=testtowrite,
                                AnswerKey=answertowrite,
                                TestDurationInSeconds=testduration )
                assign("test",test,envir=wizardenv)
            }
            # layout
            testDialog <- tktoplevel()
            tkwm.title(testDialog, "Configure Qualification Test")
            entryform <- tkframe(testDialog, relief="groove", borderwidth=2)
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=11)
                r <- r + 1
                test.entry <- tktext(entryform, height = 6, width = 75)
                tkmark.set(test.entry,"insert","0.0")
                tkgrid(tklabel(entryform, text = "Qualification Test: "), row=r, column=2, columnspan=9, sticky="w")
                r <- r + 1
                tkgrid(test.entry, row=r, column=2, columnspan=9)
                r <- r + 1
                answer.entry <- tktext(entryform, height = 6, width = 75)
                tkmark.set(answer.entry,"insert","0.0")
                tkgrid(tklabel(entryform, text = "Qualification Test AnswerKey (optional): "), row=r, column=2, columnspan=9, sticky="w")
                r <- r + 1
                tkgrid(answer.entry, row=r, column=2, columnspan=9)
                r <- r + 1
                tkgrid(tklabel(entryform, text = "How long should the test last?"), row=r, column=2)
                days <- tclVar("0")
                hours <- tclVar("0")
                mins <- tclVar("0")
                secs <- tclVar("0")
                days.entry <- tkentry(entryform, width = 5, textvariable=days)
                hours.entry <- tkentry(entryform, width = 5, textvariable=hours)
                mins.entry <- tkentry(entryform, width = 5, textvariable=mins)
                secs.entry <- tkentry(entryform, width = 5, textvariable=secs)
                tkgrid(tklabel(entryform, text = "Days: "), row=r, column=3)
                tkgrid(days.entry, row=r, column=4)
                tkgrid(tklabel(entryform, text = "Hours: "), row=r, column=5)
                tkgrid(hours.entry, row=r, column=6)
                tkgrid(tklabel(entryform, text = "Minutes: "), row=r, column=7)
                tkgrid(mins.entry, row=r, column=8)
                tkgrid(tklabel(entryform, text = "Seconds: "), row=r, column=9)
                tkgrid(secs.entry, row=r, column=10)
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
            tkgrid(entryform)
            
            # buttons
            buttons <- tkframe(testDialog)
                OKbutton <- tkbutton(buttons,text="   OK   ",command=storetest)
                Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() tkdestroy(testDialog) )
                r <- 1
                tkgrid(OKbutton, row = r, column = 1)
                tkgrid(Cancelbutton, row=r, column = 2)
            tkgrid(buttons)
            
            tkfocus(testDialog)
        }
        
        # create qual
        createqualWiz <- function(){
            # function
            create <- function(){
                if(tclvalue(name)==""){
                    tkmessageBox(message="Please enter a name!", type="ok")
                    tkfocus(createqualWiz)
                }
                else if(tclvalue(desc)==""){
                    tkmessageBox(message="Please enter a description!", type="ok")
                    tkfocus(createqualWiz)
                }
                else if(tclvalue(keywords)==""){
                    tkmessageBox(message="Please enter a name!", type="ok")
                    tkfocus(createqualWiz)
                }
                else if(tclvalue(auto)=="1" && tclvalue(auto.value)==""){
                    tkmessageBox(message="If auto-granted, please enter an automatic value!", type="ok")
                    tkfocus(updatequalWiz)
                }
                else {
                    if(is.null(wizardenv$test)){
                        test <- NULL
                        answerkey <- NULL
                        test.duration <- NULL
                    }
                    if(tclvalue(keywords)=="")
                        keywords <- NULL
                    else 
                        keywords <- tclvalue(keywords)
                    if(!is.null(wizardenv$test)){
                        auto <- NULL
                        auto.value <- NULL
                    }
                    else {
                        auto <- tclvalue(auto)
                        auto.value <- tclvalue(auto.value)
                    }
                    if(tclvalue(days)=="" && tclvalue(hours)=="" && tclvalue(mins)=="" && tclvalue(secs)=="")
                        delay <- NULL
                    else
                        delay <- seconds(as.numeric(tclvalue(days)),
                                         as.numeric(tclvalue(hours)),
                                         as.numeric(tclvalue(mins)),
                                         as.numeric(tclvalue(secs)))
                    statselect <- statusopts[as.numeric(as.character(tkcurselection(statuslist)))+1] # listbox index starts at 0
                    results <- CreateQualificationType(name=tclvalue(name), description=tclvalue(desc),
                                                       status=statselect, keywords = keywords,
                                                       retry.delay = delay,
                                                       test = test, answerkey = answerkey, test.duration = test.duration,
                                                       auto = auto, auto.value = auto.value,
                                                       verbose = TRUE, sandbox=sandbox)
                    tkdestroy(createqualWiz)
                    tkfocus(wizard)
                }
            }
            
            assign("test",NULL,envir=wizardenv)
            # layout
            createqualDialog <- tktoplevel()
            tkwm.title(createqualDialog, "Create QualificationType")
            entryform <- tkframe(createqualDialog, relief="groove", borderwidth=2)
                name <- tclVar()
                desc <- tclVar()
                keywords <- tclVar()
                auto <- tclVar("0")
                auto.value <- tclVar()
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=11)
                r <- r + 1
                tkgrid(tklabel(entryform, text = "Name for QualificationType: "), row=r, column=2, sticky="e")
                name.entry <- tkentry(entryform, width = 20, textvariable=name)
                tkgrid(name.entry, row=r, column=3, columnspan=8, sticky="w")
                r <- r + 1
                tkgrid(tklabel(entryform, text = "Description: "), row=r, column=2, sticky="e")
                desc.entry <- tkentry(entryform, width = 40, textvariable=desc)
                tkgrid(desc.entry, row=r, column=3, columnspan=8, sticky="w")
                r <- r + 1
                tkgrid(tklabel(entryform, text = "Keywords (comma-separated): "), row=r, column=2, sticky="e")
                keywords.entry <- tkentry(entryform, width = 40, textvariable=keywords)
                tkgrid(keywords.entry, row=r, column=3, columnspan=8, sticky="w")
                r <- r + 1
                tkgrid(tklabel(entryform, text = "Status: "), row=r, column=2, sticky="e")
                statuslist <- tklistbox(entryform, height=2, width=20, selectmode="single", background="white")
                    statusopts <- c("Active","Inactive")
                    tkinsert(statuslist,"end", statusopts[1])
                    tkinsert(statuslist,"end", statusopts[2])
                    tkselection.set(statuslist,0)
                tkgrid(statuslist, row=r, column=3, columnspan=3, sticky="w")
                r <- r + 2
                tkgrid(tklabel(entryform, text = "Granted automatically upon request? "), row=r, column=2, sticky="e")
                auto.entry <- tkcheckbutton(entryform, variable=auto)
                tkgrid(auto.entry, row=r, column=3, columnspan=3, sticky="w")
                r <- r + 1
                tkgrid(tklabel(entryform, text = "Automatic Value (if granted automatically; optional): "), row=r, column=2, sticky="e")
                value.entry <- tkentry(entryform, width = 15, textvariable=auto.value)
                tkgrid(value.entry, row=r, column=3, columnspan=3, sticky="w")
                r <- r + 1
                tkgrid(tklabel(entryform, text = "How long should workers have to wait to retry?"), row=r, column=2, sticky="e")
                tkgrid(tklabel(entryform, text = "(Leave all blank to disable retries.)"), row=r+1, column=2, sticky="e")
                days <- tclVar("")
                hours <- tclVar("")
                mins <- tclVar("")
                secs <- tclVar("")
                days.entry <- tkentry(entryform, width = 5, textvariable=days)
                hours.entry <- tkentry(entryform, width = 5, textvariable=hours)
                mins.entry <- tkentry(entryform, width = 5, textvariable=mins)
                secs.entry <- tkentry(entryform, width = 5, textvariable=secs)
                tkgrid(tklabel(entryform, text = "Days: "), row=r, column=3, rowspan=2)
                tkgrid(days.entry, row=r, column=4, rowspan=2)
                tkgrid(tklabel(entryform, text = "Hours: "), row=r, column=5, rowspan=2)
                tkgrid(hours.entry, row=r, column=6, rowspan=2)
                tkgrid(tklabel(entryform, text = "Minutes: "), row=r, column=7, rowspan=2)
                tkgrid(mins.entry, row=r, column=8, rowspan=2)
                tkgrid(tklabel(entryform, text = "Seconds: "), row=r, column=9, rowspan=2)
                tkgrid(secs.entry, row=r, column=10, rowspan=2)
                r <- r + 2
                tkgrid(tklabel(entryform, text = "Add Qualification Test (optional; instead of automatic value): "), row=r, column=2, sticky="e")
                testbutton <- tkbutton(entryform,text=" Add Test ",command=qualtest)
                tkgrid(testbutton, row=r, column=3, columnspan=3, sticky="w")
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
            tkgrid(entryform)
            
            # button
            buttons <- tkframe(createqualDialog)
                OKbutton <- tkbutton(buttons,text="   OK   ",command=create)
                Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {tkdestroy(createqualDialog); tkfocus(wizard)})
                r <- 1
                tkgrid(OKbutton, row = r, column = 1)
                tkgrid(Cancelbutton, row=r, column = 2)
            tkgrid(buttons)
            
            tkfocus(createqualDialog)
        }
        
        # update qual
        updatequalWiz <- function(){
            # function
            updateq <- function(){
                if(tclvalue(qualid)==""){
                    tkmessageBox(message="Please enter a QualificationTypeId!", type="ok")
                    tkfocus(updatequalWiz)
                }
                else if(tclvalue(auto)=="1" && tclvalue(auto.value)==""){
                    tkmessageBox(message="If auto-granted, please enter an automatic value!", type="ok")
                    tkfocus(updatequalWiz)
                }
                else {
                    if(is.null(wizardenv$test)){
                        test <- NULL
                        answerkey <- NULL
                        test.duration <- NULL
                        auto <- tclvalue(auto)
                        auto.value <- tclvalue(auto.value)
                    }
                    else {
                        test <- wizardenv$test$Test
                        answerkey <- wizardenv$test$AnswerKey
                        test.duration <- wizardenv$test$TestDurationInSeconds
                        auto <- NULL
                        auto.value <- NULL
                    }
                    if(tclvalue(days)=="" && tclvalue(hours)=="" && tclvalue(mins)=="" && tclvalue(secs)=="")
                        delay <- NULL
                    else
                        delay <- seconds(as.numeric(tclvalue(days)),
                                         as.numeric(tclvalue(hours)),
                                         as.numeric(tclvalue(mins)),
                                         as.numeric(tclvalue(secs)))
                    statselect <- statusopts[as.numeric(as.character(tkcurselection(statuslist)))+1] # listbox index starts at 0
                    qual <- UpdateQualificationType(qual=tclvalue(qualid),
                                                    description=tclvalue(desc),
                                                    status=statselect,
                                                    retry.delay=delay,
                                                    test = test, answerkey = answerkey, test.duration = test.duration,
                                                    auto = auto, auto.value = auto.value,
                                                    verbose=FALSE, sandbox=sandbox
                                                    )
                    tkdestroy(updatequalDialog)
                    tkfocus(wizard)
                }
            }
            
            assign("test",NULL,envir=wizardenv)
            # layout
            updatequalDialog <- tktoplevel()
            tkwm.title(updatequalDialog, "Update QualificationType")
            entryform <- tkframe(updatequalDialog, relief="groove", borderwidth=2)
                qualid <- tclVar()
                desc <- tclVar()
                auto <- tclVar("0")
                auto.value <- tclVar()
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=11)
                r <- r + 1
                tkgrid(tklabel(entryform, text = "QualificationTypeId: "), row=r, column=2, sticky="e")
                qualid.entry <- tkentry(entryform, width = 40, textvariable=qualid)
                tkgrid(qualid.entry, row=r, column=3, columnspan=8, sticky="w")
                r <- r + 1
                tkgrid(tklabel(entryform, text = "Description (optional): "), row=r, column=2, sticky="e")
                desc.entry <- tkentry(entryform, width = 40, textvariable=desc)
                tkgrid(desc.entry, row=r, column=3, columnspan=8, sticky="w")
                r <- r + 1
                tkgrid(tklabel(entryform, text = "Status (optional): "), row=r, column=2, sticky="e")
                statuslist <- tklistbox(entryform, height=2, width=20, selectmode="single", background="white")
                    statusopts <- c("Active","Inactive")
                    tkinsert(statuslist,"end", statusopts[1])
                    tkinsert(statuslist,"end", statusopts[2])
                    tkselection.set(statuslist,0)
                tkgrid(statuslist, row=r, column=3, columnspan=3, sticky="w")
                r <- r + 2
                tkgrid(tklabel(entryform, text = "Granted automatically upon request?"), row=r, column=2, sticky="e")
                auto.entry <- tkcheckbutton(entryform, variable=auto)
                tkgrid(auto.entry, row=r, column=3, columnspan=3, sticky="w")
                r <- r + 1
                tkgrid(tklabel(entryform, text = "Automatic Value (if granted automatically; optional): "), row=r, column=2, sticky="e")
                value.entry <- tkentry(entryform, width = 15, textvariable=auto.value)
                tkgrid(value.entry, row=r, column=3, columnspan=3, sticky="w")
                r <- r + 1
                tkgrid(tklabel(entryform, text = "How long should workers have to wait to retry?"), row=r, column=2, sticky="e")
                tkgrid(tklabel(entryform, text = "(Leave all blank to disable retries.)"), row=r+1, column=2, sticky="e")
                days <- tclVar("")
                hours <- tclVar("")
                mins <- tclVar("")
                secs <- tclVar("")
                days.entry <- tkentry(entryform, width = 5, textvariable=days)
                hours.entry <- tkentry(entryform, width = 5, textvariable=hours)
                mins.entry <- tkentry(entryform, width = 5, textvariable=mins)
                secs.entry <- tkentry(entryform, width = 5, textvariable=secs)
                tkgrid(tklabel(entryform, text = "Days: "), row=r, column=3, rowspan=2)
                tkgrid(days.entry, row=r, column=4, rowspan=2)
                tkgrid(tklabel(entryform, text = "Hours: "), row=r, column=5, rowspan=2)
                tkgrid(hours.entry, row=r, column=6, rowspan=2)
                tkgrid(tklabel(entryform, text = "Minutes: "), row=r, column=7, rowspan=2)
                tkgrid(mins.entry, row=r, column=8, rowspan=2)
                tkgrid(tklabel(entryform, text = "Seconds: "), row=r, column=9, rowspan=2)
                tkgrid(secs.entry, row=r, column=10, rowspan=2)
                r <- r + 2
                tkgrid(tklabel(entryform, text = "Add Qualification Test (optional; instead of automatic value): "), row=r, column=2, sticky="e")
                testbutton <- tkbutton(entryform,text=" Add Test ",command=qualtest)
                tkgrid(testbutton, row=r, column=3, columnspan=3, sticky="w")
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
            tkgrid(entryform)
            
            # button
            buttons <- tkframe(updatequalDialog)
                populate <- function(){
                    result <- searchqualsWiz()
                    tclvalue(qualid) <<- wizardenv$qualresult$QualificationTypeId # retrieve qualid from wizardenv environment
                }
                populatebutton <- tkbutton(buttons, text="Search for QualificationTypes", command=populate)
                OKbutton <- tkbutton(buttons,text="   OK   ",command=updateq)
                Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {tkdestroy(updatequalDialog); tkfocus(wizard)})
                r <- 1
                tkgrid(OKbutton, row = r, column = 1)
                tkgrid(Cancelbutton, row=r, column = 2)
            tkgrid(buttons)
            
            tkfocus(updatequalDialog)
        }
        
        # view qual
        getqualWiz <- function(){
            # function
            getqual <- function(){
                if(tclvalue(qualid)==""){
                    tkmessageBox(message="Please enter a QualificationTypeId!", type="ok")
                    tkfocus(getqualDialog)
                }
                else {
                    results <- GetQualificationType(qual=tclvalue(qualid), verbose=FALSE, sandbox=sandbox)
                    tkdestroy(getqualDialog)
                    print(t(results))
                    tkfocus(wizard)
                }
            }
            
            # layout
            getqualDialog <- tktoplevel()
            tkwm.title(getqualDialog, "Get QualificationType")
            entryform <- tkframe(getqualDialog, relief="groove", borderwidth=2)
                qualid <- tclVar()
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=4)
                r <- r + 1
                qual.entry <- tkentry(entryform, width = 50, textvariable=qualid)
                tkgrid(tklabel(entryform, text = "QualificationTypeId: "), row=r, column=2, sticky="e")
                tkgrid(qual.entry, row=r, column=3, sticky="w")
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "))
            tkgrid(entryform)
            
            # buttons
            buttons <- tkframe(getqualDialog)
                populate <- function(){
                    result <- searchqualsWiz()
                    tkdestroy(getqualDialog)
                    tclvalue(qualid) <<- wizardenv$qualresult$QualificationTypeId # retrieve qualid from wizardenv environment
                    results <- GetQualificationType(qual=tclvalue(qualid), verbose=FALSE, sandbox=sandbox)
                    print(t(results),quote=FALSE)
                    tkfocus(wizard)
                }
                populatebutton <- tkbutton(buttons, text="Search for QualificationTypes", command=populate)
                OKbutton <- tkbutton(buttons,text="   OK   ",command=getqual)
                Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {
                    tkdestroy(getqualDialog)
                    tkfocus(wizard)
                    })
                r <- 1
                tkgrid(populatebutton, row = r, column = 1)
                tkgrid(OKbutton, row = r, column = 2)
                tkgrid(Cancelbutton, row=r, column = 3)
            tkgrid(buttons)
            
            tkfocus(getqualDialog)
        }
        
        # dispose qual
        disposequalWiz <- function(){
            # function
            disposequal <- function(){
                if(tclvalue(qualid)==""){
                    tkmessageBox(message="Please enter a QualificationTypeId!", type="ok")
                    tkfocus(disposequalDialog)
                }
                else {
                    exit <- tkmessageBox(message = "Are you sure you want to dispose the Qualification?
                            This will delete all Qualification and Score data.", icon = "question", type = "yesno", default = "no")
                    if(tclvalue(exit)=="yes"){
                        tkdestroy(disposequalDialog)
                        results <- DisposeQualificationType(qual=tclvalue(qualid), verbose=FALSE, sandbox=sandbox)
                    }
                    else{
                        tkfocus(disposequalDialog)
                    }
                    tkfocus(wizard)
                }
            }
            
            # layout
            disposequalDialog <- tktoplevel()
            tkwm.title(disposequalDialog, "Dispose QualificationType")
            entryform <- tkframe(disposequalDialog, relief="groove", borderwidth=2)
                qualid <- tclVar()
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=4)
                r <- r + 1
                qual.entry <- tkentry(entryform, width = 50, textvariable=qualid)
                tkgrid(tklabel(entryform, text = "QualificationTypeId: "), row=r, column=2, sticky="e")
                tkgrid(qual.entry, row=r, column=3, sticky="w")
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "))
            tkgrid(entryform)
            
            # buttons
            buttons <- tkframe(disposequalDialog)
                populate <- function(){
                    result <- searchqualsWiz()
                    tclvalue(qualid) <<- wizardenv$qualresult$QualificationTypeId # retrieve qualid from wizardenv environment
                }
                populatebutton <- tkbutton(buttons, text="Search for QualificationTypes", command=populate)
                OKbutton <- tkbutton(buttons,text="   OK   ",command=disposequal)
                Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {
                    tkdestroy(disposequalDialog)
                    tkfocus(wizard)
                    })
                r <- 1
                tkgrid(populatebutton, row = r, column = 1)
                tkgrid(OKbutton, row = r, column = 2)
                tkgrid(Cancelbutton, row=r, column = 3)
            tkgrid(buttons)
            
            tkfocus(disposequalDialog)
        }
        
        # get workers by qual
        getworkersbyqualWiz <- function(){
            # function
            getquals <- function(){
                if(tclvalue(qualid)==""){
                    tkmessageBox(message="Please enter a QualificationTypeId!", type="ok")
                    tkfocus(getqualsDialog)
                }
                else {
                    tkdestroy(getqualsDialog)
                    results <- GetQualifications(qual=tclvalue(qualid), verbose=TRUE, return.all=TRUE, sandbox=sandbox)
                    print(results)
                    tkfocus(wizard)
                }
                
            }
            # layout
            getqualsDialog <- tktoplevel()
            tkwm.title(getqualsDialog, "Get Qualifications for QualificationType")
            entryform <- tkframe(getqualsDialog, relief="groove", borderwidth=2)
                qualid <- tclVar()
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=4)
                r <- r + 1
                qual.entry <- tkentry(entryform, width = 50, textvariable=qualid)
                tkgrid(tklabel(entryform, text = "QualificationTypeId: "), row=r, column=2, sticky="e")
                tkgrid(qual.entry, row=r, column=3, sticky="w")
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "))
            tkgrid(entryform)
            
            # buttons
            buttons <- tkframe(getqualsDialog)
                populate <- function(){
                    result <- searchqualsWiz()
                    tclvalue(qualid) <<- wizardenv$qualresult$QualificationTypeId # retrieve qualid from wizardenv environment
                }
                populatebutton <- tkbutton(buttons, text="Search for QualificationTypes", command=populate)
                OKbutton <- tkbutton(buttons,text="   OK   ",command=getquals)
                Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {
                    tkdestroy(getqualsDialog)
                    tkfocus(wizard)
                    })
                r <- 1
                tkgrid(populatebutton, row = r, column = 1)
                tkgrid(OKbutton, row = r, column = 2)
                tkgrid(Cancelbutton, row=r, column = 3)
            tkgrid(buttons)
            
            tkfocus(getqualsDialog)
        }
        
        # get worker score
        getscoreWiz <- function(){
            # function
            getscore <- function(){
                if(tclvalue(qualid)==""){
                    tkmessageBox(message="Please enter a QualificationTypeId!", type="ok")
                    tkfocus(getscoreDialog)
                }
                else if(tclvalue(worker)==""){
                    tkmessageBox(message="Please enter a WorkerId!", type="ok")
                    tkfocus(getscoreDialog)
                }
                else {
                    results <- GetQualificationScore(qual=tclvalue(qualid), workers=tclvalue(worker), verbose=FALSE, sandbox=sandbox)
                    tkdestroy(getscoreDialog)
                    tkmessageBox(message=paste("WorkerId ",results$WorkerId,"'s score: ",results$Value,sep=""), type="ok")
                    tkfocus(wizard)
                }
            }
            
            # layout
            getscoreDialog <- tktoplevel()
            tkwm.title(getscoreDialog, "Get Worker's Qualification Score")
            entryform <- tkframe(getscoreDialog, relief="groove", borderwidth=2)
                qualid <- tclVar()
                worker <- tclVar()
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=4)
                r <- r + 1
                qual.entry <- tkentry(entryform, width = 50, textvariable=qualid)
                tkgrid(tklabel(entryform, text = "QualificationTypeId: "), row=r, column=2, sticky="e")
                tkgrid(qual.entry, row=r, column=3, sticky="w")
                r <- r + 1
                worker.entry <- tkentry(entryform, width = 50, textvariable=worker)
                tkgrid(tklabel(entryform, text = "WorkerId: "), row=r, column=2, sticky="e")
                tkgrid(worker.entry, row=r, column=3, sticky="w")
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "))
            tkgrid(entryform)
            
            # buttons
            buttons <- tkframe(getscoreDialog)
                populate <- function(){
                    result <- searchqualsWiz()
                    tclvalue(qualid) <<- wizardenv$qualresult$QualificationTypeId # retrieve qualid from wizardenv environment
                }
                populatebutton <- tkbutton(buttons, text="Search for QualificationTypes", command=populate)
                OKbutton <- tkbutton(buttons,text="   OK   ",command=getscore)
                Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {
                    tkdestroy(getscoreDialog)
                    tkfocus(wizard)
                    })
                r <- 1
                tkgrid(populatebutton, row = r, column = 1)
                tkgrid(OKbutton, row = r, column = 2)
                tkgrid(Cancelbutton, row=r, column = 3)
            tkgrid(buttons)
            
            tkfocus(getscoreDialog)
        }
        
        # update worker score
        updatescoreWiz <- function(){
            # function
            updatescore <- function(){
                if(tclvalue(qualid)==""){
                    tkmessageBox(message="Please enter a QualificationTypeId!", type="ok")
                    tkfocus(updatescoreDialog)
                }
                else if(tclvalue(worker)==""){
                    tkmessageBox(message="Please enter a WorkerId!", type="ok")
                    tkfocus(updatescoreDialog)
                }
                else if(tclvalue(score)=="" && tclvalue(increment)==""){
                    tkmessageBox(message="Please enter a new score or an increment!", type="ok")
                    tkfocus(updatescoreDialog)
                }
                else {
                    if(tclvalue(score)=="")
                        score <- NULL
                    if(tclvalue(increment)=="")
                        increment <- NULL
                    results <- UpdateQualificationScore(qual=tclvalue(qualid), workers=tclvalue(worker), values = score, increment = increment,
                                                verbose=FALSE, sandbox=sandbox)
                    if(results$Valid==FALSE){
                        tkmessageBox(message="Update failed for some reason!", type="ok")
                        tkfocus(updatescoreDialog)
                    }
                    else{
                        tkmessageBox(message="Qualification Score Updated!", type="ok")
                        tkdestroy(updatescoreDialog)
                        tkfocus(wizard)
                    }
                }
            }
            # layout
            updatescoreDialog <- tktoplevel()
            tkwm.title(updatescoreDialog, "Update Worker's Qualification Score")
            entryform <- tkframe(updatescoreDialog, relief="groove", borderwidth=2)
                qualid <- tclVar()
                worker <- tclVar()
                score <- tclVar()
                increment <- tclVar()
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=4)
                r <- r + 1
                qual.entry <- tkentry(entryform, width = 50, textvariable=qualid)
                tkgrid(tklabel(entryform, text = "QualificationTypeId: "), row=r, column=2, sticky="e")
                tkgrid(qual.entry, row=r, column=3, sticky="w")
                r <- r + 1
                worker.entry <- tkentry(entryform, width = 50, textvariable=worker)
                tkgrid(tklabel(entryform, text = "WorkerId: "), row=r, column=2, sticky="e")
                tkgrid(worker.entry, row=r, column=3, sticky="w")
                r <- r + 1
                score.entry <- tkentry(entryform, width = 10, textvariable=score)
                tkgrid(tklabel(entryform, text = "Specify new score: "), row=r, column=2, sticky="e")
                tkgrid(score.entry, row=r, column=3, sticky="w")
                r <- r + 1
                increment.entry <- tkentry(entryform, width = 10, textvariable=increment)
                tkgrid(tklabel(entryform, text = "Or an amount to increment current score: "), row=r, column=2, sticky="e")
                tkgrid(increment.entry, row=r, column=3, sticky="w")
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "))
            tkgrid(entryform)
            
            # buttons
            buttons <- tkframe(updatescoreDialog)
                populate <- function(){
                    result <- searchqualsWiz()
                    tclvalue(qualid) <<- wizardenv$qualresult$QualificationTypeId # retrieve qualid from wizardenv environment
                }
                populatebutton <- tkbutton(buttons, text="Search for QualificationTypes", command=populate)
                OKbutton <- tkbutton(buttons,text="   OK   ",command=updatescore)
                Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {
                    tkdestroy(updatescoreDialog)
                    tkfocus(wizard)
                    })
                r <- 1
                tkgrid(populatebutton, row = r, column = 1)
                tkgrid(OKbutton, row = r, column = 2)
                tkgrid(Cancelbutton, row=r, column = 3)
            tkgrid(buttons)
            
            tkfocus(updatescoreDialog)
        }
        
        # search quals
        searchqualsWiz <- function(){
            # function
            searchqual <- function(){
                if(tclvalue(searchquery)==""){
                    searchquery <- NULL
                }
                if(as.character(tclvalue(mine))=="1")
                    mine <- TRUE
                else
                    mine <- FALSE
                if(as.character(tclvalue(requestable))=="1")
                    requestable <- TRUE
                else
                    requestable <- FALSE
                results <- SearchQualificationTypes(query=searchquery,only.mine=mine,only.requestable=requestable,
                                                    verbose=FALSE,sandbox=sandbox)
                if(tclvalue(builtin)=="1"){
                    output <- ListQualificationTypes()
                    for(i in 1:dim(results)[1]){
                        output[dim(output)[1]+1,] <- c(paste("User-Defined:",results$Name[i]),results$QualificationTypeId[i])
                    }
                }
                else{
                    output <- results[,c("Name","QualificationTypeId")]
                    names(output) <- c("Qualification","QualificationTypeId")
                }
                # function
                storequal <- function(){
                    pos <- as.numeric(as.character(tkcurselection(quallist)))+1 # listbox index starts at 0
                    selection <- output[pos,]
                    tkgrab.release(selectqualDialog)
                    tkdestroy(selectqualDialog)
                    tkdestroy(searchqualDialog)
                    assign("qualresult", selection, envir=wizardenv) # store 'qualresult' to wizardenv
                    invisible(selection)
                }
                # layout
                selectqualDialog <- tktoplevel()
                tkwm.title(selectqualDialog, "Current QualificationTypes")
                tkgrab.set(selectqualDialog)
                entryform <- tkframe(selectqualDialog)
                    r <- 1
                    scr <- tkscrollbar(entryform, repeatinterval=5, command=function(...) tkyview(quallist,...))
                    quallist <- tklistbox(    entryform, height=20, width=100, selectmode="single",
                                            yscrollcommand=function(...) tkset(scr,...), background="white")
                    tkgrid(quallist, row=r, column=1, columnspan=2)
                    tkgrid(scr, row=r, column=3, sticky="nsw")
                tkgrid(entryform)
                for (i in 1:dim(output)[1]) {
                    tkinsert(quallist,"end", paste(    output$Qualification[i],
                                                    "    (QualificationTypeId: ", output$QualificationTypeId[i],")",sep=""))
                }
                # buttons
                buttons <- tkframe(selectqualDialog)
                    OKbutton <- tkbutton(buttons,text="   OK   ",command=function() invisible(storequal()))
                    Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {
                        tkgrab.release(selectqualDialog)
                        tkdestroy(selectqualDialog)
                        tkdestroy(searchqualDialog)
                    })
                    r <- 1
                    tkgrid(OKbutton, row = r, column = 1)
                    tkgrid(Cancelbutton, row=r, column = 2)
                tkgrid(buttons)
                tkfocus(selectqualDialog)
                tkwait.window(selectqualDialog)
            }
            # layout
            searchqualDialog <- tktoplevel()
            tkwm.title(searchqualDialog, "Search for QualificationTypes")
            entryform <- tkframe(searchqualDialog, relief="groove", borderwidth=2)
                mine <- tclVar("1")
                requestable <- tclVar("0")
                builtin <- tclVar("1")
                searchquery <- tclVar()
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
                r <- r + 1
                mine.entry <- tkcheckbutton(entryform, variable=mine)
                tkgrid(tklabel(entryform, text = "Only search my QualificationTypes? "), row=r, column=1, columnspan=2, sticky="e")
                tkgrid(mine.entry, row=r, column=3, sticky="w")
                r <- r + 1
                requestable.entry <- tkcheckbutton(entryform, variable=requestable)
                tkgrid(tklabel(entryform, text = "Only search requestable QualificationTypes? "), row=r, column=1, columnspan=2, sticky="e")
                tkgrid(requestable.entry, row=r, column=3, sticky="w")
                r <- r + 1
                builtin.entry <- tkcheckbutton(entryform, variable=builtin)
                tkgrid(tklabel(entryform, text = "Include built-in QualificationTypes? "), row=r, column=1, columnspan=2, sticky="e")
                tkgrid(builtin.entry, row=r, column=3, sticky="w")
                r <- r + 1
                searchquery.entry <- tkentry(entryform, width = 50, textvariable=searchquery)
                tkgrid(tklabel(entryform, text = "Query (optional): "), row=r, column=1, sticky="e")
                tkgrid(searchquery.entry, row=r, column=2, columnspan=2, sticky="w")
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
            tkgrid(entryform)
            # buttons
            buttons <- tkframe(searchqualDialog)
                OKbutton <- tkbutton(buttons,text="   OK   ",command=searchqual)
                Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {
                    tkgrab.release(searchqualDialog)
                    tkdestroy(searchqualDialog)
                })
                r <- 1
                tkgrid(OKbutton, row = r, column = 1)
                tkgrid(Cancelbutton, row=r, column = 2)
            tkgrid(buttons)
            tkfocus(searchqualDialog)
            tkwait.window(searchqualDialog)
        }
        
        
        # get qual requests
        getrequestsWiz <- function(){
            # function to view qual request
            viewrequest <- function(){
                pos <- as.numeric(tkcurselection(qualreqlist))+1
                answer <- wizardenv$qualrequests$Answer[pos]
                if(!is.na(answer))
                    print(as.data.frame.QuestionFormAnswers(xmlParse(answer)))
                else
                    message("No answer data to display")
            }
            # function to approve qual requests
            approverequests <- function(){
                # function
                grant <- function(){
                    if(as.character(tkcurselection(qualreqlist))==""){
                        tkmessageBox(message="Please select a Qualification Request!", type="ok")
                        tkfocus(grantreq)
                    }
                    if(tclvalue(value)==""){
                        tkmessageBox(message="Please enter a value!", type="ok")
                        tkfocus(grantreq)
                    }
                    else{
                        tkdestroy(grantreq)
                        pos <- as.numeric(tkcurselection(qualreqlist))+1
                        qualreqid <- wizardenv$qualrequests$QualificationRequestId[pos]
                        GrantQualifications(qual.requests=qualreqid, values=tclvalue(value), verbose=FALSE, sandbox=sandbox)
                        tkinsert(qualreqlist,tkcurselection(qualreqlist),"Granted")
                        tkdelete(qualreqlist,tkcurselection(qualreqlist))
                        tkfocus(qualreqDialog)
                    }
                }
                # layout
                grantreq <- tktoplevel()
                tkwm.title(grantreq, "Assign Value for Qualification")
                r <- 1
                tkgrid(ttklabel(grantreq, text = "     "), row=r, column=1)
                tkgrid(ttklabel(grantreq, text = "     "), row=r, column=4)
                r <- r + 1
                value <- tclVar()
                value.entry <- tkentry(grantreq, width=10, textvariable=value)
                tkgrid(tklabel(grantreq, text = "Qualification Value: "), row=r, column=2, sticky="e")
                tkgrid(value.entry, row=r, column=3, sticky="w")
                r <- r + 1
                tkgrid(ttklabel(grantreq, text = "     "), row=r, column=1)
                OKbutton <- tkbutton(grantreq,text="   OK   ",command=grant)
                Cancelbutton <- tkbutton(grantreq,text=" Cancel ",command=function() tkdestroy(grantreq) )
                r <- r + 1
                tkgrid(OKbutton, row = r, column = 2)
                tkgrid(Cancelbutton, row=r, column = 3)
            }
            
            # function to reject qual requests
            rejectrequests <- function(){
                # function
                rej <- function(){
                    if(as.character(tkcurselection(qualreqlist))==""){
                        tkmessageBox(message="Please select a Qualification Request!", type="ok")
                        tkfocus(rejreq)
                    }
                    else{
                        if(tclvalue(reason)=="")
                            reason1 <- NULL
                        tkdestroy(rejreq)
                        pos <- as.numeric(tkcurselection(qualreqlist))+1
                        qualreqid <- wizardenv$qualrequests$QualificationRequestId[pos]
                        RejectQualifications(qual.request=qualreqid, reason=reason1, verbose=FALSE, sandbox=sandbox)
                        tkinsert(qualreqlist,tkcurselection(qualreqlist),"Rejected")
                        #tkdelete(qualreqlist,"rows",tkcurselection(qualreqlist))
                        tkdelete(qualreqlist,tkcurselection(qualreqlist))
                        tkfocus(qualreqDialog)
                    }
                }
                # layout
                rejreq <- tktoplevel()
                tkwm.title(rejreq, "Reason for rejection?")
                r <- 1
                tkgrid(ttklabel(rejreq, text = "     "), row=r, column=1)
                tkgrid(ttklabel(rejreq, text = "     "), row=r, column=4)
                r <- r + 1
                reason <- tclVar()
                reason.entry <- tkentry(rejreq, width=50, textvariable=reason)
                tkgrid(tklabel(rejreq, text = "Reason (optional; visible to worker): "), row=r, column=2, sticky="e")
                tkgrid(reason.entry, row=r, column=3, sticky="w")
                r <- r + 1
                tkgrid(ttklabel(rejreq, text = "     "), row=r, column=1)
                OKbutton <- tkbutton(rejreq,text="   OK   ",command=rej)
                Cancelbutton <- tkbutton(rejreq,text=" Cancel ",command=function() tkdestroy(rejreq) )
                r <- r + 1
                tkgrid(OKbutton, row = r, column = 2)
                tkgrid(Cancelbutton, row=r, column = 3)
            }
        
            # layout
            qualreqDialog <- tktoplevel()
            tkwm.title(qualreqDialog, "Qualification Requests")
            entryform <- tkframe(qualreqDialog, relief="groove", borderwidth=2)
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=4)
                r <- r + 1
                scr <- tkscrollbar(entryform, repeatinterval=5, command=function(...) tkyview(qualreqlist,...))
                qualreqlist <- tklistbox(entryform, width=50, height=15, selectmode="single",
                                            yscrollcommand=function(...) tkset(scr,...), background="white")
                tkgrid(qualreqlist,scr, row=r, column=2, columnspan=2)
                tkgrid.configure(scr, sticky="nsw")
                r <- r + 1
                
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
            tkgrid(entryform)
            # buttons
            buttons <- tkframe(qualreqDialog)
                viewbutton <- tkbutton(buttons,text=" View Selected ",command=viewrequest)
                appbutton <- tkbutton(buttons,text=" Grant Selected ",command=approverequests)
                rejbutton <- tkbutton(buttons,text=" Reject Selected ",command=rejectrequests)
                Cancelbutton <- tkbutton(buttons,text=" Close ",command=function() tkdestroy(qualreqDialog) )
                r <- 1
                tkgrid(viewbutton, row = r, column = 1)
                tkgrid(appbutton, row = r, column = 2)
                tkgrid(rejbutton, row = r, column = 3)
                tkgrid(Cancelbutton, row=r, column = 4)
            tkgrid(buttons)
            
            # function to populate QualificationRequests
            populate <- function(qualid){
                # function
                setqual <- function(){
                    tkgrab.release(getqualDialog)
                    tkdestroy(getqualDialog)
                    requests <- GetQualificationRequests(qual=tclvalue(qualid), return.all=TRUE, verbose=FALSE, sandbox=sandbox)
                    for(i in 1:dim(requests)[1]) {
                        tkinsert(qualreqlist,"end",paste("Worker: ", requests$SubjectId[i], "  (",requests$SubmitTime[i],")", sep=""))
                    }
                    assign("qualrequests", requests, envir=wizardenv) # store 'qualrequests' to wizardenv
                    tkfocus(qualreqDialog)
                }
                
                # layout
                getqualDialog <- tktoplevel()
                tkwm.title(getqualDialog, "Set QualificationType")
                entryform <- tkframe(getqualDialog, relief="groove", borderwidth=2)
                    qualid <- tclVar()
                    r <- 1
                    tkgrid(ttklabel(entryform, text = "     "), column=1)
                    tkgrid(ttklabel(entryform, text = "     "), column=4)
                    r <- r + 1
                    qual.entry <- tkentry(entryform, width = 50, textvariable=qualid)
                    tkgrid(tklabel(entryform, text = "QualificationTypeId: "), row=r, column=2, sticky="e")
                    tkgrid(qual.entry, row=r, column=3, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = "     "))
                tkgrid(entryform)
                
                # buttons
                buttons <- tkframe(getqualDialog)
                    searchqs <- function(){
                        result <- searchqualsWiz()
                        tclvalue(qualid) <<- wizardenv$qualresult$QualificationTypeId # retrieve qualid from wizardenv environment
                    }
                    populatebutton <- tkbutton(buttons, text=" Search for QualificationTypes ", command=searchqs)
                    OKbutton <- tkbutton(buttons,text="   OK   ",command=setqual)
                    Cancelbutton <- tkbutton(buttons,text=" Cancel ",command=function() {
                        tkdestroy(getqualDialog)
                        tkdestroy(qualreqDialog)
                        tkfocus(wizard)
                        })
                    r <- 1
                    tkgrid(populatebutton, row = r, column = 1)
                    tkgrid(OKbutton, row = r, column = 2)
                    tkgrid(Cancelbutton, row=r, column = 3)
                tkgrid(buttons)
                
                tkfocus(getqualDialog)
                tkwait.window(getqualDialog)
            }
            
            # populate QualificationRequest list
            populate()
        }
        
        
        # print MTurkR code demos to console
        printdemos <- function(){
        
            # PRINT CODE DEMOS TO CONSOLE
        
        }
        
        
        ##----------------------##
        ##     WIZARD LAYOUT    ##
        ##----------------------##
        
        # wizard layout
        wizard <- tktoplevel()                # create tk object
        tkwm.geometry(wizard, "500x20")        # object dimensions
        tkwm.title(wizard, paste("MTurkR ",packageDescription("MTurkR", fields = "Version")," Wizard",sep=""))    # title
        tkwm.protocol(wizard, "WM_DELETE_WINDOW", exitWiz) # regulate exit
        
        ## WIZARD MENUS ##
        topMenu <- tkmenu(wizard)           # Create a menu
        tkconfigure(wizard, menu = topMenu) # Add it to the 'wizard' window
        # switch focus to wizard
        actions <- tkmenu(topMenu, tearoff = FALSE)
            tkadd(actions, "command", label = "Enter RequesterAPI Credentials", command = credentialsWiz)
            tkadd(actions, "command", label = "Use Sandbox?", command = sandboxWiz)
            tkadd(actions, "separator")
            balance <- tkmenu(actions, tearoff = FALSE)
                # balance submenu
                tkadd(balance, "command", label = "Check Balance", command = balanceWiz)
                tkadd(balance, "command", label = "Check Sufficient Funds", command = fundcheckWiz)
            tkadd(actions, "cascade", label = "Balance", menu = balance)
            rui <- tkmenu(actions, tearoff = FALSE)
                # rui submenu
                tkadd(rui, "command", label = "Worker Page", command = function() OpenWorkerPage() )
                tkadd(rui, "command", label = "HIT Management Page", command = function() OpenManageHITPage() )
                tkadd(rui, "command", label = "Qualifications", command = OpenQualificationPage )
                tkadd(rui, "command", label = "API Reference", command = function() APIReference() )
            tkadd(rui, "command", label = "Available HITs", command = function() ViewAvailableHITs() )
            tkadd(actions, "cascade", label = "Requester UI Pages", menu = rui)
            report <- tkmenu(actions, tearoff = FALSE)
                # requester report submenu
                tkadd(report, "command", label = "Previous One Day", command = function() RequesterReport(period="OneDay"))
                tkadd(report, "command", label = "Previous Seven Days", command = function() RequesterReport(period="SevenDays"))
                tkadd(report, "command", label = "Previous Thirty Days", command = function() RequesterReport(period="ThirtyDays"))
                tkadd(report, "command", label = "Lifetime", command = function() RequesterReport(period="LifeToDate"))
            tkadd(actions, "cascade", label = "Requester Reports", menu = report)
            tkadd(actions, "separator")
            tkadd(actions, "command", label = "Load MTurkR Log Entries", command = loadlogWiz)
            tkadd(actions, "separator")
            tkadd(actions, "command", label = "Quit", command = exitWiz)
        tkadd(topMenu, "cascade", label = "Requester Actions", menu = actions, underline = 0)
        hits <- tkmenu(topMenu, tearoff = FALSE)
            # hits menu
            tkadd(hits, "command", label = "Register HITType", command = registerWiz)
            tkadd(hits, "command", label = "Create HIT", command = createWiz)
            tkadd(hits, "command", label = "Change HITType of HIT", command = changetypeWiz)
            tkadd(hits, "separator")
            tkadd(hits, "command", label = "View HIT", command = gethitWiz)
            tkadd(hits, "command", label = "Check HIT Status", command = statusWiz)
            tkadd(hits, "command", label = "Change HIT Review Status", command = reviewingWiz)
            tkadd(hits, "command", label = "View ReviewResults for HIT", command = reviewresultsWiz)
            tkadd(hits, "separator")
            tkadd(hits, "command", label = "Add Assignments to HIT", command = addassignWiz)
            tkadd(hits, "command", label = "Add Time to HIT", command = extendWiz)
            tkadd(hits, "command", label = "Expire HIT", command = expireWiz)
            tkadd(hits, "command", label = "Dispose HIT", command = disposeWiz)
        tkadd(topMenu, "cascade", label = "HITs", menu = hits, underline = 0)
        assignments <- tkmenu(topMenu, tearoff = FALSE)
            # assignments menu
            getassign <- tkmenu(assignments, tearoff = FALSE)
                # get assignments submenu
                tkadd(getassign, "command", label = "By AssignmentId", command = getassign1Wiz)
                tkadd(getassign, "command", label = "By HITId", command = getassign2Wiz)
                tkadd(getassign, "command", label = "By HITTypeId", command = getassign3Wiz)
                tkadd(assignments, "cascade", label = "Get Assignment(s)", menu = getassign, underline = 0)
            tkadd(assignments, "command", label = "Get ReviewResults for HIT", command = reviewresultsWiz)
            tkadd(assignments, "separator")
            tkadd(assignments, "command", label = "Approve Assignment", command = approveWiz)
                # approve > all
                # approve > single
                # approve > multiple
            tkadd(assignments, "command", label = "Reject Assignment", command = rejectWiz)
                # reject > single
                # reject > multiple
            assignwizard <- tkmenu(assignments, tearoff = FALSE)
                # get assignments submenu
                tkadd(assignwizard, "command", label = "By HITId", command = function() approverejectWiz("hit"))
                tkadd(assignwizard, "command", label = "By HITTypeId", command = function() approverejectWiz("hittype"))
            tkadd(assignments, "cascade", label = "Approve/Reject Wizard", menu = assignwizard, underline = 15)
        tkadd(topMenu, "cascade", label = "Assignments", menu = assignments, underline = 0)
        workers <- tkmenu(topMenu, tearoff = FALSE)
            # workers menu
            contact <- tkmenu(assignments, tearoff = FALSE)
                # contact worker(s) submenu
                tkadd(contact, "command", label = "Same Message to One or More Workers", command = contactWiz)
                tkadd(workers, "cascade", label = "Contact...", menu = contact, underline = 0)
            tkadd(workers, "command", label = "Grant Bonus(es)", command = bonusWiz)
            tkadd(workers, "separator")
            tkadd(workers, "command", label = "Block Worker(s)", command = blockWiz)
            tkadd(workers, "command", label = "Unblock Worker(s)", command = unblockWiz)
            tkadd(workers, "command", label = "Get Blocked Workers", command = getblockWiz)
            tkadd(workers, "separator")
            tkadd(workers, "command", label = "Worker Statistics", command = workerstatWiz)
        tkadd(topMenu, "cascade", label = "Workers", menu = workers, underline = 0)
        qualifications <- tkmenu(topMenu, tearoff = FALSE)
            # qualifications menu
            tkadd(qualifications, "command", label = "Create a QualificationType", command = createqualWiz)
            tkadd(qualifications, "command", label = "Update a QualificationType", command = updatequalWiz)
            tkadd(qualifications, "command", label = "View a QualificationType", command = getqualWiz)
            tkadd(qualifications, "command", label = "Dispose a QualificationType", command = disposequalWiz)
            tkadd(qualifications, "separator")
            tkadd(qualifications, "command", label = "Get Qualification Requests", command = getrequestsWiz)
            #tkadd(qualifications, "command", label = "Approve Qualification Requests", command = approverequestsWiz)
            #tkadd(qualifications, "command", label = "Reject Qualification Requests", command = rejectrequestsWiz)
            tkadd(qualifications, "separator")
            tkadd(qualifications, "command", label = "Get Worker(s) By Qualification", command = getworkersbyqualWiz)
            tkadd(qualifications, "command", label = "Get Worker Score(s)", command = getscoreWiz)
            tkadd(qualifications, "command", label = "Update Worker Score(s)", command = updatescoreWiz)
            tkadd(qualifications, "separator")
            tkadd(qualifications, "command", label = "List Built-In QualificationTypes", command = function() print(ListQualificationTypes()) )
        tkadd(topMenu, "cascade", label = "Qualifications", menu = qualifications, underline = 0)
        helpmenu <- tkmenu(topMenu, tearoff = FALSE)
            # help menu
            tkadd(helpmenu, "command", label = "MTurkR Wizard Documentation", command = function()
                browseURL("https://github.com/leeper/MTurkR/wiki/Wizard-Graphical") )
            tkadd(helpmenu, "command", label = "MTurkR Wiki", command = function()
                browseURL("https://github.com/leeper/MTurkR/wiki") )
            tkadd(helpmenu, "command", label = "MTurkR Code Demos", command = function(){
                tkmessageBox(message="Coming soon!", type="ok")
            })
            tkadd(helpmenu, "separator")
            tkadd(helpmenu, "command", label = "MTurk Worker Site", command = function() browseURL("http://www.mturk.com") )
            tkadd(helpmenu, "command", label = "MTurk Requester Site", command = function() browseURL("http://requester.mturk.com") )
            tkadd(helpmenu, "command", label = "Package Website", command = function() browseURL("http://cran.r-project.org/web/packages/MTurkR/") )
            tkadd(helpmenu, "command", label = "MTurkR Documentation", command = function()
                browseURL("http://cran.r-project.org/web/packages/MTurkR/MTurkR.pdf") )
            tkadd(helpmenu, "separator")
            tkadd(helpmenu, "command", label = "About", command =  function() {
                aboutbox <- tktoplevel()
                tkwm.title(aboutbox, paste("MTurkR Version ", packageDescription("MTurkR", fields = "Version"), sep=""))
                tkgrid(ttklabel(aboutbox, text= "     "), row=1, column=1)
                tkgrid(ttklabel(aboutbox, text= "     "), row=1, column=3)
                tkgrid(ttklabel(aboutbox, text = paste("(C) Thomas J. Leeper 2012-",format(Sys.Date(),"%Y"),sep="")), row=2, column=2)
                tkgrid(ttklabel(aboutbox, text= "     "), row=3, column=2)
                website <- ttklabel(aboutbox, text = "http://www.thomasleeper.com/MTurkR/index.html", foreground="blue")
                tkgrid(website, row=4, column=2)
                tkgrid(ttklabel(aboutbox, text= "     "), row=5, column=2)
                tkgrid(tkbutton(aboutbox, text = "   OK   ", command = function(){tkdestroy(aboutbox); tkfocus(wizard)}), row=6, column=2)
                tkgrid(ttklabel(aboutbox, text= "     "), row=7, column=2)
                tkbind(website, "<ButtonPress>", function() browseURL("http://www.thomasleeper.com/MTurkR/index.html"))
                
                tkfocus(aboutbox)
            })
        tkadd(topMenu, "cascade", label = "Help", menu = helpmenu)
        
        # set `sandbox` value, if not specified
        if(is.null(sandbox))
            sandboxWiz()
        tkfocus(wizard)
    }
}
