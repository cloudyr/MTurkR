MTurkR.Wizard <-
mturkr.wizard <-
function(style="tcltk", sandbox=getOption('MTurkR.sandbox')) {
    if(style=="simple"){
        wizard.simple(graphics=FALSE, sandbox=sandbox)
    } else if(style=="simpleGUI"){
        wizard.simple(graphics=TRUE, sandbox=sandbox)
    } else { # by default, style=="tcltk"
        
        # setup some things for the wizard
        
        ## check for support for tcltk >= 8.5
        if(!as.character(tcl("info", "tclversion")) >= "8.5") {
            stop("The graphical wizard requires tcl version >= 8.5")
        }
        
        ## temporary environment to store variables used across functions
        wizardenv <- new.env()
        wizardenv$sandbox <- tclVar(as.numeric(sandbox))
        wizardenv$qualresult <- list()
        wizardenv$qualresult$QualificationTypeId <- ""
        wizardenv$searchresult <- list()
        wizardenv$searchresult$hitid <- ""
        wizardenv$hittypeid <- tclVar()
        wizardenv$hitid <- tclVar()
        wizardenv$qualid <- tclVar()
        
        ## functions
        wzentry <- function(parent, ...) tkentry(parent, ..., background = "white")
        okcancel <- function(parent, okfun, cancelfun, ...) {
            tkgrid(ttklabel(parent, text = "  "))
            buttons <- tkframe(parent)
                tkgrid(ttkbutton(buttons, text = "   OK   ", command = okfun), row = 1, column = 1)
                tkgrid(ttkbutton(buttons, text = " Cancel ", command = cancelfun), row = 1, column = 2)
            tkgrid(buttons, ...)
            invisible(NULL)
        }
        popbuttons <- function(parent, okfun, cancelfun, poptype = "RegisterHIT", ...) {
            tkgrid(ttklabel(parent, text = "  "))
            buttons <- tkframe(parent)
                if(poptype == "RegisterHIT") {
                    populate <- function(){
                        registerWiz()
                    }
                    tkgrid(ttkbutton(buttons, text="Register New HITType", command=populate), row=1, column=1)
                } else if (poptype == "SearchHIT") {
                    populate <- function(){
                        searchWiz()
                    }
                    tkgrid(ttkbutton(buttons, text="Search for HITs", command=populate), row=1, column=1)
                } else if (poptype == "SearchQual") {
                    populate <- function(){
                        searchqualsWiz()
                    }
                    tkgrid(ttkbutton(buttons, text="Search for QualificationTypes", command=populate), row=1, column=1)
                }
                tkgrid(ttkbutton(buttons, text = "   OK   ", command = okfun), row = 1, column = 2)
                tkgrid(ttkbutton(buttons, text = " Cancel ", command = cancelfun), row =1, column = 3)
            tkgrid(buttons, ...)
            invisible(NULL)
        }            
        
        ## set `sandbox` value, if not specified
        sandboxWiz <- function(){
            sandboxQ <- tkmessageBox(message="Use MTurk Requester Sandbox (for practice)?", type="yesno", icon="question", default="no")
            if(tclvalue(sandboxQ)=="yes")
                tclvalue(wizardenv$sandbox) <- 1
            if(tclvalue(sandboxQ)=="no")
                tclvalue(wizardenv$sandbox) <- 0
            tkfocus(wizard)
        }
        if(is.null(wizardenv$sandbox)) {
            sandboxWiz()
        }
        sboxval <- function() as.logical(as.numeric(tclvalue(wizardenv$sandbox)))
        
        ## exit wizard procedure
        exitWiz <- function() {
            exit <- tkmessageBox(message = "Are you sure you want to exit the wizard?", icon = "question", type = "yesno", default = "yes")
            if(tclvalue(exit)=="yes"){
                tkdestroy(wizard)
                if(Sys.getenv("RSTUDIO") != "1") {
                    bringToTop(-1)
                }
                return(invisible(NULL))
            } else {
                tkfocus(wizard)
            }
        }
        
        ##----------------------##
        ## FUNCTIONS FOR WIZARD ##
        ##----------------------##
        
        # enter credentials
        credentialsWiz <- function() {
            credOK <- function() {
                options(MTurkR.keypair = c(tclvalue(accesskey),tclvalue(secretkey)))
                tkdestroy(credDialog)
                tkfocus(wizard)
            }
            
            credDialog <- tktoplevel()
            tkwm.title(credDialog, "Enter/Confirm MTurk Requester Credentials")
            tkfocus(credDialog)
            if(is.null(getOption('MTurkR.keypair'))){
                accesskey <- tclVar("")
                secretkey <- tclVar("")
            } else {
                accesskey <- tclVar(getOption('MTurkR.keypair', c("",""))[1])
                secretkey <- tclVar(getOption('MTurkR.keypair', c("",""))[2])
            }
            aframe <- ttklabelframe(credDialog, text = "MTurk Access Key ID")
            bframe <- ttklabelframe(credDialog, text = "MTurk Secret Access Key")
            tkgrid(wzentry(aframe, width = 50, textvariable=accesskey))
            tkgrid(wzentry(bframe, width = 50, textvariable=secretkey))
            tkgrid(aframe, row = 1)
            tkgrid(bframe, row = 2)
            okcancel(credDialog, okfun = credOK, cancelfun = function(){tkdestroy(credDialog); tkfocus(wizard)})
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
            tempbal <- AccountBalance(verbose=FALSE, sandbox=sboxval())
            tkmessageBox(title = "Current Account Balance", 
                         message = sprintf("Balance: $%0.2f", as.numeric(tempbal$balance)), type="ok")
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
                                         verbose=FALSE, sandbox = sboxval())
                # report results
                results <- tktoplevel()
                tkwm.title(results, "MTurk Account Balance Check")
                r <- 1
                tkgrid(ttklabel(results, text = paste("Previous Balance: $",round(funds$OldBalance,2), sep = "")), row=r, column=1, sticky="w")
                r <- r + 1
                tkgrid(ttklabel(results, text = paste("Total Cost: $",round(funds$Total,2), sep = "")), row=r, column=1, sticky="w")
                r <- r + 1
                tkgrid(ttklabel(results, text = paste("New Balance: $",round(funds$NewBalance,2), sep = "")), row=r, column=1, sticky="w")
                r <- r + 1
                if(funds$SufficientFunds==TRUE)
                    sufficient <- "Yes"
                else
                    sufficient <- "No"
                tkgrid(ttklabel(results, text = paste("Sufficient Funds? ",sufficient, sep = "")), row=r, column=2, sticky="w")
                r <- r + 1
                OKbutton <- ttkbutton(results, text = "   OK   ", command = function() {tkdestroy(results); tkfocus(wizard)})
                tkgrid(OKbutton, row=r, column=2)
                tkfocus(results)
            }
            
            balDialog <- tktoplevel()
            tkwm.title(balDialog, "MTurk Account Balance Check")
            amt <- tclVar("0")
            assignct <- tclVar("0")
            hitct <- tclVar("1")
            bonusct <- tclVar("0")
            bonusamt <- tclVar("0")
            masters <- tclVar("0") # default to zero
            aframe <- ttklabelframe(balDialog, text = "Payment per assignment (US $)")
                tkgrid(wzentry(aframe, width = 5, textvariable=amt))
            tkgrid(aframe, row = 1, sticky = "w")
            cframe <- ttklabelframe(balDialog, text = "Number of assignments")
                tkgrid(wzentry(cframe, width = 5, textvariable=assignct))
            tkgrid(cframe, row = 2, sticky = "w")
            hframe <- ttklabelframe(balDialog, text = "Number of HITs")
                tkgrid(wzentry(hframe, width = 5, textvariable=hitct))
            tkgrid(hframe, row = 3, sticky = "w")
            bframe <- ttklabelframe(balDialog, text = "Number of bonuses")
                tkgrid(wzentry(bframe, width = 5, textvariable=bonusct))
            tkgrid(bframe, row = 4, sticky = "w")
            baframe <- ttklabelframe(balDialog, text = "Payment per bonus (US $)")
                tkgrid(wzentry(baframe, width = 5, textvariable=bonusamt))
            tkgrid(baframe, row = 5, sticky = "w")
            mframe <- ttklabelframe(balDialog, text = "Use MTurk Masters Workers?")
                tkgrid(ttkcheckbutton(mframe, variable=masters))
            tkgrid(baframe, row = 6, sticky = "w")
            okcancel(balDialog, okfun = function() {tkdestroy(balDialog); checksufficient()}, 
                                cancelfun = function() {tkdestroy(balDialog); tkfocus(wizard)})
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
                tkgrid(ttkbutton(logentry, text="Show API Response XML in Console", command=printxml),row=8, column=1, columnspan=2)
            }
            
            
            # create listbox with entries
            logDialog <- tktoplevel()
            tkwm.title(logDialog, "MTurkR Log Entries")
            listframe <- tkframe(logDialog, relief="groove", borderwidth=2)
                scr <- ttkscrollbar(listframe,  command=function(...) tkyview(loglist,...))
                loglist <- tklistbox(listframe, width=50, height=15, selectmode="single", yscrollcommand=function(...) tkset(scr,...), background="white")
                tkgrid(loglist,scr)
                tkgrid.configure(scr, sticky="nsw")
            tkgrid(listframe, row = 1, column = 1)
            for (i in 1:dim(mturkrlog)[1]) {
                tkinsert(loglist,"end",paste(mturkrlog$Operation[i]," (",mturkrlog$Timestamp[i],")",sep=""))
            }
            buttons <- tkframe(logDialog)
                tkgrid(ttkbutton(buttons, text=" Display entry ", command=function() displaylogentry(n=as.numeric(tkcurselection(loglist))+1)), 
                       row = 1, column=1)
                tkgrid(ttkbutton(buttons, text=" Close ", command=function() tkdestroy(logDialog) ), 
                       row = 1, column=2)
            tkgrid(buttons, row = 2, column = 1)
            tkfocus(logDialog)
        }
        
        # register hittype
        registerWiz <- function(){
            assign("qualreq", NULL, envir=wizardenv) # empty qualification requirements from 'wizardenv'
            # function
            gethit <- function(){
                if(tclvalue(title)==""){
                    tkmessageBox(message="Please enter a title!", type="ok")
                    tkfocus(registerDialog)
                    return(NULL)
                }
                if(tclvalue(description)==""){
                    tkmessageBox(message="Please enter a description!", type="ok")
                    tkfocus(registerDialog)
                    return(NULL)
                } 
                if(tclvalue(reward)==""){
                    tkmessageBox(message="Please enter a reward amount (in US dollars)!", type="ok")
                    tkfocus(registerDialog)
                    return(NULL)
                }
                if(tclvalue(daysd)=="" && tclvalue(hoursd)=="" && tclvalue(minsd)=="" && tclvalue(secsd)==""){
                    tkmessageBox(message="Please enter a duration (that workers have to complete an assignment)!", type="ok")
                    tkfocus(registerDialog)
                    return(NULL)
                } 
                if(tclvalue(keywords)==""){
                    tkmessageBox(message="Please enter some keywords!", type="ok")
                    tkfocus(registerDialog)
                    return(NULL)
                }
                if(tclvalue(daysa)=="" && tclvalue(hoursa)=="" && tclvalue(minsa)=="" && tclvalue(secsa)==""){
                    tkmessageBox(message="Please enter a delay (after which assignments are automatically approved)!", type="ok")
                    tkfocus(registerDialog)
                    return(NULL)
                }
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
                                                sandbox = sboxval(),
                                                verbose = TRUE)
                if(newhittype$Valid==TRUE){
                    tclvalue(wizardenv$hittypeid) <- hittype$HITTypeId
                    tkdestroy(registerDialog)
                    tkfocus(wizard)
                } else {
                    tkmessageBox(message="RegisterHITType() failed for some reason. See console.",type="ok")
                    tkfocus(registerDialog)
                }
            }
            addqualreq <- function(){
                # function
                genqual <- function() {
                    if(tclvalue(wizardenv$qualid)==""){
                        tkmessageBox(message="Please enter a QualificationTypeId!", type="ok")
                        tkfocus(qualreqDialog)
                        return(NULL)
                    }
                    if(is.null(tkcurselection(complist))){
                        tkmessageBox(message="Please enter a comparator!", type="ok")
                        tkfocus(qualreqDialog)
                        return(NULL)
                    }
                    pos <- as.numeric(as.character(tkcurselection(complist)))+1 # listbox index starts at 0
                    selection <- complistitems[pos]
                    if(selection %in% c("Exists","DoesNotExist"))
                        qualvalue <- ""
                    else {
                        if(tclvalue(qualvalue) == ""){
                            tkmessageBox(message="Please enter a value!", type="ok")
                            tkfocus(qualreqDialog)
                            return(NULL)
                        } else {
                            qualvalue <- tclvalue(qualvalue)
                        }
                    }
                    if(tclvalue(required)=="1")
                        required <- TRUE
                    else
                        required <- FALSE
                    req <- GenerateQualificationRequirement(qual=tclvalue(wizardenv$qualid),
                                                            comparator=selection,
                                                            value=qualvalue,
                                                            preview=required,
                                                            qual.number=as.integer(tclvalue(wizardenv$nqualreqs))+1
                                                            )
                    tkdestroy(qualreqDialog)
                    wizardenv$qualreq <- paste(wizardenv$qualreq,req,sep="")
                    tclvalue(wizardenv$nqualreqs) <- as.integer(tclvalue(wizardenv$nqualreqs)) + 1 # increment 'nqualreqs'
                    tkfocus(registerDialog)
                }
                
                qualreqDialog <- tktoplevel()
                tkwm.title(qualreqDialog, "Generate QualificationRequirement")
                qframe <- ttklabelframe(qualreqDialog, text = "QualificationTypeId")
                    wizardenv$qualid <- tclVar()
                    tkgrid(wzentry(qframe, width = 50, textvariable=wizardenv$qualid))
                tkgrid(qframe, row = 1, column = 1, columnspan = 2, sticky = "w")
                rframe <- ttklabelframe(qualreqDialog, text = "Required for HIT Preview?")
                    required <- tclVar("0")
                    tkgrid(ttkcheckbutton(rframe, variable=required), column = 1, row = 1, sticky = "e")
                    tkgrid(ttklabel(rframe, text = "Yes"), column = 2, row = 1, sticky = "w")
                tkgrid(rframe, row = 2, column = 1, columnspan = 2, sticky = "w")
                cframe <- ttklabelframe(qualreqDialog, text = "Score is:")
                    scr <- ttkscrollbar(cframe, repeatinterval=4, command=function(...) tkyview(complist,...))
                    complist <- tklistbox(cframe, height=3, width=20, selectmode="single",
                                          yscrollcommand=function(...) tkset(scr,...), background="white")
                    tkgrid(complist, scr, row=1, column=1, sticky="e")
                    tkgrid.configure(scr, row=1, column=2, sticky="w")
                    complistitems <- c("<","<=",">",">=","==","!=","Exists","DoesNotExist")
                    for (i in 1:length(complistitems)) {
                        tkinsert(complist,"end",complistitems[i])
                    }
                    tkselection.set(complist,0)
                tkgrid(cframe, row = 3, column = 1, sticky = "w")
                vframe <- ttklabelframe(qualreqDialog, text = "Value:")
                    qualvalue <- tclVar()
                    tkgrid(wzentry(vframe, width=20, textvariable=qualvalue), sticky = "w")
                    tkgrid(ttklabel(vframe,text="Required except for:\n'Exists' and 'DoesNotExist'"))
                tkgrid(vframe, row = 3, column = 2, sticky = "w")
                popbuttons(qualreqDialog, okfun = genqual, 
                           cancelfun = function(){tkdestroy(qualreqDialog); tkfocus(wizard)}, 
                           poptype = "SearchQual", column = 1, columnspan = 3)
                tkfocus(qualreqDialog)
            }
            
            wizardenv$qualreq <- NULL            
            registerDialog <- tktoplevel()
            tkwm.title(registerDialog, "Register HITType")
            title <- tclVar()
            description <- tclVar()
            reward <- tclVar()
            keywords <- tclVar()
            autoapprovaldelay <- tclVar()
            regqualreq <- NA
            aframe <- ttklabelframe(registerDialog, text = "Public Title:")
            tkgrid(wzentry(aframe, width = 50, textvariable=title))
            tkgrid(aframe, sticky="w")
            
            bframe <- ttklabelframe(registerDialog, text = "Public Description:")
            tkgrid(wzentry(bframe, width = 50, textvariable=description))
            tkgrid(bframe, sticky="w")
            
            cframe <- ttklabelframe(registerDialog, text = "Public Keywords (comma-separated):")
            tkgrid(wzentry(cframe, width = 50, textvariable=keywords))
            tkgrid(cframe, sticky="w")
            
            dframe <- ttklabelframe(registerDialog, text = "Reward Amount:")
            tkgrid(ttklabel(dframe, text = "$"), column = 1, row = 1, sticky = "e")
            tkgrid(wzentry(dframe, width = 10, textvariable=reward), column = 2, row = 1, sticky = "w")
            tkgrid(dframe, sticky="w")
            
            eframe <- ttklabelframe(registerDialog, text = "Time alloted for worker to complete an assignment: ")
                daysd <- tclVar("0")
                hoursd <- tclVar("0")
                minsd <- tclVar("0")
                secsd <- tclVar("0")
                daysd.entry <- wzentry(eframe, width = 5, textvariable=daysd)
                hoursd.entry <- wzentry(eframe, width = 5, textvariable=hoursd)
                minsd.entry <- wzentry(eframe, width = 5, textvariable=minsd)
                secsd.entry <- wzentry(eframe, width = 5, textvariable=secsd)
                tkgrid(ttklabel(eframe, text = "Days: "), row=1, column=1)
                tkgrid(daysd.entry, row=1, column=2)
                tkgrid(ttklabel(eframe, text = "Hours: "), row=1, column=3)
                tkgrid(hoursd.entry, row=1, column=4)
                tkgrid(ttklabel(eframe, text = "Mins.: "), row=1, column=5)
                tkgrid(minsd.entry, row=1, column=6)
                tkgrid(ttklabel(eframe, text = "Secs.: "), row=1, column=7)
                tkgrid(secsd.entry, row=1, column=8)
            tkgrid(eframe, sticky = "w")
            fframe <- ttklabelframe(registerDialog, text = "Delay before automatically approving assignments:")
                daysa <- tclVar("0")
                hoursa <- tclVar("0")
                minsa <- tclVar("0")
                secsa <- tclVar("0")
                daysd.entry <- wzentry(fframe, width = 5, textvariable=daysa)
                hoursd.entry <- wzentry(fframe, width = 5, textvariable=hoursa)
                minsd.entry <- wzentry(fframe, width = 5, textvariable=minsa)
                secsd.entry <- wzentry(fframe, width = 5, textvariable=secsa)
                tkgrid(ttklabel(fframe, text = "Days: "), row=1, column=1)
                tkgrid(daysd.entry, row=1, column=2)
                tkgrid(ttklabel(fframe, text = "Hours: "), row=1, column=3)
                tkgrid(hoursd.entry, row=1, column=4)
                tkgrid(ttklabel(fframe, text = "Mins.: "), row=1, column=5)
                tkgrid(minsd.entry, row=1, column=6)
                tkgrid(ttklabel(fframe, text = "Secs.: "), row=1, column=7)
                tkgrid(secsd.entry, row=1, column=8)
            tkgrid(fframe, sticky = "w")
            
            gframe <- ttklabelframe(registerDialog, text = "Qualification Requirements: ")
                tkgrid(ttkbutton(gframe, text=" Add ", command=addqualreq), column = 1, row = 1)
                tkgrid(ttkbutton(gframe, text=" View ", command=function() {
                    if(!is.null(wizardenv$qualreq)){
                        qreqsplit <- strsplit(wizardenv$qualreq,"&")[[1]]
                        for(i in 2:length(qreqsplit)){
                            message(strsplit(qreqsplit[i],"=")[[1]][1],": ",strsplit(qreqsplit[i],"=")[[1]][2])
                        }
                    }
                }), column = 2, row = 1)
                tkgrid(ttkbutton(gframe, text=" Clear ", command=function() {
                    wizardenv$qualreq <- NULL
                    tclvalue(wizardenv$nqualreqs) <- 0
                }), column = 3, row = 1)
                wizardenv$nqualreqs <- tclVar(0)
                tkgrid(ttklabel(gframe, text = "Currently:"), row=1, column=4, sticky = "e")
                tkgrid(ttklabel(gframe, textvariable = wizardenv$nqualreqs), row = 1, column = 5, sticky = "w")
            tkgrid(gframe, sticky = "w")
            okcancel(registerDialog, okfun = gethit, cancelfun = function(){tkdestroy(registerDialog); tkfocus(wizard)})
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
                        return(NULL)
                    } else if(tclvalue(height)==""){
                        tkmessageBox(message="Please enter a frame height for the HIT!", type="ok")
                        tkfocus(addqDialog)
                        return(NULL)
                    } else {
                        exquestion <- GenerateExternalQuestion(url=question, frame.height=tclvalue(height))
                        assign("question",exquestion,envir=wizardenv) # assign 'question' to wizardenv
                        tkdestroy(addqDialog)
                        tkfocus(createDialog)
                    }
                }
                
                addqDialog <- tktoplevel()
                tkwm.title(addqDialog, "Add External Question to HIT")
                question <- tclVar()
                height <- tclVar("450")
                aframe <- ttklabelframe(addqDialog, text = "HIT URL (must be https): ")
                tkgrid(wzentry(aframe, width = 50, textvariable=question))
                tkgrid(aframe, sticky = "w")
                bframe <- ttklabelframe(addqDialog, text = "Frame Height (pixels):")
                tkgrid(wzentry(bframe, width = 10, textvariable=height))
                okcancel(addqDialog, okfun = store, cancelfun = function() {tkdestroy(addqDialog); tkfocus(wizard)})
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
                        return(NULL)
                    } else {
                        assign("question",question,envir=wizardenv) # assign 'question' to wizardenv
                        tkdestroy(addqDialog)
                        tkfocus(createDialog)
                    }
                }
                
                addqDialog <- tktoplevel()
                tkwm.title(addqDialog, "Add QuestionForm to HIT")
                aframe <- ttklabelframe(addqDialog, text = "QuestionForm data structure: ")
                question.entry <- tktext(aframe, height = 10, width = 75, background = "white")
                tkgrid(question.entry)
                tkmark.set(question.entry,"insert","0.0")
                tkgrid(aframe, sticky = "w")
                okcancel(addqDialog, okfun = store, cancelfun = function() {tkdestroy(addqDialog); tkfocus(wizard)})
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
                        return(NULL)
                    } else {
                        assign("question",question,envir=wizardenv) # assign 'question' to wizardenv
                        tkdestroy(addqDialog)
                        tkfocus(createDialog)
                    }
                }
                
                addqDialog <- tktoplevel()
                tkwm.title(addqDialog, "Add HTMLQuestion to HIT")
                aframe <- ttklabelframe(addqDialog, text = "HTMLQuestion data structure: ")
                question.entry <- tktext(aframe, height = 10, width = 75, background = "white")
                tkgrid(question.entry)
                tkmark.set(question.entry,"insert","0.0")
                tkgrid(aframe, sticky = "w")
                okcancel(addqDialog, okfun = store, cancelfun = function() {tkdestroy(addqDialog); tkfocus(wizard)})
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
                        return(NULL)
                    } else if(!length(lnames)==length(lvalues)){
                        tkmessageBox(message="Layout Names and Layout Values must have same length!", type="ok")
                        tkfocus(addqDialog)
                        return(NULL)
                    } else {
                        assign("layout",tclvalue(layoutid),envir=wizardenv) # assign 'layout' to wizardenv
                        if(!lnames==""){
                            layoutparameters <- GenerateHITLayoutParameter(lnames,lvalues)
                            assign("layoutparameters",layoutparameters,envir=wizardenv)
                        }
                        tkdestroy(addqDialog)
                        tkfocus(createDialog)
                    }
                }
                
                addqDialog <- tktoplevel()
                tkwm.title(addqDialog, "Add HIT Layout Parameters")
                layoutid <- tclVar()
                layoutnames <- tclVar()
                layoutvalues <- tclVar()
                aframe <- ttklabelframe(addqDialog, text = "HITLayoutId:")
                tkgrid(wzentry(aframe, width = 50, textvariable=layoutid))
                tkgrid(aframe, sticky = "w")    
                bframe <- ttklabelframe(addqDialog, text = "Layout Parameter Names (comma-separated): ")
                tkgrid(wzentry(bframe, width = 50, textvariable=layoutnames))
                tkgrid(bframe, sticky = "w")
                cframe <- ttklabelframe(addqDialog, text = "Layout Parameter Values (comma-separated): ")
                tkgrid(wzentry(cframe, width = 50, textvariable=layoutvalues))
                tkgrid(cframe, sticky = "w")
                tkgrid(ttklabel(addqDialog, text = "     "))
                tkgrid(ttkbutton(addqDialog, text = "Retrieve HITLayout Parameters from Requester Website",
                                command = function() {
                                    if(sboxval())
                                        browseURL("https://requestersandbox.mturk.com/create/projects")
                                    else
                                        browseURL("https://requester.mturk.com/create/projects")
                                    }))
                tkgrid(ttklabel(addqDialog, text = "     "))
                okcancel(addqDialog, okfun = store, cancelfun = function() {tkdestroy(addqDialog); tkfocus(wizard)})
                tkfocus(addqDialog)
            }
            
            # function to add HIT and/or Assignment Review Policies
            addreviewpolicy <- function(){
                assign("hitreviewpolicy", NULL, envir = wizardenv)
                assign("assignreviewpolicy", NULL, envir = wizardenv)
                storepolicy <- function()    {
                    if(tclvalue(questionids) != "") {
                        h <- list()
                        h$QuestionIds <- strsplit(tclvalue(questionids), ",", fixed = TRUE)[[1]]
                        if(tclvalue(threshold) != "")
                            h$QuestionAgreementThreshold <- tclvalue(threshold)
                        if(tclvalue(disregardlevel) != "")
                            h$DisregardAssignmentIfKnownAnswerScoreIsLessThan <- tclvalue(disregardlevel)
                        if(tclvalue(applevel) != "")
                            h$ApproveIfWorkerAgreementScoreIsAtLeast <- tclvalue(applevel)
                        if(tclvalue(rejlevel) != "")
                            h$RejectIfWorkerAgreementScoreIsLessThan <- tclvalue(rejlevel)
                        if(tclvalue(hitrejreason) != "")
                            h$RejectReason <- tclvalue(hitrejreason)
                        if(tclvalue(disregardreject) != "")
                            h$DisregardAssignmentIfRejected <- as.logical(as.numeric(tclvalue(disregardreject)))
                        if(tclvalue(hextlevel) != "") {
                            if(as.numeric(tclvalue(hextlevel)) > 100 | as.numeric(tclvalue(hextlevel)) < 1) {
                                tkmessageBox(message="Score must be between 1 and 100!", type="ok")
                                tkfocus(reviewpolicyDialog)
                                return(NULL)
                            }
                            h$ExtendIfHITAgreementScoreIsLessThan <- tclvalue(hextlevel)
                        }
                        if(tclvalue(hextassign) != "")
                            h$ExtendMaximumAssignments <- tclvalue(hextassign)
                        if(tclvalue(hdays)!="0" | tclvalue(hhours)!="0" | tclvalue(hmins)!="0" | tclvalue(hsecs)!="0") {
                            h$ExtendMinimumTimeInSeconds <- seconds(as.numeric(tclvalue(hdays)),
                                                                    as.numeric(tclvalue(hhours)),
                                                                    as.numeric(tclvalue(hmins)),
                                                                    as.numeric(tclvalue(hsecs)))
                        }
                        hittowrite <- do.call("GenerateHITReviewPolicy", h)
                        assign("hitreviewpolicy", hittowrite, envir = wizardenv)
                        tkmessageBox(message="HIT ReviewPolicy added to HIT.\nReopen dialog to reset or modify.", type="ok")
                    }
                    if(tclvalue(tkget(akentry,"0.0","end")) != "\n") {
                        a <- list()
                        ak <- strsplit(strsplit(tclvalue(tkget(akentry,"0.0","end")), "\n")[[1]], "=")
                        a$AnswerKey <- setNames(lapply(ak, `[`, 2), sapply(ak, `[`, 1))
                        if(tclvalue(applevel) != "") {
                            if(as.numeric(tclvalue(applevel)) > 101 | as.numeric(tclvalue(applevel)) < 0) {
                                tkmessageBox(message="Approval score must be between 0 and 101!", type="ok")
                                tkfocus(reviewpolicyDialog)
                                return(NULL)
                            }
                            a$ApproveIfKnownAnswerScoreIsAtLeast <- tclvalue(applevel)
                        }
                        if(tclvalue(assignappreason) != "")
                            a$ApproveReason <- tclvalue(assignappreason)
                        if(tclvalue(rejlevel) != "") {
                            if(as.numeric(tclvalue(rejlevel)) > 101 | as.numeric(tclvalue(rejlevel)) < 0) {
                                tkmessageBox(message="Rejection score must be between 0 and 101!", type="ok")
                                tkfocus(reviewpolicyDialog)
                                return(NULL)
                            }
                            a$RejectIfKnownAnswerScoreIsLessThan <- tclvalue(rejlevel)
                        }
                        if(tclvalue(assignrejreason) != "")
                            a$RejectReason <- tclvalue(assignrejreason)
                        if(tclvalue(aextlevel) != "") {
                            if(as.numeric(tclvalue(aextlevel)) > 101 | as.numeric(tclvalue(aextlevel)) < 0) {
                                tkmessageBox(message="Extension score must be between 0 and 101!", type="ok")
                                tkfocus(reviewpolicyDialog)
                                return(NULL)
                            }
                            a$ExtendIfKnownAnswerScoreIsLessThan <- tclvalue(aextlevel)
                        }
                        if(tclvalue(aextassign) != "") {
                            if(as.numeric(tclvalue(aextassign)) > 25 | as.numeric(tclvalue(aextassign)) < 2) {
                                tkmessageBox(message="Extension score must be between 2 and 25!", type="ok")
                                tkfocus(reviewpolicyDialog)
                                return(NULL)
                            }
                            a$ExtendMaximumAssignments <- tclvalue(aextassign)
                        }
                        if(tclvalue(adays)!="0" | tclvalue(ahours)!="0" | tclvalue(amins)!="0" | tclvalue(asecs)!="0") {
                            a$ExtendMinimumTimeInSeconds <- seconds(as.numeric(tclvalue(adays)),
                                                                    as.numeric(tclvalue(ahours)),
                                                                    as.numeric(tclvalue(amins)),
                                                                    as.numeric(tclvalue(asecs)))
                        }
                        assigntowrite <- do.call("GenerateAssignmentReviewPolicy", a)
                        assign("assignreviewpolicy", assigntowrite, envir = wizardenv)
                        tkmessageBox(message="Assignment ReviewPolicy added to HIT.\nReopen dialog to reset or modify.", type="ok")
                    }
                    tkdestroy(reviewpolicyDialog)
                    tkfocus(createDialog)
                }
                
                reviewpolicyDialog <- tktoplevel()
                tkwm.title(reviewpolicyDialog, "Add ReviewPolicy")
                
                # HIT ReviewPolicy
                hform <- ttklabelframe(reviewpolicyDialog, 
                                       text = "HIT-level ReviewPolicy (optional):", 
                                       borderwidth = 2)
                    tkgrid(ttklabel(hform, text = "Review based on between-worker agreement\n"), row = 1, column = 1, columnspan = 2, sticky = "w")
                    # QuestionIds
                    qidframe <- ttklabelframe(hform, text = "Question IDs (required)", borderwidth = 2)
                        tkgrid(ttklabel(qidframe, text = "Enter questionIDs, separated by commas:"), row = 1, column = 1, sticky = "w")
                        questionids <- tclVar("")
                        tkgrid(wzentry(qidframe, width = 50, textvariable = questionids), row = 2, column = 1, sticky = "w")
                        # QuestionAgreementThreshold
                        thresholdframe <- tkframe(qidframe, borderwidth = 2)
                            threshold <- tclVar("")
                            tkgrid(ttklabel(thresholdframe, text = "Agreement Threshold:"), row = 1, column = 1, sticky = "w")
                            tkgrid(wzentry(thresholdframe, width = 5, textvariable = threshold), row = 1, column = 2, sticky = "w")
                            tkgrid(ttklabel(thresholdframe, text = "%"), row = 1, column = 3, sticky = "w")
                        tkgrid(thresholdframe, column = 1, row = 3, sticky = "w")
                        # DisregardAssignmentIfKnownAnswerScoreIsLessThan
                        disregardframe <- tkframe(qidframe, borderwidth = 2)
                            disregardlevel <- tclVar("")
                            tkgrid(ttklabel(disregardframe, text = "Disregard Below Agreement Score: "), row = 1, column = 1, sticky = "w")
                            tkgrid(wzentry(disregardframe, width = 5, textvariable = disregardlevel), row = 1, column = 2, sticky = "w")
                        tkgrid(disregardframe, column = 1, row = 4, sticky = "w")
                    tkgrid(qidframe, column = 1, row = 2, columnspan = 2, sticky = "w")
                    # ApproveIfWorkerAgreementScoreIsAtLeast
                    hitappframe <- ttklabelframe(hform, text = "Approve Above Agreement Score", borderwidth = 2)
                        happlevelframe <- ttklabelframe(hitappframe, text = "Score", borderwidth = 2)
                            applevel <- tclVar("")
                            tkgrid(wzentry(happlevelframe, width = 5, textvariable = applevel))
                        tkgrid(happlevelframe, column = 1, row = 1)
                    tkgrid(hitappframe, column = 1, row = 5, columnspan = 2, sticky = "w")
                    # RejectIfWorkerAgreementScoreIsLessThan
                    hitrejframe <- ttklabelframe(hform, text = "Reject Below Agreement Score", borderwidth = 2)
                        rejlevelframe <- ttklabelframe(hitrejframe, text = "Score", borderwidth = 2)
                            rejlevel <- tclVar("")
                            tkgrid(wzentry(rejlevelframe, width = 5, textvariable = rejlevel))
                        tkgrid(rejlevelframe, column = 1, row = 1)
                        # RejectReason
                        hrejframe <- ttklabelframe(hitrejframe, text = "Rejection Reason:", borderwidth = 2)
                            hitrejreason <- tclVar("")
                            tkgrid(wzentry(hrejframe, width = 40, textvariable = hitrejreason))
                        tkgrid(hrejframe, column = 2, row = 1)
                        # DisregardAssignmentIfRejected
                        disregardframe <- tkframe(hitrejframe, borderwidth = 2)
                            disregardreject <- tclVar("0")
                            tkgrid(ttkcheckbutton(disregardframe, variable = disregardreject), row = 1, column = 1, sticky = "w")
                            tkgrid(ttklabel(disregardframe, text = "Disregard Rejected Assignments"), row = 1, column = 2, sticky = "w")
                        tkgrid(disregardframe, column = 1, row = 2, columnspan = 2, sticky = "w")
                    tkgrid(hitrejframe, column = 1, row = 6, columnspan = 2, sticky = "w")
                    # ExtendIfHITAgreementScoreIsLessThan
                    hextframe <- ttklabelframe(hform, text = "Extend Below Agreement Score", borderwidth = 2)
                        hextlevelframe <- ttklabelframe(hextframe, text = "Score", borderwidth = 2)
                            hextlevel <- tclVar("")
                            tkgrid(wzentry(hextlevelframe, width = 5, textvariable = hextlevel))
                        tkgrid(hextlevelframe, column = 1, row = 1, sticky = "w")
                        # ExtendMaximumAssignments
                        hextassignframe <- ttklabelframe(hextframe, text = "Add assignments (min 2)", borderwidth = 2)
                            hextassign <- tclVar("5")
                            tkgrid(wzentry(hextassignframe, width = 5, textvariable = hextassign))
                        tkgrid(hextassignframe, column = 2, row = 1, sticky = "w")
                        # ExtendMinimumTimeInSeconds
                        hextsecsframe <- ttklabelframe(hextframe, text = "Add time", borderwidth = 2)
                            hdays <- tclVar("0")
                            hhours <- tclVar("0")
                            hmins <- tclVar("0")
                            hsecs <- tclVar("0")
                            hdays.entry <- wzentry(hextsecsframe, width = 5, textvariable=hdays)
                            hhours.entry <- wzentry(hextsecsframe, width = 5, textvariable=hhours)
                            hmins.entry <- wzentry(hextsecsframe, width = 5, textvariable=hmins)
                            hsecs.entry <- wzentry(hextsecsframe, width = 5, textvariable=hsecs)
                            tkgrid(ttklabel(hextsecsframe, text = "Days: "), row=1, column=1)
                            tkgrid(hdays.entry, row=1, column=2)
                            tkgrid(ttklabel(hextsecsframe, text = "Hours: "), row=1, column=3)
                            tkgrid(hhours.entry, row=1, column=4)
                            tkgrid(ttklabel(hextsecsframe, text = "Mins.: "), row=1, column=5)
                            tkgrid(hmins.entry, row=1, column=6)
                            tkgrid(ttklabel(hextsecsframe, text = "Secs.: "), row=1, column=7)
                            tkgrid(hsecs.entry, row=1, column=8)
                        tkgrid(hextsecsframe, column = 1, row = 3, columnspan = 2)
                    tkgrid(hextframe, column = 1, row = 7, columnspan = 2, sticky = "w")
                tkgrid(hform, column = 1, row = 1, sticky = "nw")
                tkgrid(ttklabel(reviewpolicyDialog, text = "    "), column = 2, row = 1, sticky = "nw")
                
                # Assignment ReviewPolicy
                aform <- ttklabelframe(reviewpolicyDialog, 
                                       text = "Assignment-level ReviewPolicy (optional):", 
                                       borderwidth = 2)
                    tkgrid(ttklabel(aform, text = "Review based on known answers\n"), row = 1, column = 1, columnspan = 2, sticky = "w")
                    # AnswerKey
                    akframe <- ttklabelframe(aform, text = "AnswerKey (required)", borderwidth = 2)
                        tkgrid(ttklabel(akframe, text = "Enter as 'QuestionID=Value' pairs, one per line:"), row = 1, column = 1, sticky = "w")
                        akentry <- tktext(akframe, height = 4, width = 35, bg = "white")
                        tkmark.set(akentry,"insert","0.0")
                        tkgrid(akentry, row = 2, column = 1)
                    tkgrid(akframe, column = 1, row = 2, columnspan = 2)
                    appframe <- ttklabelframe(aform, text = "Approve Above Minimum KnownAnswer Score", borderwidth = 2)
                        # ApproveIfKnownAnswerScoreIsAtLeast
                        applevelframe <- ttklabelframe(appframe, text = "Score", borderwidth = 2)
                            applevel <- tclVar("")
                            tkgrid(wzentry(applevelframe, width = 5, textvariable = applevel))
                        tkgrid(applevelframe, column = 1, row = 1)
                        # ApproveReason
                        aappframe <- ttklabelframe(appframe, text = "Approval Reason (if applicable):", borderwidth = 2)
                            assignappreason <- tclVar("")
                            tkgrid(wzentry(aappframe, width = 40, textvariable = assignappreason))
                        tkgrid(aappframe, column = 2, row = 1)
                    tkgrid(appframe, column = 1, row = 3, columnspan = 2)
                    rejframe <- ttklabelframe(aform, text = "Reject Below KnownAnswer Score", borderwidth = 2)
                        # RejectIfKnownAnswerScoreIsLessThan
                        rejlevelframe <- ttklabelframe(rejframe, text = "Score", borderwidth = 2)
                            rejlevel <- tclVar("")
                            tkgrid(wzentry(rejlevelframe, width = 5, textvariable = rejlevel))
                        tkgrid(rejlevelframe, column = 1, row = 1)
                        # RejectReason
                        arejframe <- ttklabelframe(rejframe, text = "Rejection Reason (if applicable):", borderwidth = 2)
                            assignrejreason <- tclVar("")
                            tkgrid(wzentry(arejframe, width = 40, textvariable = assignrejreason))
                        tkgrid(arejframe, column = 2, row = 1)
                    tkgrid(rejframe, column = 1, row = 4, columnspan = 2)
                    # ExtendIfKnownAnswerScoreIsLessThan
                    aextframe <- ttklabelframe(aform, text = "Extend Below KnownAnswer Score", borderwidth = 2)
                        aextlevelframe <- ttklabelframe(aextframe, text = "Score", borderwidth = 2)
                            aextlevel <- tclVar("")
                            tkgrid(wzentry(aextlevelframe, width = 5, textvariable = aextlevel))
                        tkgrid(aextlevelframe, column = 1, row = 1, sticky = "w")
                        # ExtendMaximumAssignments
                        aextassignframe <- ttklabelframe(aextframe, text = "Add assignments (min 2)", borderwidth = 2)
                            aextassign <- tclVar("5")
                            tkgrid(wzentry(aextassignframe, width = 5, textvariable = aextassign))
                        tkgrid(aextassignframe, column = 2, row = 1, sticky = "w")
                        # ExtendMinimumTimeInSeconds
                        aextsecsframe <- ttklabelframe(aextframe, text = "Add time", borderwidth = 2)
                            adays <- tclVar("0")
                            ahours <- tclVar("0")
                            amins <- tclVar("0")
                            asecs <- tclVar("0")
                            adays.entry <- wzentry(aextsecsframe, width = 5, textvariable=adays)
                            ahours.entry <- wzentry(aextsecsframe, width = 5, textvariable=ahours)
                            amins.entry <- wzentry(aextsecsframe, width = 5, textvariable=amins)
                            asecs.entry <- wzentry(aextsecsframe, width = 5, textvariable=asecs)
                            tkgrid(ttklabel(aextsecsframe, text = "Days: "), row=1, column=1)
                            tkgrid(adays.entry, row=1, column=2)
                            tkgrid(ttklabel(aextsecsframe, text = "Hours: "), row=1, column=3)
                            tkgrid(ahours.entry, row=1, column=4)
                            tkgrid(ttklabel(aextsecsframe, text = "Mins.: "), row=1, column=5)
                            tkgrid(amins.entry, row=1, column=6)
                            tkgrid(ttklabel(aextsecsframe, text = "Secs.: "), row=1, column=7)
                            tkgrid(asecs.entry, row=1, column=8)
                        tkgrid(aextsecsframe, column = 1, row = 3, columnspan = 2)
                    tkgrid(aextframe, column = 1, row = 5, columnspan = 2)
                tkgrid(aform, column = 3, row = 1, sticky = "nw")
                
                okcancel(reviewpolicyDialog, okfun = storepolicy, 
                         cancelfun = function() {tkdestroy(reviewpolicyDialog); tkfocus(wizard)},
                         columnspan = 2)
                tkfocus(reviewpolicyDialog)
            }
            
            # function to create HIT
            create <- function(){
                if(tclvalue(wizardenv$hittype)==""){
                    tkmessageBox(message="Please enter a HITTypeId!", type="ok")
                    tkfocus(createDialog)
                    return(NULL)
                }
                if(tclvalue(annotate)==""){
                    tkmessageBox(message="Please enter a name (private) for the HIT!", type="ok")
                    tkfocus(createDialog)
                    return(NULL)
                } 
                if(tclvalue(assigns)==""){
                    tkmessageBox(message="Please enter the number of assignments for the HIT!", type="ok")
                    tkfocus(createDialog)
                    return(NULL)
                }
                if(tclvalue(days)=="" && tclvalue(hours)=="" && tclvalue(mins)=="" && tclvalue(secs)==""){
                    tkmessageBox(message="Please enter the amount of time the HIT should be available!", type="ok")
                    tkfocus(createDialog)
                    return(NULL)
                } 
                if(is.null(wizardenv$layoutid) && is.null(wizardenv$question)){
                    tkmessageBox(message="Specify Question or HITLayout Parameters!", type="ok")
                    tkfocus(createDialog)
                    return(NULL)
                } 
                if(is.null(wizardenv$hitreviewpolicy))
                    hitpolicy <- NULL
                else
                    hitpolicy <- wizardenv$hitreviewpolicy
                if(is.null(wizardenv$assignreviewpolicy))
                    assignpolicy <- NULL
                else
                    assignpolicy <- wizardenv$assignreviewpolicy
                newhit <- CreateHIT(hit.type=tclvalue(wizardenv$hittype),
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
                                    sandbox=sboxval()
                                    )
                if(newhit$Valid==TRUE){
                    assign("newHITId", newhit$HITId, envir = wizardenv) # assign newHITId to wizardenv
                    tkdestroy(createDialog)
                    tkfocus(wizard)
                } else{
                    tkmessageBox(message="CreateHIT() failed for some reason. See console.", type="ok")
                    tkfocus(createDialog)
                }
            }
            
            assign("question",NULL,envir=wizardenv) # make value of 'question' NULL, unless changed in dialog
            assign("layoutid",NULL,envir=wizardenv) # make value of 'layoutid' NULL, unless changed in dialog
            assign("layoutparameters",NULL,envir=wizardenv) # make value of 'layoutparameters' NULL, unless changed in dialog
            assign("reviewpolicy",NULL,envir=wizardenv) # make value of 'reviewpolicy' NULL, unless changed in dialog
            # dialog
            createDialog <- tktoplevel()
            tkwm.title(createDialog, "Create HIT")
            wizard$hittypeid <- tclVar()
            annotate <- tclVar()
            assigns <- tclVar()
            aframe <- ttklabelframe(createDialog, text = "HITTypeId:")
            tkgrid(wzentry(aframe, width = 50, textvariable = wizardenv$hittypeid))
            tkgrid(aframe, sticky = "w")
            bframe <- ttklabelframe(createDialog, text = "Enter name for HIT (visible only to you):")
            tkgrid(wzentry(bframe, width = 50, textvariable=annotate))
            tkgrid(bframe, sticky = "w")
            cframe <- ttklabelframe(createDialog, text = "Number of assignments")
            tkgrid(wzentry(cframe, width = 10, textvariable=assigns))
            tkgrid(cframe, sticky = "w")
            dframe <- ttklabelframe(createDialog, text = "How long should HIT remain available?")
                days <- tclVar("0")
                hours <- tclVar("0")
                mins <- tclVar("0")
                secs <- tclVar("0")
                days.entry <- wzentry(dframe, width = 5, textvariable=days)
                hours.entry <- wzentry(dframe, width = 5, textvariable=hours)
                mins.entry <- wzentry(dframe, width = 5, textvariable=mins)
                secs.entry <- wzentry(dframe, width = 5, textvariable=secs)
                tkgrid(ttklabel(dframe, text = "Days: "), row=1, column=1)
                tkgrid(days.entry, row=1, column=2)
                tkgrid(ttklabel(dframe, text = "Hours: "), row=1, column=3)
                tkgrid(hours.entry, row=1, column=4)
                tkgrid(ttklabel(dframe, text = "Mins.: "), row=1, column=5)
                tkgrid(mins.entry, row=1, column=6)
                tkgrid(ttklabel(dframe, text = "Secs.: "), row=1, column=7)
                tkgrid(secs.entry, row=1, column=8)
            tkgrid(dframe, sticky = "w")
            questionframe <- ttklabelframe(createDialog, text = "Specify HIT content using one of the following:")
                externalbutton <- ttkbutton(questionframe, text=" Add ExternalQuestion ", width = 20, command=addexquestion)
                internalbutton <- ttkbutton(questionframe, text=" Add QuestionForm ", width = 20, command=addinquestion)
                htmlbutton <- ttkbutton(questionframe, text=" Add HTMLQuestion ", width = 20, command=addhtmlquestion)
                layoutbutton <- ttkbutton(questionframe, text=" Add HITLayout data ", width = 20, command=addlayout)
                tkgrid(externalbutton, row=1, column=1)
                tkgrid(internalbutton, row=1, column=2)
                tkgrid(htmlbutton, row=2, column=1)
                tkgrid(layoutbutton, row=2, column=2)
            tkgrid(questionframe, sticky = "w")
            eframe <- ttklabelframe(createDialog, text = "ReviewPolicy (optional)")
                tkgrid(ttkbutton(eframe, text="Add ReviewPolicy", command = addreviewpolicy))
            tkgrid(eframe, sticky = "w")
            tkgrid(ttklabel(createDialog, text = "   "))
            popbuttons(createDialog, okfun = create, 
                       cancelfun = function(){tkdestroy(createDialog); tkfocus(wizard)}, 
                       poptype = "RegisterHIT")
            tkfocus(createDialog)
        }
        
        # changehittypeofhit
        changetypeWiz <- function() {
            # function to change HITType
            change <- function(){
                if(tclvalue(wizardenv$hittypeid)==""){
                    tkmessageBox(message="Please enter a new HITTypeId!", type="ok")
                    tkfocus(changeDialog)
                    return(NULL)
                }
                if(tclvalue(oldhittype)=="" && tclvalue(hitid)=="" && tclvalue(annotation)==""){
                    tkmessageBox(message="Please enter a HITType, HITId, or Annotation!", type="ok")
                    tkfocus(changeDialog)
                    return(NULL)
                }
                if(sum(c(!tclvalue(oldhittype)=="", !tclvalue(hitid)=="", !tclvalue(annotation)=="")) > 1){
                    tkmessageBox(message="Only an old HITType OR old HITId OR old Annotation can be specified!", type="ok")
                    tkfocus(changeDialog)
                    return(NULL)
                }
                if(!tclvalue(oldhittype)==""){
                    changed <- ChangeHITType(old.hit.type = tclvalue(oldhittype),
                                             new.hit.type = tclvalue(wizardenv$hittypeid),
                                             verbose = FALSE,
                                             sandbox = sboxval())
                    tkdestroy(changeDialog)
                    tkfocus(wizard)
                } else if(!tclvalue(hitid)==""){
                    h <- strsplit(tclvalue(hitid), ",")[[1]]
                    changed <- ChangeHITType(hit = h,
                                             new.hit.type = tclvalue(wizardenv$hittypeid),
                                             verbose = FALSE,
                                             sandbox = sboxval())
                    tkdestroy(changeDialog)
                    tkfocus(wizard)
                } else if(!tclvalue(annotation)==""){
                    changed <- ChangeHITType(old.annotation = tclvalue(annotation),
                                             new.hit.type = tclvalue(wizardenv$hittypeid),
                                             verbose = FALSE,
                                             sandbox = sboxval())
                    tkdestroy(changeDialog)
                    tkfocus(wizard)
                } 
            }
            
            # dialog
            changeDialog <- tktoplevel()
            tkwm.title(changeDialog, "Change HITType of HIT(s)")
            wizardenv$hittypeid <- tclVar()
            oldhittype <- tclVar()
            hitid <- tclVar()
            annotation <- tclVar()
            aframe <- ttklabelframe(changeDialog, text = "Enter old HITTypeId, HITId(s), or annotation to change:")
                oldhittype.entry <- wzentry(aframe, width = 50, textvariable=oldhittype)
                tkgrid(ttklabel(aframe, text = "Old HITTypeId: "), row=1, column=1, sticky = "e")
                tkgrid(oldhittype.entry, row=1, column=2, sticky="w")
                hit.entry <- wzentry(aframe, width = 50, textvariable=hitid)
                tkgrid(ttklabel(aframe, text = "Old HITId(s) (comma-separated): "), row=2, column=1, sticky = "e")
                tkgrid(hit.entry, row=2, column=2, sticky="w")
                annotation.entry <- wzentry(aframe, width = 50, textvariable=annotation)
                tkgrid(ttklabel(aframe, text = "Annotation/Batch: "), row=3, column=1, sticky = "e")
                tkgrid(annotation.entry, row=3, column=2, sticky="w")
            tkgrid(aframe)
            bframe <- ttklabelframe(changeDialog, text = "Enter New HITTypeId (or Register a HITType, below):")
            tkgrid(wzentry(bframe, width = 50, textvariable=wizardenv$hittypeid))
            tkgrid(bframe, sticky = "w")
            popbuttons(changeDialog, okfun = change, 
                       cancelfun = function(){tkdestroy(changeDialog); tkfocus(wizard)}, 
                       poptype = "RegisterHIT")
            tkfocus(changeDialog)
        }
        
        # gethit
        gethitWiz <- function(){
            # function
            gethit <- function(){
                if(tclvalue(wizardenv$hitid)==""){
                    tkmessageBox(message="Please enter a HITId!", type="ok")
                    tkfocus(gethitDialog)
                    return(NULL)
                }
                tkdestroy(gethitDialog)
                hit <- GetHIT(hit=tclvalue(wizardenv$hitid),verbose=FALSE, sandbox=sboxval())
                #print(t(hit$HITs)[1:18,1])
                
                viewhitDialog <- tktoplevel()
                tkwm.title(viewhitDialog, "HIT Details")
                entryform <- tkframe(viewhitDialog, relief="groove", borderwidth=2)
                    r <- 1
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=3)
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = paste("HITId:",hit$HITs$HITId)), row=r, column=2, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = paste("HITTypeId:",hit$HITs$HITTypeId)), row=r, column=2, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = paste("Title:",hit$HITs$Title)), row=r, column=2, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = paste("Description:",hit$HITs$Description)), row=r, column=2, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = paste("Annotation:",hit$HITs$RequesterAnnotation)), row=r, column=2, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = paste("NumberOfSimilarHITs:",hit$HITs$NumberOfSimilarHITs)), row=r, column=2, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = paste("CreationTime:",hit$HITs$CreationTime)), row=r, column=2, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = paste("HITStatus:",hit$HITs$HITStatus)), row=r, column=2, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = paste("HITReviewStatus:",hit$HITs$HITReviewStatus)), row=r, column=2, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = paste("MaxAssignments:",hit$HITs$MaxAssignments)), row=r, column=2, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = paste("Amount:",hit$HITs$Amount)), row=r, column=2, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = paste("AutoApprovalDelayInSeconds:",hit$HITs$AutoApprovalDelayInSeconds)), row=r, column=2, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = paste("AssignmentDurationInSeconds:",hit$HITs$AssignmentDurationInSeconds)), row=r, column=2, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = paste("Expiration:",hit$HITs$Expiration)), row=r, column=2, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(entryform)
                buttons <- tkframe(viewhitDialog)
                    OKbutton <- ttkbutton(buttons, text="   OK   ", command=function() {tkdestroy(viewhitDialog); tkfocus(wizard)})
                    r <- 1
                    tkgrid(OKbutton, row=r, column=2)
                tkgrid(buttons)
                
                tkfocus(viewhitDialog)
            }
            
            gethitDialog <- tktoplevel()
            tkwm.title(gethitDialog, "View HIT Details")
            aframe <- ttklabelframe(gethitDialog, text = "HITId:")
            wizardenv$hitid <- tclVar()
            tkgrid(wzentry(aframe, width = 50, textvariable=wizardenv$hitid))
            tkgrid(aframe)
            popbuttons(gethitDialog, okfun = gethit, 
                       cancelfun = function(){tkdestroy(gethitDialog); tkfocus(wizard)}, 
                       poptype = "SearchHIT")
            tkfocus(gethitDialog)
        }
        
        # check hit status
        statusWiz <- function(){
            # function
            status <- function(){
                if(tclvalue(wizardenv$hitid)==""){
                    tkmessageBox(message="Please enter a HITId!", type="ok")
                    tkfocus(statusDialog)
                    return(NULL)
                }
                tkdestroy(statusDialog)
                status <- HITStatus(hit=tclvalue(wizardenv$hitid), verbose=FALSE, sandbox=sboxval())
                
                viewhitDialog <- tktoplevel()
                tkwm.title(viewhitDialog, "HIT Status")
                entryform <- tkframe(viewhitDialog, relief="groove", borderwidth=2)
                    r <- 1
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=3)
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = paste("HITId:",status$HITId)), row=r, column=2, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = paste("Annotation:",status$RequesterAnnotation)), row=r, column=2, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = paste("HITStatus:",status$HITStatus)), row=r, column=2, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = paste("HITReviewStatus:",status$HITReviewStatus)), row=r, column=2, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = paste("NumberofAssignmentsPending:",status$NumberofAssignmentsPending)), row=r, column=2, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = paste("NumberofAssignmentsAvailable:",status$NumberofAssignmentsAvailable)), row=r, column=2, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = paste("NumberofAssignmentsCompleted:",status$NumberofAssignmentsCompleted)), row=r, column=2, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = paste("Expiration:",status$Expiration)), row=r, column=2, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(entryform)
                buttons <- tkframe(viewhitDialog)
                    OKbutton <- ttkbutton(buttons, text="   OK   ", command=function() {tkdestroy(viewhitDialog); tkfocus(wizard)})
                    r <- 1
                    tkgrid(OKbutton, row=r, column=2)
                tkgrid(buttons)
                
                tkfocus(viewhitDialog)
            }
            
            statusDialog <- tktoplevel()
            tkwm.title(statusDialog, "Get Status of HIT")
            aframe <- ttklabelframe(statusDialog, text = "HITId:")
            wizardenv$hitid <- tclVar()
            tkgrid(wzentry(aframe, width = 50, textvariable=wizardenv$hitid))
            tkgrid(aframe)
            popbuttons(statusDialog, okfun = status, 
                       cancelfun = function(){tkdestroy(statusDialog); tkfocus(wizard)}, 
                       poptype = "SearchHIT")
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
                if(tclvalue(wizardenv$hitid)==""){
                    tkmessageBox(message="Please enter a HITId!", type="ok")
                    tkfocus(resultsDialog)
                    return(NULL)
                }
                tkdestroy(resultsDialog)
                GetReviewResultsForHIT(hit=tclvalue(wizardenv$hitid),
                                       assignment=assignmentid,
                                       policy.level=selections,
                                       verbose=TRUE, sandbox=sboxval())
                tkfocus(wizard)
            }
            
            resultsDialog <- tktoplevel()
            tkwm.title(resultsDialog, "Get ReviewResults for HIT")
            hitlevel <- tclVar("1")
            assignlevel <- tclVar("1")
            wizardenv$hitid <- tclVar()
            assignmentid <- tclVar()
            aframe <- ttklabelframe(resultsDialog, text = "HITId: ")
            tkgrid(wzentry(aframe, width = 50, textvariable=wizardenv$hitid))
            bframe <- ttklabelframe(resultsDialog, text = "AssignmentId: ")
            tkgrid(wzentry(bframe, width = 50, textvariable=assignmentid))
            cframe <- ttklabelframe(resultsDialog, text = "Policy Level(s): ")
            tkgrid(ttklabel(cframe, text = "HIT: "), row=1, column=1, sticky="w")
            tkgrid(ttkcheckbutton(cframe, variable=hitlevel), row = 1, column = 2, sticky = "w")
            tkgrid(ttklabel(cframe, text = "Assignment: "), row=2, column=1, sticky="w")
            tkgrid(ttkcheckbutton(cframe, variable=assignlevel), row = 2, column = 2, sticky = "w")
            tkgrid(aframe)
            tkgrid(bframe)
            tkgrid(cframe, sticky = "w")
            popbuttons(resultsDialog, okfun = getreview, 
                       cancelfun = function(){tkdestroy(resultsDialog); tkfocus(wizard)}, 
                       poptype = "SearchHIT")
            
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
                if(tclvalue(wizardenv$hitid)==""){
                    tkmessageBox(message="Please enter a HITId!", type="ok")
                    tkfocus(reviewingDialog)
                    return(NULL)
                }
                tkdestroy(reviewingDialog)
                SetHITAsReviewing(hit = tclvalue(wizardenv$hitid), revert=revert, verbose=TRUE, sandbox=sboxval())
                tkfocus(wizard)
            }
            reviewingDialog <- tktoplevel()
            tkwm.title(reviewingDialog, "Set Status of HIT as Reviewing")
            aframe <- ttklabelframe(reviewingDialog, text = "HITId:")
            wizardenv$hitid <- tclVar()
            revert <- tclVar("0")
            tkgrid(wzentry(aframe, width = 50, textvariable=wizardenv$hitid))
            tkgrid(aframe)
            bframe <- ttklabelframe(reviewingDialog, text = "Revert to Reviewable? ")
            tkgrid(ttkcheckbutton(bframe, variable=revert))
            tkgrid(bframe, sticky = "w")
            popbuttons(reviewingDialog, okfun = chgstatus, 
                       cancelfun = function(){tkdestroy(reviewingDialog); tkfocus(wizard)}, 
                       poptype = "SearchHIT")
            tkfocus(reviewingDialog)
        }
        
        # add assignments to hit
        addassignWiz <- function(){
            # function
            addassign <- function(){
                if(tclvalue(wizardenv$hitid)==""){
                    tkmessageBox(message="Please enter a HITId!", type="ok")
                    tkfocus(addassignDialog)
                } else if(tclvalue(assignments)==""){
                    tkmessageBox(message="Please enter a number of additional assignments!", type="ok")
                    tkfocus(addassignDialog)
                } else {
                    tkdestroy(addassignDialog)
                    ExtendHIT(hit=tclvalue(wizardenv$hitid),add.assignments=tclvalue(assignments), sandbox=sboxval())
                    tkfocus(wizard)
                }
            }
            
            addassignDialog <- tktoplevel()
            tkwm.title(addassignDialog, "Add Assignments to HIT")
            aframe <- ttklabelframe(addassignDialog, text = "HITId:")
            wizardenv$hitid <- tclVar()
            assignments <- tclVar()
            tkgrid(wzentry(aframe, width = 50, textvariable=wizardenv$hitid))
            tkgrid(aframe)
            bframe <- ttklabelframe(addassignDialog, text = "Number of Assignments to Add:")
            tkgrid(wzentry(bframe, width = 20, textvariable=assignments))
            tkgrid(bframe, sticky = "w")
            popbuttons(addassignDialog, okfun = addassign, 
                       cancelfun = function(){tkdestroy(addassignDialog); tkfocus(wizard)}, 
                       poptype = "SearchHIT")
            tkfocus(addassignDialog)
        }
        
        # add time to hit
        extendWiz <- function(){
            # function
            extend <- function(){
                if(tclvalue(wizardenv$hitid)==""){
                    tkmessageBox(message="Please enter a HITId!", type="ok")
                    tkfocus(extendDialog)
                } else if(tclvalue(days)=="" && tclvalue(hours)=="" && tclvalue(mins)=="" && tclvalue(secs)==""){
                    tkmessageBox(message="Please enter an amount of time!", type="ok")
                    tkfocus(extendDialog)
                } else {
                    tkdestroy(extendDialog)
                    ExtendHIT(hit=tclvalue(wizardenv$hitid),
                              add.seconds=seconds(as.numeric(tclvalue(days)), 
                                                  as.numeric(tclvalue(hours)), 
                                                  as.numeric(tclvalue(mins)), 
                                                  as.numeric(tclvalue(secs))),
                              sandbox=sboxval()
                            )
                    tkfocus(wizard)
                }
            }
            
            extendDialog <- tktoplevel()
            tkwm.title(extendDialog, "Extend HIT")
            entryform <- tkframe(extendDialog, relief="groove", borderwidth=2)
                aframe <- ttklabelframe(entryform, text = "HITId:")
                wizardenv$hitid <- tclVar()
                tkgrid(wzentry(aframe, width = 50, textvariable = wizardenv$hitid))
                tkgrid(aframe, sticky = "w")
                tkgrid(ttklabel(entryform, text = "     "))
                bframe <- ttklabelframe(entryform, text = "Add time")
                # times
                days <- tclVar("0")
                hours <- tclVar("0")
                mins <- tclVar("0")
                secs <- tclVar("0")
                r <- 1
                days.entry <- wzentry(bframe, width = 5, textvariable=days)
                hours.entry <- wzentry(bframe, width = 5, textvariable=hours)
                mins.entry <- wzentry(bframe, width = 5, textvariable=mins)
                secs.entry <- wzentry(bframe, width = 5, textvariable=secs)
                tkgrid(ttklabel(bframe, text = "Days: "), row=r, column=1)
                tkgrid(days.entry, row=r, column=2)
                tkgrid(ttklabel(bframe, text = "Hours: "), row=r, column=3)
                tkgrid(hours.entry, row=r, column=4)
                tkgrid(ttklabel(bframe, text = "Minutes: "), row=r, column=5)
                tkgrid(mins.entry, row=r, column=6)
                tkgrid(ttklabel(bframe, text = "Seconds: "), row=r, column=7)
                tkgrid(secs.entry, row=r, column=8)
                tkgrid(ttklabel(bframe, text = "     "), row=r+1, column=1)
                tkgrid(bframe, sticky = "w")
            tkgrid(entryform)
            popbuttons(extendDialog, okfun = extend, 
                       cancelfun = function(){tkdestroy(extendDialog); tkfocus(wizard)}, 
                       poptype = "SearchHIT")
            tkfocus(extendDialog)
        }
        
        # expire hit
        expireWiz <- function(){
            # function
            expire <- function(){
                if(tclvalue(wizardenv$hitid)==""){
                    tkmessageBox(message="Please enter a HITId!", type="ok")
                    tkfocus(expireDialog)
                } else{
                    tkdestroy(expireDialog)
                    ExpireHIT(hit=tclvalue(wizardenv$hitid), sandbox=sboxval())
                }
            }
            
            expireDialog <- tktoplevel()
            tkwm.title(expireDialog, "Expire HIT")
            aframe <- ttklabelframe(expireDialog, text = "HITId:")
            wizardenv$hitid <- tclVar()
            tkgrid(wzentry(aframe, width = 50, textvariable=wizardenv$hitid))
            tkgrid(aframe)
            popbuttons(expireDialog, okfun = expire, 
                       cancelfun = function(){tkdestroy(expireDialog); tkfocus(wizard)}, 
                       poptype = "SearchHIT")
            tkfocus(expireDialog)
        }
        
        # dispose hit
        disposeWiz <- function(){
            # function
            dispose <- function(){
                if(tclvalue(wizardenv$hitid)==""){
                    tkmessageBox(message="Please enter a HITId!", type="ok")
                    tkfocus(disposeDialog)
                    return(NULL)
                }
                exit <- tkmessageBox(message = "Are you sure you want to dispose the HIT? This will delete all HIT and Assignment data.",
                                    icon = "question", type = "yesno", default = "no")
                if(tclvalue(exit)=="yes"){
                    tkdestroy(disposeDialog)
                    DisposeHIT(hit=tclvalue(wizardenv$hitid), sandbox=sboxval())
                } else{
                    tkfocus(disposeDialog)
                }
            }
            
            disposeDialog <- tktoplevel()
            tkwm.title(disposeDialog, "Dispose HIT")
            aframe <- ttklabelframe(disposeDialog, text = "HITId:")
            wizardenv$hitid <- tclVar()
            tkgrid(wzentry(aframe, width = 50, textvariable=wizardenv$hitid))
            tkgrid(aframe)
            popbuttons(disposeDialog, okfun = dispose, 
                       cancelfun = function(){tkdestroy(disposeDialog); tkfocus(wizard)}, 
                       poptype = "SearchHIT")
            tkfocus(disposeDialog)
        }
        
        # search HITs
        searchWiz <- function(){
            results <- SearchHITs(verbose=FALSE)
            # populate scrollable listbox
            if(nrow(results)>0) {
                currenthits <- tktoplevel()
                tkgrab.set(currenthits)
                tkwm.title(currenthits, "Available HITs")
                entryform <- ttklabelframe(currenthits, text = "Select a HIT:")
                    scr <- ttkscrollbar(entryform, command=function(...) tkyview(hitlist,...))
                    hitlist <- tklistbox(entryform, height=8, width=100, selectmode="single",
                                         yscrollcommand=function(...) tkset(scr,...), background="white")
                    tkgrid(hitlist, scr, column=1)
                    tkgrid.configure(scr, column=2, sticky="nsw")
                    for (i in 1:dim(results$HITs)[1]) {
                        tkinsert(hitlist,"end",paste(results$HITs$RequesterAnnotation[i]," (HITId: ",results$HITs$HITId[i],")",sep=""))
                    }
                    selecthit <- function(){
                        pos <- as.numeric(as.character(tkcurselection(hitlist)))+1 # listbox index starts at 0
                        selection <- results$HITs[pos,]
                        tkgrab.release(currenthits)
                        tkdestroy(currenthits)
                        tclvalue(wizardenv$hitid) <- selection$HITId
                        tclvalue(wizardenv$hittypeid) <- selection$HITTypeId
                    }
                tkgrid(entryform)
                okcancel(currenthits, okfun = selecthit, cancelfun = function() {tkdestroy(currenthits); tkfocus(wizard)})
                tkfocus(currenthits)
                tkwait.window(currenthits)
            } else {
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
                } else {
                    tkdestroy(getassign1Dialog)
                    if(verbose){
                        results <- GetAssignment(assignment=tclvalue(assignment),verbose=TRUE, sandbox=sboxval())
                        print(results)
                    } else {
                        results <- GetAssignment(assignment=tclvalue(assignment),verbose=FALSE, sandbox=sboxval())
                    }
                    if(save==TRUE)
                        savetofile(results)
                    tkfocus(wizard)
                }
            }
            
            getassign1Dialog <- tktoplevel()
            tkwm.title(getassign1Dialog, "Get Assignment")
            aframe <- ttklabelframe(getassign1Dialog, text = "AssignmentId:")
            assignment <- tclVar()
            tkgrid(wzentry(aframe, width = 50, textvariable=assignment))
            tkgrid(aframe)
            buttons <- tkframe(getassign1Dialog)
                printbutton <- ttkbutton(buttons, text=" Print to Console ", command=function() getassign(TRUE,FALSE) )
                savebutton <- ttkbutton(buttons, text=" Save to File ", command=function() getassign(FALSE,TRUE) )
                Cancelbutton <- ttkbutton(buttons, text=" Cancel ", command=function() {tkdestroy(getassign1Dialog); tkfocus(wizard)})
                r <- 1
                tkgrid(printbutton, row = r, column = 1)
                tkgrid(savebutton, row = r, column = 2)
                tkgrid(Cancelbutton, row=r, column = 3)
            tkgrid(buttons)
            tkfocus(getassign1Dialog)
        }
        
        getassign2Wiz <- function(){    # by HITId
            getassign <- function(verbose,save){
                if(tclvalue(wizardenv$hitid)==""){
                    tkmessageBox(message="Please enter a HITId!", type="ok")
                    tkfocus(getassign2Dialog)
                } else {
                    tkdestroy(getassign2Dialog)
                    if(verbose){
                        results <- GetAssignment(hit=tclvalue(wizardenv$hitid),verbose=TRUE, sandbox=sboxval())
                        print(results)
                    } else {
                        results <- GetAssignment(hit=tclvalue(wizardenv$hitid),verbose=FALSE, sandbox=sboxval())
                    }
                    if(save==TRUE)
                        savetofile(results)
                    tkfocus(wizard)
                }
            }
            
            getassign2Dialog <- tktoplevel()
            tkwm.title(getassign2Dialog, "Get Assignment")
            aframe <- ttklabelframe(getassign2Dialog, text = "HITId:")
            wizardenv$hitid <- tclVar()
            tkgrid(wzentry(aframe, width = 50, textvariable=wizardenv$hitid))
            tkgrid(aframe)
            buttons <- tkframe(getassign2Dialog)
                populatebutton <- ttkbutton(buttons, text="Search for HITs", command=searchWiz)
                printbutton <- ttkbutton(buttons, text=" Print to Console ", command=function() getassign(TRUE,FALSE) )
                savebutton <- ttkbutton(buttons, text=" Save to File ", command=function() getassign(FALSE,TRUE) )
                Cancelbutton <- ttkbutton(buttons,text=" Cancel ",command=function() {tkdestroy(getassign2Dialog); tkfocus(wizard)})
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
                        results <- GetAssignment(hit.type=tclvalue(hittype),verbose=TRUE, sandbox=sboxval())
                        print(results)
                    } else {
                        results <- GetAssignment(hit.type=tclvalue(hittype),verbose=FALSE, sandbox=sboxval())
                    }
                    if(save==TRUE)
                        savetofile(results)
                    
                    tkfocus(wizard)
                }
            }
            
            getassign3Dialog <- tktoplevel()
            tkwm.title(getassign3Dialog, "Get Assignment")
            aframe <- ttklabelframe(getassign3Dialog, text = "HITTypeId:")
            wizardenv$hittypeid <- tclVar()
            tkgrid(wzentry(aframe, width = 50, textvariable=wizardenv$hittypeid))
            tkgrid(aframe)
            buttons <- tkframe(getassign3Dialog)
                populatebutton <- ttkbutton(buttons, text="Search for HITType (by HIT)", command=searchWiz)
                printbutton <- ttkbutton(buttons, text=" Print to Console ", command=function() getassign(TRUE,FALSE) )
                savebutton <- ttkbutton(buttons, text=" Save to File ", command=function() getassign(FALSE,TRUE) )
                Cancelbutton <- ttkbutton(buttons,text=" Cancel ",command=function() {tkdestroy(getassign3Dialog); tkfocus(wizard)})
                r <- 1
                tkgrid(populatebutton, row = r, column = 1)
                tkgrid(printbutton, row = r, column = 2)
                tkgrid(savebutton, row = r, column = 3)
                tkgrid(Cancelbutton, row=r, column = 4)
            tkgrid(buttons)
            tkfocus(getassign3Dialog)
        }
        
        getassign4Wiz <- function(){    # by Annotation
            getassign <- function(verbose, save){
                if(tclvalue(annotation)==""){
                    tkmessageBox(message="Please enter an Annotation value!", type="ok")
                    tkfocus(getassign4Dialog)
                } else {
                    tkdestroy(getassign4Dialog)
                    if(verbose){
                        print(GetAssignment(annotation = tclvalue(annotation), verbose=TRUE, sandbox=sboxval()))
                    } else {
                        results <- GetAssignment(annotation = tclvalue(annotation), verbose=FALSE, sandbox=sboxval())
                    }
                    if(save==TRUE)
                        savetofile(results)
                    tkfocus(wizard)
                }
            }
            getassign4Dialog <- tktoplevel()
            tkwm.title(getassign4Dialog, "Get Assignment")
            aframe <- ttklabelframe(getassign4Dialog, text = "Requester Annotation:")
            annotation <- tclVar()
            tkgrid(wzentry(aframe, width = 50, textvariable=annotation))
            tkgrid(aframe)
            buttons <- tkframe(getassign4Dialog)
                printbutton <- ttkbutton(buttons, text=" Print to Console ", command=function() getassign(TRUE,FALSE) )
                savebutton <- ttkbutton(buttons, text=" Save to File ", command=function() getassign(FALSE,TRUE) )
                Cancelbutton <- ttkbutton(buttons,text=" Cancel ",command=function() {tkdestroy(getassign4Dialog); tkfocus(wizard)})
                r <- 1
                tkgrid(printbutton, row = r, column = 2)
                tkgrid(savebutton, row = r, column = 3)
                tkgrid(Cancelbutton, row=r, column = 4)
            tkgrid(buttons)
            tkfocus(getassign4Dialog)
        }
        
        
        # approve assignment(s)
        approveWiz <- function(){
            # approveall function
            approveall <- function(){
                # function
                approveallforhit <- function(){
                    if(tclvalue(wizardenv$hitid)==""){
                        tkmessageBox(message="Please enter a HITId!", type="ok")
                        tkfocus(approveallDialog)
                    }
                    exit <- tkmessageBox(message = "Are you sure you want to approve all assignments for the HIT?",
                                    icon = "question", type = "yesno", default = "no")
                    if(tclvalue(exit)=="yes"){
                        tkdestroy(approveallDialog)
                        ApproveAllAssignments(hit=tclvalue(wizardenv$hitid), sandbox=sboxval())
                        tkfocus(wizard)
                    } else {
                        tkfocus(approveallDialog)
                    }
                }
                tkdestroy(approveDialog)
                approveallDialog <- tktoplevel()
                wizardenv$hitid <- tclVar()
                tkwm.title(approveallDialog, "Approve All Assignments")
                aframe <- ttklabelframe(approveallDialog, text = "HITId: ")
                tkgrid(wzentry(aframe, width = 50, textvariable=wizardenv$hitid))
                tkgrid(aframe)
                popbuttons(approveallDialog, okfun = approveallforhit, 
                           cancelfun = function(){tkdestroy(approveallDialog); tkfocus(wizard)}, 
                           poptype = "SearchHIT")
                tkfocus(approveallDialog)
            }
            # approve1 function
            approve1 <- function(){
                if(tclvalue(feedback)=="") {
                    feedbackvalue <- NULL
                } else if(tclvalue(assignment)==""){
                    tkmessageBox(message="Please enter an AssignmentId!", type="ok")
                    tkfocus(approveDialog)
                } else {
                    ApproveAssignment(assignments=tclvalue(assignment), feedback=feedbackvalue, verbose=TRUE, sandbox=sboxval())
                    tkdestroy(approveDialog)
                    tkfocus(wizard)
                }
            }
            approveDialog <- tktoplevel()
            tkwm.title(approveDialog, "Approve Assignments")
            aframe <- ttklabelframe(approveDialog, text = "AssignmentId: ")
            assignment <- tclVar()
            tkgrid(wzentry(aframe, width = 50, textvariable=assignment))
            bframe <- ttklabelframe(approveDialog, text = "Feedback for worker (optional): ")
            feedback <- tclVar()
            tkgrid(wzentry(bframe, width = 50, textvariable=feedback))
            tkgrid(ttklabel(approveDialog, text = "    "))
            tkgrid(ttkbutton(approveDialog, text="Approve All Assignments for a HIT?", command=approveall))
            tkgrid(ttklabel(approveDialog, text = "    "))
            tkgrid(aframe)
            tkgrid(bframe)
            okcancel(approveDialog, okfun = approve1, cancelfun = function() {tkdestroy(approveDialog); tkfocus(wizard)})
            tkfocus(approveDialog)
        }
        
        # reject assignment(s)
        rejectWiz <- function(){
            
            # function
            reject <- function() {
                if(tclvalue(feedback)=="") {
                    tkmessageBox(message="Please enter a reason for rejection!", type="ok")
                    tkfocus(rejectDialog)
                } else if(tclvalue(assignment)==""){
                    tkmessageBox(message="Please enter an AssignmentId!", type="ok")
                    tkfocus(rejectDialog)
                } else {
                    RejectAssignment(assignments=tclvalue(assignment), feedback=tclvalue(feedback), verbose=TRUE, sandbox=sboxval())
                    tkdestroy(rejectDialog)
                    tkfocus(wizard)
                }
            }
            
            
            rejectDialog <- tktoplevel()
            tkwm.title(rejectDialog, "Reject Assignments")
            aframe <- ttklabelframe(rejectDialog, text = "AssignmentId: ")
            assignment <- tclVar()
            tkgrid(wzentry(aframe, width = 50, textvariable=assignment))
            bframe <- ttklabelframe(rejectDialog, text = "Feedback for worker (required): ")
            feedback <- tclVar()
            tkgrid(wzentry(bframe, width = 50, textvariable=feedback))
            tkgrid(aframe)
            tkgrid(bframe)
            okcancel(rejectDialog, okfun = reject, cancelfun = function() {tkdestroy(rejectDialog); tkfocus(wizard)})
                
            tkfocus(rejectDialog)
        }
   
        
        
        
        # approve/reject assignment dialog
        approverejectWiz <- function(type){
            if(is.null(type))
                invisible(NULL)
            else{
                # function: get assignment data and send to listboxes
                populate <- function(type) {
                    searchWiz()
                    if(type=="hit"){
                        results <- GetAssignments(hit=tclvalue(wizardenv$hitid), 
                                                  return.all=TRUE, verbose=FALSE, sandbox=sboxval())
                    } else if(type=="hittype"){
                        results <- GetAssignments(hit.type=tclvalue(wizardenv$hittypeid), 
                                                  return.all=TRUE, verbose=FALSE, sandbox=sboxval())
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
                    buttons <- tkframe(viewassign)
                        AnswerDetails <- ttkbutton(buttons,text=" View Assignment Answers ",command=printdetails)
                        OKbutton <- ttkbutton(buttons, text="   OK   ",
                                             command=function() {tkdestroy(viewassign); tkfocus(approverejectDialog)})
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
                            ApproveAssignment(assignments=assignid, feedback=reason, verbose=FALSE, sandbox=sboxval())
                            tkinsert(sublist,tkcurselection(sublist),"Approved")
                            tkdelete(sublist,tkcurselection(sublist))
                            tkfocus(approverejectDialog)
                        }
                    }
                    
                    appreason <- tktoplevel()
                    tkwm.title(appreason, "Reason for rejection?")
                    r <- 1
                    tkgrid(ttklabel(appreason, text = "     "), row=r, column=1)
                    tkgrid(ttklabel(appreason, text = "     "), row=r, column=4)
                    r <- r + 1
                    reason <- tclVar()
                    reason.entry <- wzentry(appreason, width=50, textvariable=reason)
                    tkgrid(ttklabel(appreason, text = "Reason (optional; visible to worker): "), row=r, column=2, sticky="e")
                    tkgrid(reason.entry, row=r, column=3, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(appreason, text = "     "), row=r, column=1)
                    OKbutton <- ttkbutton(appreason,text="   OK   ",command=app)
                    Cancelbutton <- ttkbutton(appreason,text=" Cancel ",command=function() tkdestroy(appreason) )
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
                        } else{
                            tkdestroy(rejreason)
                            pos <- as.numeric(tkcurselection(sublist))+1
                            assignid <- wizardenv$submitted$AssignmentId[pos]
                            RejectAssignment(assignments=assignid, feedback=reason, verbose=FALSE, sandbox=sboxval())
                            tkinsert(sublist,tkcurselection(sublist),"Rejected")
                            tkdelete(sublist,tkcurselection(sublist))
                            tkfocus(approverejectDialog)
                        }
                    }
                    
                    rejreason <- tktoplevel()
                    tkwm.title(rejreason, "Reason for approval?")
                    r <- 1
                    tkgrid(ttklabel(rejreason, text = "     "), row=r, column=1)
                    tkgrid(ttklabel(rejreason, text = "     "), row=r, column=4)
                    r <- r + 1
                    reason <- tclVar()
                    reason.entry <- wzentry(rejreason, width=50, textvariable=reason)
                    tkgrid(ttklabel(rejreason, text = "Reason (required; visible to worker): "), row=r, column=2, sticky="e")
                    tkgrid(reason.entry, row=r, column=3, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(rejreason, text = "     "), row=r, column=1)
                    OKbutton <- ttkbutton(rejreason,text="   OK   ",command=rej)
                    Cancelbutton <- ttkbutton(rejreason,text=" Cancel ",command=function() tkdestroy(rejreason) )
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
                        } else{
                            if(tclvalue(reason)=="")
                                reason <- NULL
                            tkdestroy(appreason)
                            pos <- as.numeric(tkcurselection(isrejlist))+1
                            assignid <- wizardenv$rejected$AssignmentId[pos]
                            ApproveAssignment(assignments=assignid, feedback=reason, rejected=TRUE, verbose=FALSE, sandbox=sboxval())
                            tkinsert(isrejlist,tkcurselection(isrejlist),"Approved")
                            tkdelete(isrejlist,tkcurselection(isrejlist))
                            tkfocus(approverejectDialog)
                        }
                    }
                    
                    appreason <- tktoplevel()
                    tkwm.title(appreason, "Reason for approval?")
                    r <- 1
                    tkgrid(ttklabel(appreason, text = "     "), row=r, column=1)
                    tkgrid(ttklabel(appreason, text = "     "), row=r, column=4)
                    r <- r + 1
                    reason <- tclVar()
                    reason.entry <- wzentry(appreason, width=50, textvariable=reason)
                    tkgrid(ttklabel(appreason, text = "Reason (optional; visible to worker): "), row=r, column=2, sticky="e")
                    tkgrid(reason.entry, row=r, column=3, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(appreason, text = "     "), row=r, column=1)
                    OKbutton <- ttkbutton(appreason,text="   OK   ",command=app)
                    Cancelbutton <- ttkbutton(appreason,text=" Cancel ",command=function() tkdestroy(appreason) )
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
                                       reasons = reason, verbose=FALSE, sandbox=sboxval())
                            tkfocus(approverejectDialog)
                        }
                    }
                    
                    bonusreason <- tktoplevel()
                    tkwm.title(bonusreason, "Bonus Amount")
                    r <- 1
                    tkgrid(ttklabel(bonusreason, text = "     "), row=r, column=1)
                    tkgrid(ttklabel(bonusreason, text = "     "), row=r, column=4)
                    r <- r + 1
                    amount <- tclVar()
                    amount.entry <- wzentry(bonusreason, width=50, textvariable=amount)
                    tkgrid(ttklabel(bonusreason, text = "Bonus amount: $"), row=r, column=2, sticky="e")
                    tkgrid(amount.entry, row=r, column=3, sticky="w")
                    r <- r + 1
                    reason <- tclVar()
                    reason.entry <- wzentry(bonusreason, width=50, textvariable=reason)
                    tkgrid(ttklabel(bonusreason, text = "Bonus message (visible to worker): "), row=r, column=2, sticky="e")
                    tkgrid(reason.entry, row=r, column=3, sticky="w")
                    r <- r + 1
                    tkgrid(ttklabel(bonusreason, text = "     "), row=r, column=1)
                    OKbutton <- ttkbutton(bonusreason,text="   OK   ",command=b)
                    Cancelbutton <- ttkbutton(bonusreason,text=" Cancel ",command=function() tkdestroy(bonusreason) )
                    r <- r + 1
                    tkgrid(OKbutton, row = r, column = 2)
                    tkgrid(Cancelbutton, row=r, column = 3)                    
                }
                
                
                approverejectDialog <- tktoplevel()
                tkwm.title(approverejectDialog, "Approve and/or Reject Assignments")
                entryform <- tkframe(approverejectDialog, relief="groove", borderwidth=2)
                    r <- 1
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=11)
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = "Submitted Assignments: "), row=r, column=2, columnspan=2)
                    tkgrid(ttklabel(entryform, text = "Approved Assignments: "), row=r, column=6)
                    tkgrid(ttklabel(entryform, text = "Rejected Assignments: "), row=r, column=9)
                    scr1 <- ttkscrollbar(entryform, command=function(...) tkyview(sublist,...))
                    scr2 <- ttkscrollbar(entryform, command=function(...) tkyview(isapplist,...))
                    scr3 <- ttkscrollbar(entryform, command=function(...) tkyview(isrejlist,...))
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
                    r <- r + 4
                    viewbutton1 <- ttkbutton(entryform,text=" View Assignment ",command=function() view(type="submitted"))
                    tkgrid(viewbutton1, row = r, column = 2, columnspan=3)
                    viewbutton2 <- ttkbutton(entryform,text=" View Assignment ",command=function() view(type="approved"))
                    tkgrid(viewbutton2, row = r, column = 6, columnspan=2)
                    viewbutton3 <- ttkbutton(entryform,text=" View Assignment ",command=function() view(type="rejected"))
                    tkgrid(viewbutton3, row = r, column = 9, columnspan=2)
                    r <- r + 1
                    appbutton <- ttkbutton(entryform,text=" Approve Selected ",command=appfun)
                    tkgrid(appbutton, row = r, column = 2)
                    rejbutton <- ttkbutton(entryform,text=" Reject Selected ",command=rejfun)
                    tkgrid(rejbutton, row = r, column = 3)
                    apprejectedbutton <- ttkbutton(entryform,text=" Re-Approve Selected ",command=apprejected)
                    tkgrid(apprejectedbutton, row = r, column = 9, columnspan=2, rowspan=2)
                    r <- r + 3
                    popbutton <- ttkbutton(entryform,text=" Repopulate Lists ",command=function(){
                            for(i in 0:(length(sublist)-1)){
                                tkdelete(sublist,i)
                            }
                            for(i in 0:(length(isapplist)-1)){
                                tkdelete(isapplist,i)
                            }
                            for(i in 0:(length(isrejlist)-1)){
                                tkdelete(isrejlist,i)
                            }
                            if(type=="hit") {
                                populate("hit")
                            } else if(type=="hit.type")
                                populate("hit.type")
                        } )
                    tkgrid(popbutton, row = r, column = 2, columnspan=3)
                    r <- r + 1
                    tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(entryform)
                populate(type) # fill listboxes with initial assignment statuses
            }
        }
        
        
        # contact worker (single)
        contactWiz <- function(){
            # function
            contact <- function(){
                bodytowrite <- tclvalue(tkget(body.entry,"0.0","end"))
                workers <- strsplit(tclvalue(tkget(worker.entry,"0.0","end")),"[\n]+")[[1]]
                if(tclvalue(emailsubject)==""){
                    tkmessageBox(message="Please enter an email subject!", type="ok")
                    tkfocus(contactDialog)
                    return(NULL)
                } else if(nchar(tclvalue(emailsubject))>200){
                    tkmessageBox(message=paste("Email message body must be less than 200 characters.\nCurrent length is ",
                                        nchar(tclvalue(emailsubject))," characters", sep=""),
                                type="ok")
                    tkfocus(contactDialog)
                    return(NULL)
                } 
                if(nchar(bodytowrite)<=1){
                    tkmessageBox(message="Please enter an email message body!", type="ok")
                    tkfocus(contactDialog)
                    return(NULL)
                } else if(nchar(bodytowrite)>4096){
                    tkmessageBox(message=paste("Email message body must be less than 4096 characters.\nCurrent length is ",
                                        nchar(bodytowrite)," characters", sep=""),
                                type="ok")
                    tkfocus(contactDialog)
                    return(NULL)
                } 
                if(!length(workers) || workers == ""){
                    tkmessageBox(message="Please enter at least one WorkerId!", type="ok")
                    tkfocus(contactDialog)
                    return(NULL)
                }
                workers <- gsub("[[:space:]]", "", workers)
                ContactWorker(subjects = tclvalue(emailsubject),
                              msgs = bodytowrite, 
                              workers = workers,
                              verbose = TRUE, 
                              batch = TRUE, 
                              sandbox = sboxval())
                tkdestroy(contactDialog)
                tkfocus(wizard)
            }
            previewEmail <- function() {
                bodytowrite <- tclvalue(tkget(body.entry,"0.0","end"))
                if(nchar(bodytowrite)<=1){
                    tkmessageBox(message="Please enter an email message body!", type="ok")
                    tkfocus(contactDialog)
                    return(NULL)
                }
                if(tclvalue(emailsubject)==""){
                    tkmessageBox(message="Please enter an email subject!", type="ok")
                    tkfocus(contactDialog)
                    return(NULL)
                }
                bodytowrite <- gsub("\\n","\n", bodytowrite, fixed = TRUE)
                bodytowrite <- gsub("\\t","\t", bodytowrite, fixed = TRUE)
                txtToPrint <- 
                    c("Your message will look like this:\n\n",
                    "Subject:",tclvalue(emailsubject),"\n\nBody:\n\n",
                    "Message from [Your Requester Name]\n",
                    "---------------------------------\n",
                    bodytowrite,
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
            }
            
            contactDialog <- tktoplevel()
            tkwm.title(contactDialog, "Contact MTurk Worker(s) via Email")
            # workerid, emailsubject, msg body
            entryform <- tkframe(contactDialog, relief="groove", borderwidth=2)
                r <- 1
                sframe <- ttklabelframe(entryform,
                                        text = "Email Subject (max 200 char.):", 
                                        borderwidth = 2)
                    emailsubject <- tclVar()
                    subject.entry <- wzentry(sframe, width = 50, textvariable=emailsubject)
                    tkgrid(subject.entry)
                tkgrid(sframe, row = r, sticky = "w")
                r <- r + 1
                bframe <- ttklabelframe(entryform,
                                        text = "Email Body (max 4096 char.):", 
                                        borderwidth = 2)
                    wizardenv$chars <- tclVar('0')
                    body.entry <- tktext(bframe, height = 10, width = 50, background = "white")
                    tkgrid(body.entry, column = 1, columnspan = 3)
                    tkmark.set(body.entry,"insert","0.0")
                    editModified <- function(){
                        tclvalue(wizardenv$chars) <- 
                          as.character(nchar(curl_escape(tclvalue(tkget(body.entry,"0.0","end")))))
                    }
                    tkbind(body.entry, "<Key>", editModified)
                    tkgrid(ttklabel(bframe, text='Number of characters:'), row=2, column=1, sticky='e')
                    tkgrid(ttklabel(bframe, textvariable = wizardenv$chars), row=2, column=2, sticky='w')
                    tkgrid(ttkbutton(bframe, text = "Preview Email", command = previewEmail), row = 2, column = 3)
                tkgrid(bframe, row = r)
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "    "), row = r)
                r <- r + 1
                wframe <- ttklabelframe(entryform,
                                        text = "WorkerId(s) (one per line):", 
                                        borderwidth = 2)
                    worker.entry <- tktext(wframe, height = 10, width = 50, background = "white")
                    tkmark.set(worker.entry,"insert","0.0")
                    tkgrid(worker.entry)
                tkgrid(wframe, row = r)
            tkgrid(entryform)
            okcancel(contactDialog, okfun = contact, cancelfun = function() {tkdestroy(contactDialog); tkfocus(wizard)})
            tkfocus(contactDialog)
        }      
        
        
        # grant bonus(es)
        bonusWiz <- function(){
            # function
            bonus <- function(){
                if(tclvalue(workerid)==""){
                    tkmessageBox(message="Please enter a WorkerId!", type="ok")
                    tkfocus(bonusDialog)
                } else if(tclvalue(assignmentid)==""){
                    tkmessageBox(message="Please enter an AssignmentId!", type="ok")
                    tkfocus(bonusDialog)
                } else if(tclvalue(amount)==""){
                    tkmessageBox(message="Please enter an amount (in US Dollars)!", type="ok")
                    tkfocus(bonusDialog)
                } else if(tclvalue(reason)==""){
                    tkmessageBox(message="Please enter a reason for the bonus!", type="ok")
                    tkfocus(bonusDialog)
                } else {
                    tkdestroy(bonusDialog)
                    GrantBonus( workers=tclvalue(workerid),
                                assignments=tclvalue(assignmentid),
                                amounts=tclvalue(amount),
                                reasons=tclvalue(reason),
                                verbose=TRUE, sandbox=sboxval())
                    tkfocus(wizard)
                }
            }
            
            
            bonusDialog <- tktoplevel()
            tkwm.geometry(bonusDialog)
            tkwm.title(bonusDialog, "Bonus MTurk Worker")
            entryform <- tkframe(bonusDialog, relief="groove", borderwidth=2)
                # workerid, assignmentid, msg body
                workerid <- tclVar()
                assignmentid <- tclVar()
                amount <- tclVar()
                reason <- tclVar()
                wframe <- ttklabelframe(entryform, text = "WorkerId")
                    tkgrid(wzentry(wframe, width = 50, textvariable=workerid))
                tkgrid(wframe, row = 1)
                aframe <- ttklabelframe(entryform, text = "AssignmentId")
                    tkgrid(wzentry(aframe, width = 50, textvariable=assignmentid))
                tkgrid(aframe, row = 2)
                bframe <- ttklabelframe(entryform, text = "Bonus Amount (in US $)")
                    tkgrid(wzentry(bframe, width = 50, textvariable=amount))
                tkgrid(bframe, row = 3)
                rframe <- ttklabelframe(entryform, text = "Reason for bonus")
                    tkgrid(wzentry(rframe, width = 50, textvariable=reason))
                tkgrid(rframe, row = 4)
            tkgrid(entryform)
            okcancel(bonusDialog, okfun = bonus, cancelfun = function() {tkdestroy(bonusDialog); tkfocus(wizard)})
            tkfocus(bonusDialog)
        }
        
        # block worker(s)
        blockWiz <- function(){
            # function
            block <- function(){
                workers <- strsplit(tclvalue(tkget(worker.entry,"0.0","end")),"[\n]+")[[1]]
                if(!length(workers) || workers == ""){
                    tkmessageBox(message="Please enter at least one WorkerId!", type="ok")
                    tkfocus(blockDialog)
                } else if(tclvalue(reason)==""){
                    tkmessageBox(message="Please enter a reason for block!", type="ok")
                    tkfocus(blockDialog)
                } else {
                    tkdestroy(blockDialog)
                    BlockWorker(workers = gsub("[[:space:]]", "", workers), 
                                reasons = tclvalue(reason), 
                                verbose = TRUE, 
                                sandbox = sboxval())
                    tkfocus(wizard)
                }
            }
            
            blockDialog <- tktoplevel()
            tkwm.title(blockDialog, "Block Worker(s)")
            entryform <- tkframe(blockDialog, relief="groove", borderwidth=2)
                wframe <- ttklabelframe(entryform, text = "WorkerId(s) (one per line): ")
                    worker.entry <- tktext(wframe, height = 10, width = 50, background = "white")
                    tkgrid(worker.entry)
                    tkmark.set(worker.entry,"insert","0.0")
                tkgrid(wframe, row = 1)
                rframe <- ttklabelframe(entryform, text = "Reason for block:")
                    reason <- tclVar()
                    tkgrid(wzentry(rframe, width = 50, textvariable=reason))
                tkgrid(rframe, row = 2, sticky = "w")
            tkgrid(entryform)
            okcancel(blockDialog, okfun = block, cancelfun = function() {tkdestroy(blockDialog); tkfocus(wizard)})
            tkfocus(blockDialog)
        }
        
        # unblock worker(s)
        unblockWiz <- function(){
            # function
            unblock <- function(){
                workers <- strsplit(tclvalue(tkget(worker.entry,"0.0","end")),"[\n]+")[[1]]
                if(!length(workers) || workers == ""){
                    tkmessageBox(message="Please enter at least one WorkerId!", type="ok")
                    tkfocus(unblockDialog)
                } else {
                    tkdestroy(unblockDialog)
                    if(tclvalue(reason)=="") {
                        UnblockWorker(workers = gsub("[[:space:]]", "", workers), 
                                      verbose = TRUE, 
                                      sandbox = sboxval())
                    } else {
                        UnblockWorker(workers = gsub("[[:space:]]", "", workers), 
                                      reasons = tclvalue(reason), 
                                      verbose = TRUE, 
                                      sandbox = sboxval())
                    }
                    tkfocus(wizard)
                }
            }
            
            unblockDialog <- tktoplevel()
            tkwm.title(unblockDialog, "Unblock Worker(s)")
            entryform <- tkframe(unblockDialog, relief="groove", borderwidth=2)
                wframe <- ttklabelframe(entryform, text = "WorkerId(s) (one per line): ")
                    worker.entry <- tktext(wframe, height = 10, width = 50, background = "white")
                    tkgrid(worker.entry)
                    tkmark.set(worker.entry,"insert","0.0")
                tkgrid(wframe, row = 1)
                rframe <- ttklabelframe(entryform, text = "Reason for unblock:")
                    reason <- tclVar()
                    tkgrid(wzentry(rframe, width = 50, textvariable=reason))
                tkgrid(rframe, row = 2, sticky = "w")
            tkgrid(entryform)
            okcancel(unblockDialog, okfun = unblock, cancelfun = function() {tkdestroy(unblockDialog); tkfocus(wizard)})
            tkfocus(unblockDialog)
        }
        
        # get blocked worker(s)
        getblockWiz <- function(){
            blockedworkers <- GetBlockedWorkers(verbose=FALSE, sandbox=sboxval())
            # populate scrollable listbox
            if(!is.null(blockedworkers) && is.data.frame(blockedworkers)) {
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
                
                blocklistDialog <-tktoplevel()
                tkwm.title(blocklistDialog, "Currently Blocked Workers")
                entryform <- tkframe(blocklistDialog, relief="groove", borderwidth=2)
                scr <- ttkscrollbar(entryform,  command=function(...) tkyview(workerlist,...))
                workerlist <- tklistbox(entryform, height=8, selectmode="multiple", yscrollcommand=function(...) tkset(scr,...), background="white")
                tkgrid(workerlist,scr)
                tkgrid.configure(scr, rowspan=4, sticky="nsw")
                for (i in 1:dim(blockedworkers)[1]) {
                    tkinsert(workerlist,"end",blockedworkers$WorkerId[i])
                }
                tkgrid(entryform)
                okcancel(blocklistDialog, okfun = selectworkers, cancelfun = function() {tkdestroy(blocklistDialog); tkfocus(wizard)})
                tkfocus(blocklistDialog)
            } else {
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
                    WorkerReport(worker=tclvalue(workerid), period=reportperiod, sandbox=sboxval())
                    tkfocus(wizard)
                }
            }
            
            statDialog <- tktoplevel()
            tkwm.title(statDialog, "MTurk Worker Statistics")
            wframe <- ttklabelframe(statDialog, text = "WorkerId")
                workerid <- tclVar()
                worker.entry <- wzentry(wframe, width = 20, textvariable=workerid)
                tkgrid(worker.entry)
            tkgrid(wframe, row = 1)
            pframe <- ttklabelframe(statDialog, text = "Report period")
                periods <- c("OneDay","SevenDays","ThirtyDays","LifeToDate")
                period.entry <- tklistbox(pframe, 
                                          height = length(periods), width=20, 
                                          selectmode="single", background="white")
                tkgrid(period.entry)
                for (i in 1:length(periods)) {
                    tkinsert(period.entry,"end",periods[i])
                }
                tkselection.set(period.entry,3)
            tkgrid(pframe, row = 2)
            okcancel(statDialog, okfun = getReport, cancelfun = function() {tkdestroy(statDialog); tkfocus(wizard)})
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
                test <- list(Test=testtowrite,
                             AnswerKey=answertowrite,
                             TestDurationInSeconds=testduration )
                assign("test", test, envir=wizardenv)
            }
            
            testDialog <- tktoplevel()
            tkwm.title(testDialog, "Configure Qualification Test")
            aframe <- ttklabelframe(testDialog, text = "Qualification Test XML:")
                test.scroll <- tkscrollbar(aframe, repeatinterval = 5,
                                           command = function(...){ tkyview(test.entry, ...) })
                test.entry <- tktext(aframe, height = 6, width = 75, background = "white",
                                     yscrollcommand = function(...) tkset(test.scroll, ...))
                tkmark.set(test.entry,"insert","0.0")
                tkgrid(test.entry, row = 1, column = 1, sticky = "nsew")
                tkgrid(test.scroll, row = 1, column = 2, sticky = "nsew")
            bframe <- ttklabelframe(testDialog, text = "AnswerKey XML (optional):")
                answer.scroll <- tkscrollbar(bframe, repeatinterval = 5,
                                             command = function(...){ tkyview(answer.entry, ...) })
                answer.entry <- tktext(bframe, height = 6, width = 75, background = "white",
                                       yscrollcommand = function(...) tkset(answer.scroll, ...))
                tkmark.set(answer.entry,"insert","0.0")
                tkgrid(answer.entry, row = 1, column = 1, sticky = "nsew")
                tkgrid(answer.scroll, row = 1, column = 2, sticky = "nsew")
            tkgrid(aframe, row = 1, column = 1, columnspan = 2, sticky = "nsew")
            tkgrid(bframe, row = 2, column = 1, columnspan = 2, sticky = "nsew")
            tkgrid.columnconfigure(aframe, 1, weight = 1)
            tkgrid.columnconfigure(aframe, 2, weight = 0)
            tkgrid.rowconfigure(aframe, 1, weight = 1)
            tkgrid.columnconfigure(bframe, 1, weight = 1)
            tkgrid.columnconfigure(bframe, 2, weight = 0)
            tkgrid.rowconfigure(bframe, 1, weight = 1)
            cframe <- ttklabelframe(testDialog, text = "How long should the test last?")
                days <- tclVar("0")
                hours <- tclVar("0")
                mins <- tclVar("0")
                secs <- tclVar("0")
                days.entry <- wzentry(cframe, width = 5, textvariable = days)
                hours.entry <- wzentry(cframe, width = 5, textvariable = hours)
                mins.entry <- wzentry(cframe, width = 5, textvariable = mins)
                secs.entry <- wzentry(cframe, width = 5, textvariable = secs)
                tkgrid(ttklabel(cframe, text = "Days: "), row=1, column = 1)
                tkgrid(days.entry, row=1, column = 2)
                tkgrid(ttklabel(cframe, text = "Hours: "), row=1, column = 3)
                tkgrid(hours.entry, row=1, column = 4)
                tkgrid(ttklabel(cframe, text = "Minutes: "), row=1, column = 5)
                tkgrid(mins.entry, row=1, column = 6)
                tkgrid(ttklabel(cframe, text = "Seconds: "), row=1, column = 7)
                tkgrid(secs.entry, row=1, column = 8)
            tkgrid(cframe, row = 3, column = 1, sticky = "nsew")
            tkgrid(ttkbutton(testDialog, text = "Generate AnswerKey Template from Test", 
                             command = function() {
                                test <- tclvalue(tkget(test.entry, "0.0", "end"))
                                if(test == "") return(NULL)
                                test <- try(xmlParse(test))
                                if(!inherits(test, "try-error")) {
                                    a <- GenerateAnswerKey(AnswerKeyTemplate(test))
                                    tkinsert(answer.entry, "insert", a$string)
                                }
                                return(NULL)
                             }), row = 3, column = 2)
            okcancel(testDialog, row = 4, column = 1, columnspan = 2, stick = "s", okfun = storetest, 
                     cancelfun = function() {tkdestroy(testDialog); tkfocus(wizard)})
            tkgrid.columnconfigure(testDialog, 1, weight = 1)
            tkgrid.columnconfigure(testDialog, 2, weight = 1)
            tkgrid.rowconfigure(testDialog, 1, weight = 6)
            tkgrid.rowconfigure(testDialog, 2, weight = 6)
            tkgrid.rowconfigure(testDialog, 3, weight = 1)
            tkgrid.rowconfigure(testDialog, 4, weight = 1)
            tkfocus(testDialog)
        }
        
        # create qual
        createqualWiz <- function(){
            # function
            create <- function(){
                if(tclvalue(name)==""){
                    tkmessageBox(message="Please enter a name!", type="ok")
                    tkfocus(createqualWiz)
                    return(NULL)
                }
                if(tclvalue(desc)==""){
                    tkmessageBox(message="Please enter a description!", type="ok")
                    tkfocus(createqualWiz)
                    return(NULL)
                }
                if(is.null(wizardenv$test)){
                    test <- NULL
                    answerkey <- NULL
                    test.duration <- NULL
                }
                if(tclvalue(keywords)=="")
                    keywords <- NULL
                else 
                    keywords <- tclvalue(keywords)
                if(is.null(wizardenv$test)){
                    test <- NULL
                    answerkey <- NULL
                    test.duration <- NULL
                    temp <- tclvalue(auto.value)
                    if(temp=="") {
                        auto.value <- NULL
                        auto <- FALSE
                    } else {
                        auto.value <- temp
                        auto <- TRUE
                    }
                } else {
                    test <- wizardenv$test$Test
                    answerkey <- wizardenv$test$AnswerKey
                    test.duration <- wizardenv$test$TestDurationInSeconds
                    auto <- NULL
                    auto.value <- NULL
                }
                if(tclvalue(days)=="" && tclvalue(hours)=="" && tclvalue(mins)=="" && tclvalue(secs)=="") {
                    delay <- NULL
                } else {
                    delay <- seconds(as.numeric(tclvalue(days)),
                                     as.numeric(tclvalue(hours)),
                                     as.numeric(tclvalue(mins)),
                                     as.numeric(tclvalue(secs)))
                } 
                statselect <- statusopts[as.numeric(as.character(tkcurselection(statuslist)))+1] # listbox index starts at 0
                results <- CreateQualificationType(name=tclvalue(name), description=tclvalue(desc),
                                                   status=statselect, keywords = keywords,
                                                   retry.delay = delay,
                                                   test = test, answerkey = answerkey, test.duration = test.duration,
                                                   auto = auto, auto.value = auto.value,
                                                   verbose = TRUE, sandbox=sboxval())
                tkdestroy(createqualDialog)
                tkfocus(wizard)
            }
            
            assign("test",NULL,envir=wizardenv)
            
            createqualDialog <- tktoplevel()
            tkwm.title(createqualDialog, "Create QualificationType")
            name <- tclVar()
            desc <- tclVar()
            keywords <- tclVar()
            auto.value <- tclVar()
            aframe <- ttklabelframe(createqualDialog, text = "Public Name for QualificationType:")
                tkgrid(wzentry(aframe, width = 60, textvariable=name))
            tkgrid(aframe, sticky = "w", row = 1, column = 1, columnspan = 2)
            bframe <- ttklabelframe(createqualDialog, text = "Public Description:")
                tkgrid(wzentry(bframe, width = 60, textvariable=desc))
            tkgrid(bframe, sticky = "w", row = 2, column = 1, columnspan = 2)
            cframe <- ttklabelframe(createqualDialog, text = "Keywords (comma-separated):")
                tkgrid(wzentry(cframe, width = 60, textvariable=keywords))
            tkgrid(cframe, sticky = "w", row = 3, column = 1, columnspan = 2)
            dframe <- ttklabelframe(createqualDialog, text = "Status:")
                statuslist <- tklistbox(dframe, height=2, width=20, selectmode="single", background="white")
                    statusopts <- c("Active","Inactive")
                    tkinsert(statuslist,"end", statusopts[1])
                    tkinsert(statuslist,"end", statusopts[2])
                    tkselection.set(statuslist,0)
                tkgrid(statuslist)
            tkgrid(dframe, sticky = "w", row = 4, column = 1, columnspan = 2)
            eframe <- ttklabelframe(createqualDialog, text = "Granted automatically?")
                tkgrid(ttklabel(eframe, text = "If so, set score: "))
                tkgrid(wzentry(eframe, width = 15, textvariable=auto.value))
            tkgrid(eframe, sticky = "w", row = 5, column = 1)
            hframe <- ttklabelframe(createqualDialog, text = "Qualification Test: ")
                tkgrid(ttkbutton(hframe, text=" Add Optional Test ", command=qualtest))
                tkgrid(ttklabel(hframe, text = "Use instead of automatic value"))
            tkgrid(hframe, sticky = "w", row = 5, column = 2)
            gframe <- ttklabelframe(createqualDialog, text = "How long should workers have to wait to retry?")
                days <- tclVar("")
                hours <- tclVar("")
                mins <- tclVar("")
                secs <- tclVar("")
                days.entry <- wzentry(gframe, width = 5, textvariable=days)
                hours.entry <- wzentry(gframe, width = 5, textvariable=hours)
                mins.entry <- wzentry(gframe, width = 5, textvariable=mins)
                secs.entry <- wzentry(gframe, width = 5, textvariable=secs)
                tkgrid(ttklabel(gframe, text = "Days: "), row=1, column=1)
                tkgrid(days.entry, row=1, column=2)
                tkgrid(ttklabel(gframe, text = "Hours: "), row=1, column=3)
                tkgrid(hours.entry, row=1, column=4)
                tkgrid(ttklabel(gframe, text = "Minutes: "), row=1, column=5)
                tkgrid(mins.entry, row=1, column=6)
                tkgrid(ttklabel(gframe, text = "Seconds: "), row=1, column=7)
                tkgrid(secs.entry, row=1, column=8)
                tkgrid(ttklabel(gframe, text = "(Leave all blank to disable retries.)"), row=2, column=1, columnspan = 8, sticky = "w")
            tkgrid(gframe, sticky = "w", row = 6, column = 1, columnspan = 2)
            okcancel(createqualDialog, 
                     okfun = create, 
                     cancelfun = function() {tkdestroy(createqualDialog); tkfocus(wizard)}, 
                     column = 1, columnspan = 2)
            tkfocus(createqualDialog)
        }
        
        # update qual
        updatequalWiz <- function(){
            # function
            updateq <- function(){
                if(tclvalue(wizardenv$qualid)==""){
                    tkmessageBox(message="Please enter a QualificationTypeId!", type="ok")
                    tkfocus(updatequalWiz)
                    return(NULL)
                }
                if(is.null(wizardenv$test)){
                    test <- NULL
                    answerkey <- NULL
                    test.duration <- NULL
                    temp <- tclvalue(auto.value)
                    if(temp=="") {
                        auto.value <- NULL
                        auto <- FALSE
                    } else {
                        auto.value <- temp
                        auto <- TRUE
                    }
                } else {
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
                qual <- UpdateQualificationType(qual=tclvalue(wizardenv$qualid),
                                                description=tclvalue(desc),
                                                status=statselect,
                                                retry.delay=delay,
                                                test = test, answerkey = answerkey, test.duration = test.duration,
                                                auto = auto, auto.value = auto.value,
                                                verbose=FALSE, sandbox=sboxval()
                                                )
                tkdestroy(updatequalDialog)
                tkfocus(wizard)
            }
            
            assign("test",NULL,envir=wizardenv)
            
            updatequalDialog <- tktoplevel()
            tkwm.title(updatequalDialog, "Update QualificationType")
            wizardenv$qualid <- tclVar()
            desc <- tclVar()
            keywords <- tclVar()
            auto.value <- tclVar()
            aframe <- ttklabelframe(updatequalDialog, text = "QualificationTypeId")
                tkgrid(wzentry(aframe, width = 60, textvariable=wizardenv$qualid))
            tkgrid(aframe, sticky = "w", row = 1, column = 1, columnspan = 2)
            bframe <- ttklabelframe(updatequalDialog, text = "Public Description:")
                tkgrid(wzentry(bframe, width = 60, textvariable=desc))
            tkgrid(bframe, sticky = "w", row = 2, column = 1, columnspan = 2)
            cframe <- ttklabelframe(updatequalDialog, text = "Keywords (comma-separated):")
                tkgrid(wzentry(cframe, width = 60, textvariable=keywords))
            tkgrid(cframe, sticky = "w", row = 3, column = 1, columnspan = 2)
            dframe <- ttklabelframe(updatequalDialog, text = "Status:")
                statuslist <- tklistbox(dframe, height=2, width=20, selectmode="single", background="white")
                    statusopts <- c("Active","Inactive")
                    tkinsert(statuslist,"end", statusopts[1])
                    tkinsert(statuslist,"end", statusopts[2])
                    tkselection.set(statuslist,0)
                tkgrid(statuslist)
            tkgrid(dframe, sticky = "w", row = 4, column = 1, columnspan = 2)
            eframe <- ttklabelframe(updatequalDialog, text = "Granted automatically?")
                tkgrid(ttklabel(eframe, text = "If so, set score: "))
                tkgrid(wzentry(eframe, width = 15, textvariable=auto.value))
            tkgrid(eframe, sticky = "w", row = 5, column = 1)
            hframe <- ttklabelframe(updatequalDialog, text = "Qualification Test: ")
                tkgrid(ttkbutton(hframe, text=" Add Optional Test ", command=qualtest))
                tkgrid(ttklabel(hframe, text = "Use instead of automatic value"))
            tkgrid(hframe, sticky = "w", row = 5, column = 2)
            gframe <- ttklabelframe(updatequalDialog, text = "How long should workers have to wait to retry?")
                days <- tclVar("")
                hours <- tclVar("")
                mins <- tclVar("")
                secs <- tclVar("")
                days.entry <- wzentry(gframe, width = 5, textvariable=days)
                hours.entry <- wzentry(gframe, width = 5, textvariable=hours)
                mins.entry <- wzentry(gframe, width = 5, textvariable=mins)
                secs.entry <- wzentry(gframe, width = 5, textvariable=secs)
                tkgrid(ttklabel(gframe, text = "Days: "), row=1, column=1)
                tkgrid(days.entry, row=1, column=2)
                tkgrid(ttklabel(gframe, text = "Hours: "), row=1, column=3)
                tkgrid(hours.entry, row=1, column=4)
                tkgrid(ttklabel(gframe, text = "Minutes: "), row=1, column=5)
                tkgrid(mins.entry, row=1, column=6)
                tkgrid(ttklabel(gframe, text = "Seconds: "), row=1, column=7)
                tkgrid(secs.entry, row=1, column=8)
                tkgrid(ttklabel(gframe, text = "(Leave all blank to disable retries.)"), row=2, column=1, columnspan = 8, sticky = "w")
            tkgrid(gframe, sticky = "w", row = 6, column = 1, columnspan = 2)
            popbuttons(updatequalDialog, okfun = updateq, 
                       cancelfun = function(){tkdestroy(updatequalDialog); tkfocus(wizard)}, 
                       poptype = "SearchQual", column = 1, columnspan = 2)
            tkfocus(updatequalDialog)
        }
        
        # view qual
        getqualWiz <- function(){
            # function
            getqual <- function(){
                if(tclvalue(wizardenv$qualid)==""){
                    tkmessageBox(message="Please enter a QualificationTypeId!", type="ok")
                    tkfocus(getqualDialog)
                } else {
                    results <- GetQualificationType(qual=tclvalue(wizardenv$qualid), verbose=FALSE, sandbox=sboxval())
                    tkdestroy(getqualDialog)
                    print(t(results))
                    tkfocus(wizard)
                }
            }
            
            getqualDialog <- tktoplevel()
            tkwm.title(getqualDialog, "Get QualificationType")
            wizardenv$qualid <- tclVar()
            aframe <- ttklabelframe(getqualDialog, text = "QualificationTypeId: ")
            tkgrid(wzentry(aframe, width = 50, textvariable = wizardenv$qualid))
            tkgrid(aframe, sticky = "w")
            buttons <- tkframe(getqualDialog)
                populate <- function(){
                    searchqualsWiz()
                    tkdestroy(getqualDialog)
                    results <- GetQualificationType(qual=tclvalue(wizardenv$qualid), verbose=FALSE, sandbox=sboxval())
                    print(t(results),quote=FALSE)
                    tkfocus(wizard)
                }
                populatebutton <- ttkbutton(buttons, text="Search for QualificationTypes", command=populate)
                OKbutton <- ttkbutton(buttons,text="   OK   ",command=getqual)
                Cancelbutton <- ttkbutton(buttons,text=" Cancel ",command=function() {
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
                if(tclvalue(wizardenv$qualid)==""){
                    tkmessageBox(message="Please enter a QualificationTypeId!", type="ok")
                    tkfocus(disposequalDialog)
                    return(NULL)
                }
                exit <- tkmessageBox(message = "Are you sure you want to dispose the Qualification?
                        This will delete all Qualification and Score data.", icon = "question", type = "yesno", default = "no")
                if(tclvalue(exit)=="yes"){
                    tkdestroy(disposequalDialog)
                    results <- DisposeQualificationType(qual = tclvalue(wizardenv$qualid), 
                                                        verbose = FALSE, 
                                                        sandbox = sboxval())
                } else {
                    tkfocus(disposequalDialog)
                }
                tkfocus(wizard)
            }
            
            disposequalDialog <- tktoplevel()
            tkwm.title(disposequalDialog, "Dispose QualificationType")
            wizardenv$qualid <- tclVar()
            aframe <- ttklabelframe(disposequalDialog, text = "QualificationTypeId: ")
            tkgrid(wzentry(aframe, width = 50, textvariable = wizardenv$qualid))
            tkgrid(aframe, sticky = "w")
            popbuttons(disposequalDialog, okfun = disposequal, 
                       cancelfun = function(){tkdestroy(disposequalDialog); tkfocus(wizard)}, 
                       poptype = "SearchQual")
            tkfocus(disposequalDialog)
        }
        
        # get workers by qual
        getworkersbyqualWiz <- function(){
            # function
            getquals <- function(){
                if(tclvalue(wizardenv$qualid)==""){
                    tkmessageBox(message="Please enter a QualificationTypeId!", type="ok")
                    tkfocus(getqualsDialog)
                    return(NULL)
                }
                tkdestroy(getqualsDialog)
                results <- GetQualifications(qual = tclvalue(wizardenv$qualid), 
                                             verbose = TRUE, 
                                             return.all = TRUE, 
                                             sandbox = sboxval())
                print(results)
                tkfocus(wizard)
            }
            getqualsDialog <- tktoplevel()
            tkwm.title(getqualsDialog, "Get Qualifications for QualificationType")
            wizardenv$qualid <- tclVar()
            aframe <- ttklabelframe(getqualsDialog, text = "QualificationTypeId: ")
            tkgrid(wzentry(aframe, width = 50, textvariable = wizardenv$qualid))
            tkgrid(aframe, sticky = "w")
            popbuttons(getqualsDialog, okfun = getquals, 
                       cancelfun = function(){tkdestroy(getqualsDialog); tkfocus(wizard)}, 
                       poptype = "SearchQual")
            tkfocus(getqualsDialog)
        }
        
        # get worker score
        getscoreWiz <- function(){
            # function
            getscore <- function(){
                workers <- strsplit(tclvalue(tkget(worker.entry,"0.0","end")),"[\n]+")[[1]]
                if(tclvalue(wizardenv$qualid)==""){
                    tkmessageBox(message="Please enter a QualificationTypeId!", type="ok")
                    tkfocus(getscoreDialog)
                    return(NULL)
                }
                if(tclvalue(workers)==""){
                    tkmessageBox(message="Please enter a WorkerId!", type="ok")
                    tkfocus(getscoreDialog)
                    return(NULL)
                }
                workers <- gsub("[[:space:]]", "", workers)
                results <- GetQualificationScore(qual = tclvalue(wizardenv$qualid), 
                                                 workers = workers, 
                                                 verbose = FALSE, 
                                                 sandbox = sboxval())
                tkdestroy(getscoreDialog)
                tkfocus(wizard)
            }
            
            getscoreDialog <- tktoplevel()
            tkwm.title(getscoreDialog, "Get Qualification Score(s)")
            wizardenv$qualid <- tclVar()
            aframe <- ttklabelframe(getscoreDialog, text = "QualificationTypeId: ")
            tkgrid(wzentry(aframe, width = 50, textvariable = wizardenv$qualid))
            bframe <- ttklabelframe(getscoreDialog, text = "WorkerId(s) (one per line): ")
                worker.entry <- tktext(bframe, height = 20, width = 50, background = "white")
                tkmark.set(worker.entry,"insert","0.0")
                tkgrid(worker.entry)
            tkgrid(aframe, sticky = "w")
            tkgrid(bframe, sticky = "w")
            popbuttons(getscoreDialog, okfun = getscore, 
                       cancelfun = function(){tkdestroy(getscoreDialog); tkfocus(wizard)}, 
                       poptype = "SearchQual")
            tkfocus(getscoreDialog)
        }
        
        # assign qualification to worker
        assignqualWiz <- function(){
            # function
            assignscore <- function(){
                workers <- strsplit(tclvalue(tkget(worker.entry,"0.0","end")),"[\n]+")[[1]]
                if(tclvalue(wizardenv$qualid)==""){
                    tkmessageBox(message="Please enter a QualificationTypeId!", type="ok")
                    tkfocus(assignqualDialog)
                    return(NULL)
                }
                if(!length(workers) || workers == ""){
                    tkmessageBox(message="Please enter at least one WorkerId!", type="ok")
                    tkfocus(assignqualDialog)
                    return(NULL)
                }
                if(tclvalue(score)==""){
                    tkmessageBox(message="Please enter a score!", type="ok")
                    tkfocus(assignqualDialog)
                    return(NULL)
                }
                if(tclvalue(score) == "") {
                    score <- NULL
                } else {
                    score <- tclvalue(score)
                }
                workers <- gsub("[[:space:]]", "", workers)
                results <- AssignQualification(qual = tclvalue(wizardenv$qualid), 
                                               workers = workers, 
                                               value = score, 
                                               verbose = TRUE, 
                                               sandbox = sboxval())
                tkdestroy(assignqualDialog)
                tkfocus(wizard)
            }
            
            assignqualDialog <- tktoplevel()
            tkwm.title(assignqualDialog, "Assign Qualification to Worker")
            wizardenv$qualid <- tclVar()
            score <- tclVar()
            notify <- tclVar(1)
            entry <- tkframe(assignqualDialog)
            aframe <- ttklabelframe(entry, text = "QualificationTypeId: ")
            tkgrid(wzentry(aframe, width = 50, textvariable = wizardenv$qualid))
            bframe <- ttklabelframe(entry, text = "WorkerId(s) (one per line): ")
                worker.entry <- tktext(bframe, height = 20, width = 50, background = "white")
                tkmark.set(worker.entry,"insert","0.0")
                tkgrid(worker.entry)
            cframe <- ttklabelframe(entry, text = "Specify new score value: ")
                tkgrid(ttklabel(cframe, text = "New score: "), row = 1, column = 1, sticky="e")
                tkgrid(wzentry(cframe, width = 10, textvariable=score), row = 1, column=2, sticky="w")
            dframe <- ttklabelframe(entry, text = "Notify workers? ")
                tkgrid(ttklabel(dframe, text = "Yes "), row = 1, column = 1, sticky="e")
                tkgrid(ttkcheckbutton(dframe, variable = notify), row = 1, column=2, sticky="w")
            tkgrid(aframe, sticky = "w", column = 1, columnspan = 2)
            tkgrid(bframe, sticky = "w", column = 1, columnspan = 2)
            tkgrid(cframe, sticky = "w", row = 3, column = 1)
            tkgrid(dframe, sticky = "w", row = 3, column = 2)
            tkgrid(entry)
            popbuttons(assignqualDialog, okfun = assignscore, 
                       cancelfun = function(){tkdestroy(assignqualDialog); tkfocus(wizard)}, 
                       poptype = "SearchQual")
            tkfocus(assignqualDialog)
        }
        
        # update worker score
        updatescoreWiz <- function(){
            # function
            updatescore <- function(){
                workers <- strsplit(tclvalue(tkget(worker.entry,"0.0","end")),"[\n]+")[[1]]
                if(tclvalue(wizardenv$qualid)==""){
                    tkmessageBox(message="Please enter a QualificationTypeId!", type="ok")
                    tkfocus(updatescoreDialog)
                    return(NULL)
                }
                if(!length(workers) || workers == ""){
                    tkmessageBox(message="Please enter at least one WorkerId!", type="ok")
                    tkfocus(updatescoreDialog)
                    return(NULL)
                }
                if(tclvalue(score)=="" && tclvalue(increment)==""){
                    tkmessageBox(message="Please enter a new score or an increment!", type="ok")
                    tkfocus(updatescoreDialog)
                    return(NULL)
                }
                if(tclvalue(score) == "") {
                    score <- NULL
                } else {
                    score <- tclvalue(score)
                }
                if(tclvalue(increment) == "") {
                    increment <- NULL
                } else {
                    increment <- tclvalue(increment)
                }
                workers <- gsub("[[:space:]]", "", workers)
                results <- UpdateQualificationScore(qual = tclvalue(wizardenv$qualid), 
                                                    workers = workers, 
                                                    values = score, 
                                                    increment = increment,
                                                    verbose = TRUE, 
                                                    sandbox = sboxval())
                tkdestroy(updatescoreDialog)
                tkfocus(wizard)
            }
            
            updatescoreDialog <- tktoplevel()
            tkwm.title(updatescoreDialog, "Update Qualification Score(s)")
            wizardenv$qualid <- tclVar()
            score <- tclVar()
            increment <- tclVar()
            
            aframe <- ttklabelframe(updatescoreDialog, text = "QualificationTypeId: ")
            tkgrid(wzentry(aframe, width = 50, textvariable = wizardenv$qualid))
            bframe <- ttklabelframe(updatescoreDialog, text = "WorkerId(s) (one per line): ")
                worker.entry <- tktext(bframe, height = 20, width = 50, background = "white")
                tkmark.set(worker.entry,"insert","0.0")
                tkgrid(worker.entry)
            cframe <- ttklabelframe(updatescoreDialog, text = "Specify new score or score increment value: ")
                tkgrid(ttklabel(cframe, text = "      New score: "), row = 1, column = 1, sticky="e")
                tkgrid(wzentry(cframe, width = 10, textvariable=score), row = 1, column=2, sticky="w")
                tkgrid(ttklabel(cframe, text = "Increment value: "), row = 2, column=1, sticky="e")
                tkgrid(wzentry(cframe, width = 10, textvariable=increment), row = 2, column=2, sticky="w")
            tkgrid(aframe, sticky = "w")
            tkgrid(bframe, sticky = "w")
            tkgrid(cframe, sticky = "w")
            
            popbuttons(updatescoreDialog, okfun = updatescore, 
                       cancelfun = function(){tkdestroy(updatescoreDialog); tkfocus(wizard)}, 
                       poptype = "SearchQual")
            tkfocus(updatescoreDialog)
        }
        
        # search quals
        searchqualsWiz <- function(){
            # function
            searchqual <- function(){
                if(tclvalue(searchquery)==""){
                    searchquery <- NULL
                }
                if(as.character(tclvalue(mine))=="1") {
                    mine <- TRUE
                } else { 
                    mine <- FALSE
                }
                if(as.character(tclvalue(requestable))=="1") {
                    requestable <- TRUE
                } else {
                    requestable <- FALSE
                }
                results <- SearchQualificationTypes(query=searchquery,only.mine=mine,only.requestable=requestable,
                                                    verbose=FALSE,sandbox=sboxval())
                if(tclvalue(builtin)=="1"){
                    output <- ListQualificationTypes()
                    for(i in 1:dim(results)[1]){
                        output[dim(output)[1]+1,] <- c(paste("User-Defined:",results$Name[i]),results$QualificationTypeId[i])
                    }
                } else{
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
                    tclvalue(wizardenv$qualid) <- selection[2]
                    invisible(selection)
                }
                
                selectqualDialog <- tktoplevel()
                tkwm.title(selectqualDialog, "Current QualificationTypes")
                tkgrab.set(selectqualDialog)
                entryform <- tkframe(selectqualDialog)
                    r <- 1
                    scr <- ttkscrollbar(entryform,  command=function(...) tkyview(quallist,...))
                    quallist <- tklistbox(    entryform, height=20, width=100, selectmode="single",
                                            yscrollcommand=function(...) tkset(scr,...), background="white")
                    tkgrid(quallist, row=r, column=1, columnspan=2)
                    tkgrid(scr, row=r, column=3, sticky="nsw")
                tkgrid(entryform)
                for (i in 1:dim(output)[1]) {
                    tkinsert(quallist,"end", paste(    output$Qualification[i],
                                                    "    (QualificationTypeId: ", output$QualificationTypeId[i],")",sep=""))
                }
                okcancel(selectqualDialog, okfun = function() {invisible(storequal())}, 
                         cancelfun = function() {
                            tkgrab.release(selectqualDialog)
                            tkdestroy(selectqualDialog)
                            tkdestroy(searchqualDialog)
                        })
                tkfocus(selectqualDialog)
                tkwait.window(selectqualDialog)
            }
            
            searchqualDialog <- tktoplevel()
            tkwm.title(searchqualDialog, "Search for QualificationTypes")
            mine <- tclVar("1")
            requestable <- tclVar("0")
            builtin <- tclVar("1")
            searchquery <- tclVar()
            aframe <- ttklabelframe(searchqualDialog, text = "Search Options")
                tkgrid(ttkcheckbutton(aframe, variable=mine), row = 1, column = 1, sticky = "e")
                tkgrid(ttklabel(aframe, text = "Only search my QualificationTypes"), row = 1, column = 2, sticky = "w")
                tkgrid(ttkcheckbutton(aframe, variable=requestable), row = 2, column = 1, sticky = "e")
                tkgrid(ttklabel(aframe, text = "Only search requestable QualificationTypes"), row = 2, column = 2, sticky = "w")
                tkgrid(ttkcheckbutton(aframe, variable=builtin), row = 3, column = 1, sticky = "e")
                tkgrid(ttklabel(aframe, text = "Include built-in QualificationTypes"), row = 3, column = 2, sticky = "w")
            bframe <- ttklabelframe(searchqualDialog, text = "Search Query (optional):")
                tkgrid(wzentry(bframe, width = 50, textvariable=searchquery))
            tkgrid(aframe, stick = "w")
            tkgrid(bframe, stick = "w")
            okcancel(searchqualDialog, okfun = searchqual, 
                         cancelfun = function() {
                            tkgrab.release(searchqualDialog)
                            tkdestroy(searchqualDialog)
                        })
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
                    } else {
                        tkdestroy(grantreq)
                        pos <- as.numeric(tkcurselection(qualreqlist))+1
                        qualreqid <- wizardenv$qualrequests$QualificationRequestId[pos]
                        GrantQualifications(qual.requests=qualreqid, values=tclvalue(value), verbose=FALSE, sandbox=sboxval())
                        tkinsert(qualreqlist,tkcurselection(qualreqlist),"Granted")
                        tkdelete(qualreqlist,tkcurselection(qualreqlist))
                        tkfocus(qualreqDialog)
                    }
                }
                
                grantreq <- tktoplevel()
                tkwm.title(grantreq, "Assign Value for Qualification")
                r <- 1
                tkgrid(ttklabel(grantreq, text = "     "), row=r, column=1)
                tkgrid(ttklabel(grantreq, text = "     "), row=r, column=4)
                r <- r + 1
                value <- tclVar()
                value.entry <- wzentry(grantreq, width=10, textvariable=value)
                tkgrid(ttklabel(grantreq, text = "Qualification Value: "), row=r, column=2, sticky="e")
                tkgrid(value.entry, row=r, column=3, sticky="w")
                r <- r + 1
                tkgrid(ttklabel(grantreq, text = "     "), row=r, column=1)
                OKbutton <- ttkbutton(grantreq,text="   OK   ",command=grant)
                Cancelbutton <- ttkbutton(grantreq,text=" Cancel ",command=function() tkdestroy(grantreq) )
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
                    } else {
                        if(tclvalue(reason)=="")
                            reason1 <- NULL
                        tkdestroy(rejreq)
                        pos <- as.numeric(tkcurselection(qualreqlist))+1
                        qualreqid <- wizardenv$qualrequests$QualificationRequestId[pos]
                        RejectQualifications(qual.requests=qualreqid, reason=reason1, verbose=FALSE, sandbox=sboxval())
                        tkinsert(qualreqlist,tkcurselection(qualreqlist),"Rejected")
                        #tkdelete(qualreqlist,"rows",tkcurselection(qualreqlist))
                        tkdelete(qualreqlist,tkcurselection(qualreqlist))
                        tkfocus(qualreqDialog)
                    }
                }
                
                rejreq <- tktoplevel()
                tkwm.title(rejreq, "Reason for rejection?")
                r <- 1
                tkgrid(ttklabel(rejreq, text = "     "), row=r, column=1)
                tkgrid(ttklabel(rejreq, text = "     "), row=r, column=4)
                r <- r + 1
                reason <- tclVar()
                reason.entry <- wzentry(rejreq, width=50, textvariable=reason)
                tkgrid(ttklabel(rejreq, text = "Reason (optional; visible to worker): "), row=r, column=2, sticky="e")
                tkgrid(reason.entry, row=r, column=3, sticky="w")
                r <- r + 1
                tkgrid(ttklabel(rejreq, text = "     "), row=r, column=1)
                OKbutton <- ttkbutton(rejreq,text="   OK   ",command=rej)
                Cancelbutton <- ttkbutton(rejreq,text=" Cancel ",command=function() tkdestroy(rejreq) )
                r <- r + 1
                tkgrid(OKbutton, row = r, column = 2)
                tkgrid(Cancelbutton, row=r, column = 3)
            }
        
            
            qualreqDialog <- tktoplevel()
            tkwm.title(qualreqDialog, "Qualification Requests")
            entryform <- tkframe(qualreqDialog, relief="groove", borderwidth=2)
                r <- 1
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=1)
                tkgrid(ttklabel(entryform, text = "     "), row=r, column=4)
                r <- r + 1
                scr <- ttkscrollbar(entryform,  command=function(...) tkyview(qualreqlist,...))
                qualreqlist <- tklistbox(entryform, width=50, height=15, selectmode="single",
                                            yscrollcommand=function(...) tkset(scr,...), background="white")
                tkgrid(qualreqlist,scr, row=r, column=2, columnspan=2)
                tkgrid.configure(scr, sticky="nsw")
                r <- r + 1
                
                r <- r + 1
                tkgrid(ttklabel(entryform, text = "     "), row=r)
            tkgrid(entryform)
            buttons <- tkframe(qualreqDialog)
                viewbutton <- ttkbutton(buttons,text=" View Selected ",command=viewrequest)
                appbutton <- ttkbutton(buttons,text=" Grant Selected ",command=approverequests)
                rejbutton <- ttkbutton(buttons,text=" Reject Selected ",command=rejectrequests)
                Cancelbutton <- ttkbutton(buttons,text=" Close ",command=function() tkdestroy(qualreqDialog) )
                r <- 1
                tkgrid(viewbutton, row = r, column = 1)
                tkgrid(appbutton, row = r, column = 2)
                tkgrid(rejbutton, row = r, column = 3)
                tkgrid(Cancelbutton, row=r, column = 4)
            tkgrid(buttons)
            
            # function to populate QualificationRequests
            populate <- function(){
                # function
                setqual <- function(){
                    tkgrab.release(getqualDialog)
                    tkdestroy(getqualDialog)
                    requests <- GetQualificationRequests(qual = tclvalue(wizardenv$qualid), 
                                                         return.all = TRUE, 
                                                         verbose = FALSE, 
                                                         sandbox = sboxval())
                    for(i in 1:dim(requests)[1]) {
                        tkinsert(qualreqlist,"end",paste("Worker: ", requests$SubjectId[i], "  (",requests$SubmitTime[i],")", sep=""))
                    }
                    assign("qualrequests", requests, envir=wizardenv) # store 'qualrequests' to wizardenv
                    tkfocus(qualreqDialog)
                }                
                getqualDialog <- tktoplevel()
                tkwm.title(getqualDialog, "Set QualificationType")
                wizardenv$qualid <- tclVar()
                aframe <- ttklabelframe(getqualDialog, text = "QualificationTypeId: ")
                tkgrid(wzentry(aframe, width = 50, textvariable=wizardenv$qualid))
                tkgrid(aframe)
                popbuttons(getqualDialog, okfun = setqual, 
                           cancelfun = function() {
                             tkdestroy(getqualDialog)
                             tkdestroy(qualreqDialog)
                             tkfocus(wizard)
                           },
                           poptype = "SearchQual")
                tkfocus(getqualDialog)
                tkwait.window(getqualDialog)
            }
            populate() # populate QualificationRequest list
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
            tkadd(actions, 'checkbutton', label='Use Sandbox?', variable = wizardenv$sandbox)
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
                tkadd(getassign, "command", label = "By Annotation/Batch", command = getassign4Wiz)
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
            tkadd(qualifications, "command", label = "Assign Qualification to Worker(s)", command = assignqualWiz)
            tkadd(qualifications, "command", label = "Get Worker Score(s)", command = getscoreWiz)
            tkadd(qualifications, "command", label = "Update Worker Score(s)", command = updatescoreWiz)
            tkadd(qualifications, "command", label = "Get Worker(s) By Qualification", command = getworkersbyqualWiz)
            tkadd(qualifications, "separator")
            tkadd(qualifications, "command", label = "List Built-In QualificationTypes", command = function() print(ListQualificationTypes()) )
        tkadd(topMenu, "cascade", label = "Qualifications", menu = qualifications, underline = 0)
        helpmenu <- tkmenu(topMenu, tearoff = FALSE)
            # help menu
            tkadd(helpmenu, "command", label = "MTurkR Wizard Documentation", command = function()
                browseURL("https://github.com/leeper/MTurkR/wiki/Wizard-Graphical") )
            tkadd(helpmenu, "command", label = "MTurkR Documentation", command = function() {
                browseURL("http://cran.r-project.org/web/packages/MTurkR/MTurkR.pdf") })
            tkadd(helpmenu, "command", label = "MTurkR Wiki", command = function()
                browseURL("https://github.com/leeper/MTurkR/wiki") )
            tkadd(helpmenu, "separator")
            tkadd(helpmenu, "command", label = "MTurk Worker Site", command = function() browseURL("http://www.mturk.com") )
            tkadd(helpmenu, "command", label = "MTurk Requester Site", command = function() browseURL("http://requester.mturk.com") )
            tkadd(helpmenu, "command", label = "Package Website", command = function() {
                browseURL("http://cran.r-project.org/web/packages/MTurkR/")})
            tkadd(helpmenu, "separator")
            tkadd(helpmenu, "command", label = "Submit Bug or Issue", command = function() {
                browseURL("https://github.com/leeper/MTurkR/issues/new")})
        tkadd(topMenu, "cascade", label = "Help", menu = helpmenu)
        
        tkfocus(wizard)
    }
}
