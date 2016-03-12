create <-
CreateHIT <-
createhit <-
function (hit.type = NULL, question = NULL, validate.question = FALSE,
    expiration, assignments = "1", 
    assignment.review.policy = NULL, hit.review.policy = NULL, 
    annotation = NULL, unique.request.token = NULL, title = NULL, 
    description = NULL, reward = NULL, duration = NULL, keywords = NULL, 
    auto.approval.delay = NULL, qual.req = NULL, hitlayoutid = NULL, 
    hitlayoutparameters = NULL, response.group = NULL, 
    verbose = getOption('MTurkR.verbose', TRUE), ...) {
    operation <- "CreateHIT"
    if (!is.null(hit.type)) {
        if (is.factor(hit.type)) {
            hit.type <- as.character(hit.type)
        }
        if ( (!is.null(title) || !is.null(description) ||
            !is.null(reward) || !is.null(duration)) & verbose) {
            warning("HITType specified, HITType parameters (title, description, reward, duration) ignored")
        }
        GETparameters <- paste("&HITTypeId=", hit.type, sep = "")
    } else {
        if (is.null(title) || is.null(description) || is.null(reward) || is.null(duration)) {
            stop("Must specify HITType xor HITType parameters (title, description, reward, duration)")
        } else {
            register <- RegisterHITType(title, description, reward, 
                duration, keywords = keywords, auto.approval.delay = auto.approval.delay, 
                qual.req = qual.req, ...)
            if (is.null(register$Valid)) {
                return(register)
            }
            if (!as.logical(register$Valid)) {
                stop("Could not RegisterHITType(), check parameters")
            } else {
                GETparameters <- paste("&HITTypeId=", register$HITTypeId, sep = "")
            }
        }
    }
    if (is.null(question)) {
        if (!is.null(hitlayoutid)) {
            GETparameters <- paste(GETparameters, "&HITLayoutId=", hitlayoutid, sep = "")
            if (!is.null(hitlayoutparameters)) {
                GETparameters <- paste(GETparameters, hitlayoutparameters, sep = "")
            }
        } else {
            stop("Must specify QuestionForm, HTMLQuestion, or ExternalQuestion for 'question' parameter; or a 'hitlayoutid'")
        }
    } else {
        if (class(question) %in% c('HTMLQuestion','ExternalQuestion')) {
            question <- question$string
        }
        if (validate.question==TRUE) {
            if (!is.null(xmlChildren(xmlParse(question))$QuestionForm)) {
                namespace <- xmlNamespace(xmlChildren(xmlParse(question))$QuestionForm)[1]
            } else if (!is.null(xmlChildren(xmlParse(question))$HTMLQuestion)) {
                namespace <- xmlNamespace(xmlChildren(xmlParse(question))$HTMLQuestion)[1]
            } else if (!is.null(xmlChildren(xmlParse(question))$ExternalQuestion)) {
                namespace <- xmlNamespace(xmlChildren(xmlParse(question))$ExternalQuestion)[1]
            } else {
                stop("No Namespace specified in 'question'")
            }
            validation <- xmlSchemaValidate(namespace, question)
            if (!validation$status==0) {
                warning("'question' object does not validate against MTurk schema")
                return(validation)
            }
        }
        GETparameters <- paste(GETparameters, "&Question=", curl_escape(question), sep = "")
    }
    if (is.null(expiration)) {
        stop("Must specify HIT LifetimeInSeconds for expiration parameter")
    } else if (as.numeric(expiration) < 30 | as.numeric(expiration) > 31536000) {
        stop("HIT LifetimeInSeconds/expiration must be between 30 and 31536000 seconds")
    } else {
        GETparameters <- paste(GETparameters, "&LifetimeInSeconds=", expiration, sep = "")
    }
    if (is.null(assignments)) {
        stop("Number of Assignments must be specified")
    } else if (as.numeric(assignments) < 1 | as.numeric(assignments) > 1e+09) {
        stop("MaxAssignments must be between 1 and 1000000000")
    } else {
        GETparameters <- paste(GETparameters, "&MaxAssignments=", assignments, sep = "")
    }
    if (!is.null(response.group)) {
        if (any(!response.group %in% c("Request", "Minimal", "HITDetail", "HITQuestion", "HITAssignmentSummary"))) {
            stop("ResponseGroup must be in c(Request,Minimal,HITDetail,HITQuestion,HITAssignmentSummary)")
        }
        if (length(response.group) == 1) {
            GETparameters <- paste(GETparameters, "&ResponseGroup=", response.group, sep = "")
        } else {
            for (i in 1:length(response.group)) {
                GETparameters <- paste(GETparameters, "&ResponseGroup", i-1,
                                       "=", response.group[i], sep = "")
            }
        }
    }
    if (!is.null(assignment.review.policy)) {
        GETparameters <- paste(GETparameters, assignment.review.policy, sep = "")
    }
    if (!is.null(hit.review.policy)) {
        GETparameters <- paste(GETparameters, hit.review.policy, sep = "")
    }
    if (!is.null(annotation) && nchar(curl_escape(annotation)) > 255) {
        stop("Annotation must be <= 255 characters")
    } else if (!is.null(annotation)) {
        GETparameters <- paste(GETparameters, "&RequesterAnnotation=", 
                               curl_escape(annotation), sep = "")
    }
    if (!is.null(unique.request.token) && nchar(curl_escape(unique.request.token)) > 64) {
        stop("UniqueRequestToken must be <= 64 characters")
    } else if (!is.null(unique.request.token)) {
        GETparameters <- paste(GETparameters, "&UniqueRequestToken=", 
                               curl_escape(unique.request.token), sep = "")
    }
    request <- request(operation, GETparameters = GETparameters, ...)
    if (is.null(request$valid)) {
        return(request)
    }
    if (request$valid) {
        HITs <- emptydf(1, 3, c("HITTypeId", "HITId", "Valid"))
        hit <- strsplit(strsplit(request$xml, "<HITId>")[[1]][2], "</HITId>")[[1]][1]
        if (is.null(hit.type)) {
            hit.type <- strsplit(strsplit(request$xml, "<HITTypeId>")[[1]][2], "</HITTypeId>")[[1]][1]
        }
        HITs[1, ] <- c(hit.type, hit, request$valid)
        if (verbose) {
            if (!is.null(hit.type)) {
                message("HIT ", hit, " created")
            } else if(is.null(hit.type)) {
                message("HIT ", hit, " created (of type ", hit.type,")")
            }
        }
        return(HITs)
    } else if (!request$valid && verbose) {
        warning("Invalid Request")
        return(request)
    }
}
