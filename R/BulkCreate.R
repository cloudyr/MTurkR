BulkCreate <- function(questions, annotation, verbose = FALSE, ...) {
    HITs <- list()
    if (length(annotation) %in% c(1, length(questions))) {
        annotation <- rep(annotation[1], length(questions))
    }
    arguments <- list(...)
    if (!"hit.type" %in% names(arguments)) {
        hittypeargs <- unique(c(names(formals(MTurkR::RegisterHITType)), 
                                names(formals(MTurkR::request))))
        hittypeargs <- hittypeargs[hittypeargs != "..."]
        register <- do.call("RegisterHITType", 
                            arguments[names(arguments) %in% hittypeargs])
        if (!as.logical(register$Valid)) {
            stop("Could not RegisterHITType(), check parameters")
        }
    }
    for (i in 1:length(questions)) {
        if (inherits(questions[[i]], "ExternalQuestion")) {
            thisq <- questions[[i]]$string
        } else if (inherits(questions[[i]], "HTMLQuestion")) {
            thisq <- questions[[i]]$string
        } else {
            thisq <- questions[[i]]
        }
        if (!"hit.type" %in% names(arguments)) {
            HITs[[i]] <- CreateHIT(question = thisq, annotation = annotation[i], 
                                   hit.type = register$HITTypeId, verbose = verbose, ...)
        } else {
            HITs[[i]] <- CreateHIT(question = thisq, annotation = annotation[i], verbose = verbose, ...)
        }
    }
    return(HITs)
}

BulkCreateFromTemplate <- function(template, input, annotation, type = "HTMLQuestion", frame.height = 450, verbose = FALSE, ...) {
    if (type == "HTMLQuestion") {
        if(file.exists(template)) {
            template <- GenerateHTMLQuestion(file = template, frame.height = frame.height)$string
        } else {
            template <- GenerateHTMLQuestion(character = template, frame.height = frame.height)$string
        }
    }
    if (length(annotation) != 1) {
        annotation <- rep(annotation[1], nrow(input))
    }
    input[] <- lapply(input, as.character)
    questions <- GenerateHITsFromTemplate(template, input, filenames = NULL, write.files = FALSE)
    BulkCreate(questions = questions, annotation = annotation, verbose = verbose, ...)
}

BulkCreateFromURLs <- function(url, frame.height = 450, annotation, verbose = FALSE, ...) {
    if (length(annotation) == 1) {
        annotation <- rep(annotation[1], length(url))
    }
    questions <- lapply(url, GenerateExternalQuestion, frame.height = frame.height)
    BulkCreate(questions = questions, annotation = annotation, verbose = verbose, ...)
}


BulkCreateFromHITLayout <- function(hitlayoutid, input, annotation, verbose = FALSE, ...) {
    HITs <- list()
    if (length(annotation) == 1) {
        annotation <- rep(annotation[1], nrow(input))
    }
    a <- list(...)
    if (!"hit.type" %in% names(a)) {
        hittypeargs <- unique(c(names(formals(MTurkR::RegisterHITType)), 
                                names(formals(MTurkR::request))))
        hittypeargs <- hittypeargs[hittypeargs != "..."]
        register <- do.call("RegisterHITType", a[names(a) %in% hittypeargs])
        if (!as.logical(register$Valid)) {
            stop("Could not RegisterHITType(), check parameters")
        }
    }
    input[] <- lapply(input, as.character)
    for (i in 1:nrow(input)) {
        if (!"hit.type" %in% names(a)) {
            HITs[[i]] <- CreateHIT(hitlayoutid = hitlayoutid, 
                               hitlayoutparameters = 
                                 GenerateHITLayoutParameter(names = names(input), 
                                                            values = unlist(input[i,,drop=TRUE])), 
                               annotation = annotation[i], 
                               hit.type = register$HITTypeId, verbose = verbose, ...)
        } else {
            HITs[[i]] <- CreateHIT(hitlayoutid = hitlayoutid, 
                               hitlayoutparameters = 
                                 GenerateHITLayoutParameter(names = names(input), 
                                                            values = unlist(input[i,,drop=TRUE])), 
                               annotation = annotation[i], verbose = verbose, ...)
        }
    }
    return(HITs)
}
