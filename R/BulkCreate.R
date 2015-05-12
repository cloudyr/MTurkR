BulkCreate <- function(questions, annotation, ...) {
    HITs <- list()
    if(length(annotation) != 1)
        annotation <- rep(annotation[1], length(parameters))
    for(i in 1:length(questions)) {
        if(inherits(questions[[i]], "ExternalQuestion")) {
            thisq <- questions[[i]]$string
        } else if(inherits(questions[[i]], "HTMLQuestion")) {
            thisq <- questions[[i]]$string
        } else {
            thisq <- questions[[i]]
        }
        # this will be very slow if a HITTypeId isn't specified in `...`; need to fix that
        HITs[[i]] <- CreateHIT(question = thisq, annotation = annotation[i], ...)
    }
    return(HITs)
}

BulkCreateFromTemplate <- function(template, input, type = "HTMLQuestion", annotation, ...) {
    if(type == "HTMLQuestion") {
        if(file.exists(template)) {
            template <- GenerateHTMLQuestion(file = template)$string
        } else {
            template <- GenerateHTMLQuestion(character = template)$string
        }
    }
    if(length(annotation) != 1)
        annotation <- rep(annotation[1], length(parameters))
    questions <- GenerateHITsFromTemplate(template, input, filenames = NULL, write.files = FALSE)
    BulkCreate(questions = questions, annotation = annotation[i], ...)
}

BulkCreateFromHITLayout <- function(hitlayoutid, parameters, annotation, ...) {
    HITs <- list()
    p <- lapply(parameters, function(x) GenerateHITLayoutParameter(values = x))
    if(length(annotation) != 1)
        annotation <- rep(annotation[1], length(parameters))
    for(i in 1:length(p)) {
        HITs[[i]] <- CreateHIT(hitlayoutid = hitlayoutid, hitlayoutparameters = p[[i]], annotation = annotation[i], ...)
    }
    return(HITs)
}
