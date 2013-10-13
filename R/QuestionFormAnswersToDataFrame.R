QuestionFormAnswersToDataFrame <-
function (xml = NULL, xml.parsed = NULL) {
    if(!is.null(xml)) 
        xml.parsed <- xmlParse(xml)
    answers <- xpathApply(xml.parsed, "//Answer")
    # estimate length of dataframe:
	total <- length(answers)
    extractQuestionFormAnswersElement <- function(i) {
        z <- xmlChildren(xmlParse(xmlValue(i)))$QuestionFormAnswers
        removeXMLNamespaces(z, all = TRUE)
        return(z)
    }
    z <- extractQuestionFormAnswersElement(answers[[1]])
    questions <- xmlChildren(z)
    iterations <- total * length(questions)
    # create dataframe
	values <- as.data.frame(matrix(nrow = iterations, ncol = 10))
    names(values) <- c(	"AssignmentId", "WorkerId", "HITId", "QuestionIdentifier", 
                        "FreeText", "SelectionIdentifier", "OtherSelectionField", 
                        "UploadedFileKey", "UploadedFileSizeInBytes", "Combined.Answers")
	convertxml <- function(node){
        questions <- xmlChildren(extractQuestionFormAnswersElement(node))
		out <- as.data.frame(matrix(nrow = length(questions), ncol = 10))
		names(out) <- names(values)
		out$AssignmentId <- xmlValue(xpathApply(node, "../AssignmentId")[[1]])
        out$WorkerId <- xmlValue(xpathApply(node, "../WorkerId")[[1]])
        out$HITId <- xmlValue(xpathApply(node, "../HITId")[[1]])
        for(z in 1:length(questions)){
			if(length(xmlChildren(questions[[z]])$QuestionIdentifier) == 1) 
				out$QuestionIdentifier[z] <- xmlValue(xmlChildren(questions[[z]])$QuestionIdentifier)
			if(length(xmlChildren(questions[[z]])$FreeText) == 1) {
				out$FreeText[z] <- xmlValue(xmlChildren(questions[[z]])$FreeText)
				out$Combined.Answers[z] <- xmlValue(xmlChildren(questions[[z]])$FreeText)
			}
			else if(length(xmlChildren(questions[[z]])$UploadedFileKey) == 1) {
				out$UploadedFileKey[z] <- xmlValue(xmlChildren(questions[[z]])$UploadedFileKey)
				out$UploadedFileSizeInBytes[z] <- xmlValue(xmlChildren(questions[[z]])$UploadedFileSizeInBytes)
				out$Combined.Answers[z] <- paste(out$UploadedFileKey[z], 
					out$UploadedFileSizeInBytes[z], sep = ":")
			}
			else if (length(xmlChildren(questions[[z]])$SelectionIdentifier) == 1) {
				out$SelectionIdentifier[z] <- xmlValue(xmlChildren(questions[[z]])$SelectionIdentifier)
				out$Combined.Answers[z] <- xmlValue(xmlChildren(questions[[z]])$SelectionIdentifier)
				if(length(xmlChildren(questions[[1]])$OtherSelectionField) == 1) {
					multiple <- paste(out$SelectionIdentifier[z], 
								xmlValue(xmlChildren(questions[[z]])$OtherSelectionField), 
								sep = ";")
					out$Combined.Answers[z] <- multiple
					rm(multiple)
				}
			}
			else if(length(xmlChildren(questions[[z]])$SelectionIdentifier) > 1) {
				multiple <- ""
				for(j in 1:length(xmlChildren(questions[[z]])$SelectionIdentifier)) {
					multiple <- paste(multiple,
								xmlValue(xmlChildren(xmlChildren(questions[[z]])[j]$QuestionIdentifier)$text),
								sep = ";")
				}
				if(length(xmlChildren(questions[[z]])$OtherSelectionField) == 1) 
					multiple <- paste(multiple,
								xmlValue(xmlChildren(questions[[z]])$OtherSelectionField), 
								sep = ";")
				out$SelectionIdentifier[z] <- multiple
				out$Combined.Answers[z] <- multiple
				rm(multiple)
			}
			else if(length(xmlChildren(questions[[z]])$OtherSelectionField) == 1) {
				out$OtherSelectionField[z] <- xmlValue(xmlChildren(questions[[z]])$OtherSelectionField)
				out$Combined.Answers[z] <- xmlValue(xmlChildren(questions[[z]])$OtherSelectionField)
			}
        }
		return(out)
	}
	values <- do.call(rbind,lapply(answers,FUN=convertxml))
	return(values)
}
