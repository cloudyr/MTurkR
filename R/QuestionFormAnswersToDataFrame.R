QuestionFormAnswersToDataFrame <-
function (xml = NULL, xml.parsed = NULL) 
{
    if (!is.null(xml)) 
        xml.parsed <- xmlParse(xml)
    answers <- xpathApply(xml.parsed, "//Answer")
    total <- length(answers)
    extractQuestionFormAnswersElement <- function(i) {
        z <- xmlChildren(xmlParse(xmlValue(answers[[i]])))$QuestionFormAnswers
        removeXMLNamespaces(z, all = TRUE)
        return(z)
    }
    z <- extractQuestionFormAnswersElement(1)
    questions <- xmlChildren(z)
    iterations <- total * length(questions)
    values <- as.data.frame(matrix(nrow = iterations, ncol = 10))
    names(values) <- c("AssignmentId", "WorkerId", "HITId", "QuestionIdentifier", 
        "FreeText", "SelectionIdentifier", "OtherSelectionField", 
        "UploadedFileKey", "UploadedFileSizeInBytes", "Combined.Answers")
    j <- 1 	# j iterates rows of the original input data
			# i iterates rows of the output dataframe
	z <- 0 	# z iterates through answers within each row of the original data
	for (i in 1:iterations) {
        z <- z + 1
        values$AssignmentId[i] <- xmlValue(xpathApply(answers[[j]], 
            "../AssignmentId")[[1]])
        values$WorkerId[i] <- xmlValue(xpathApply(answers[[j]], 
            "../WorkerId")[[1]])
        values$HITId[i] <- xmlValue(xpathApply(answers[[j]], 
            "../HITId")[[1]])
        questions <- xmlChildren(extractQuestionFormAnswersElement(j))
        if (length(xmlChildren(questions[[z]])$QuestionIdentifier) == 1) 
            values$QuestionIdentifier[i] <- xmlValue(xmlChildren(questions[[z]])$QuestionIdentifier)
        if (length(xmlChildren(questions[[z]])$FreeText) == 1) {
            values$FreeText[i] <- xmlValue(xmlChildren(questions[[z]])$FreeText)
            values$Combined.Answers[i] <- xmlValue(xmlChildren(questions[[z]])$FreeText)
        }
        else if (length(xmlChildren(questions[[z]])$UploadedFileKey) == 1) {
            values$UploadedFileKey[i] <- xmlValue(xmlChildren(questions[[z]])$UploadedFileKey)
            values$UploadedFileSizeInBytes[i] <- xmlValue(xmlChildren(questions[[z]])$UploadedFileSizeInBytes)
            values$Combined.Answers[i] <- paste(values$UploadedFileKey[i], 
                values$UploadedFileSizeInBytes, sep = ":")
        }
        else if (length(xmlChildren(questions[[z]])$SelectionIdentifier) == 1) {
            values$SelectionIdentifier[i] <- xmlValue(xmlChildren(questions[[z]])$SelectionIdentifier)
            values$Combined.Answers[i] <- xmlValue(xmlChildren(questions[[z]])$SelectionIdentifier)
            if (length(xmlChildren(questions[[1]])$OtherSelectionField) == 
                1) {
                multiple <- paste(values$SelectionIdentifier[i], 
                  xmlValue(xmlChildren(questions[[z]])$OtherSelectionField), 
                  sep = ";")
                values$Combined.Answers[i] <- multiple
            }
        }
        else if (length(xmlChildren(questions[[z]])$SelectionIdentifier) > 1) {
            multiple <- ""
            for (j in 1:length(xmlChildren(questions[[z]])$SelectionIdentifier)) {
                multiple <- paste(multiple, xmlValue(xmlChildren(xmlChildren(questions[[z]])[i]$QuestionIdentifier)$text), 
                  sep = ";")
            }
            if (length(xmlChildren(questions[[z]])$OtherSelectionField) == 1) 
                multiple <- paste(multiple, xmlValue(xmlChildren(questions[[z]])$OtherSelectionField), 
                  sep = ";")
            values$SelectionIdentifier[i] <- multiple
            values$Combined.Answers[i] <- multiple
        }
        else if (length(xmlChildren(questions[[z]])$OtherSelectionField) == 1) {
            values$OtherSelectionField[i] <- xmlValue(xmlChildren(questions[[z]])$OtherSelectionField)
            values$Combined.Answers[i] <- xmlValue(xmlChildren(questions[[z]])$OtherSelectionField)
        }
        if (i == (j * length(questions))) {
            j <- j + 1
            z <- 0
        }
    }
    return(values)
}
