QuestionFormToDataFrame <-
function (xml = NULL, xml.parsed = NULL) 
{
    if (!is.null(xml)) 
        xml.parsed <- xmlParse(xml)
    qform <- xmlChildren(xmlChildren(xml.parsed)$QuestionForm)
    #overview <- qform[names(qform) == "Overview"]
    questions <- qform[names(qform) == "Question"]
    #ov <- data.frame(matrix(nrow = length(overview), ncol = ))
    qdf <- data.frame(matrix(nrow = length(questions), ncol = 5))
    names(qdf) <- c("QuestionIdentifier", "DisplayName", "IsRequired", 
        "QuestionContent", "AnswerSpecification")
    for (i in 1:length(questions)) {
        qdf$QuestionIdentifier[i] <- xmlValue(xmlChildren(questions[[i]])$QuestionIdentifier)
        qdf$DisplayName[i] <- xmlValue(xmlChildren(questions[[i]])$DisplayName)
        qdf$IsRequired[i] <- xmlValue(xmlChildren(questions[[i]])$IsRequired)
        qdf$QuestionContent[i] <- toString.XMLNode(xmlChildren(questions[[i]])$QuestionContent)
        qdf$AnswerSpecification[i] <- toString.XMLNode(xmlChildren(questions[[i]])$AnswerSpecification)
    }
    #return(list(Overview = ov, Questions = qdf))
	return(list(Questions = qdf))
}
