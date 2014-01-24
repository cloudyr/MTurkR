AnswerKeyToDataFrame <-
function (xml = NULL, xml.parsed = NULL) {
    if(!is.null(xml)) 
        xml.parsed <- xmlParse(xml)
    nodes <- xmlChildren(xmlChildren(xml.parsed)$AnswerKey)
    answerkey <- data.frame(matrix(nrow = length(strsplit(xml,'/AnswerOption')[[1]])-1,
                                   ncol = 3))
    names(answerkey) <- c("QuestionIdentifier", "SelectionIdentifier", 
        "AnswerScore")
    k <- 1
    for(i in 1:length(nodes[names(nodes) == "Question"])) {
        question <- xmlChildren(nodes[names(nodes) == "Question"][[i]])
        qid <- xmlValue(question$QuestionIdentifier)
        answeroptions <- question[names(question) == "AnswerOption"]
        for(j in 1:length(answeroptions)) {
            answerkey$QuestionIdentifier[k] <- qid
            answerkey$SelectionIdentifier[k] <- xmlValue(xmlChildren(answeroptions[[j]])$SelectionIdentifier)
            answerkey$AnswerScore[k] <- xmlValue(xmlChildren(answeroptions[[j]])$AnswerScore)
            k <- k + 1
        }
    }
    if(!is.null(nodes$QualificationValueMapping)) {
        map <- xmlChildren(nodes$QualificationValueMapping)
        mapping <- list()
        if("PercentageMapping" %in% names(map)) {
            mapping$Type <- "PercentageMapping"
            mapping$MaximumSummedScore <- xmlValue(xmlChildren(map$PercentageMapping)$MaximumSummedScore)
        }
        else if("ScaleMapping" %in% names(map)) {
            mapping$Type <- "ScaleMapping"
            mapping$SummedScoreMultiplier <- xmlValue(xmlChildren(map$PercentageMapping)$SummedScoreMultiplier)
        }
        else if("RangeMapping" %in% names(map)) {
            mapping$Type <- "RangeMapping"
            ranges.xml <- xmlChildren(map$RangeMapping)
            scoreranges <- ranges.xml[names(ranges.xml) == "SummedScoreRange"]
            mapping$Ranges <- data.frame(matrix(nrow=length(scoreranges), ncol=3))
            names(mapping$Ranges) <- c("InclusiveLowerBound", 
                "InclusiveUpperBound", "QualificationValue")
            for(i in 1:length(scoreranges)) {
                mapping$Ranges[i, ] <- c(xmlValue(xmlChildren(scoreranges[[i]])$InclusiveLowerBound), 
                  xmlValue(xmlChildren(scoreranges[[i]])$InclusiveUpperBound), 
                  xmlValue(xmlChildren(scoreranges[[i]])$QualificationValue))
            }
            mapping$OutOfRangeQualificationValue <- xmlValue(ranges.xml$OutOfRangeQualificationValue)
        }
        return(list(Questions = answerkey, Scoring = mapping))
    }
    else
		return(list(Questions = answerkey))
}
