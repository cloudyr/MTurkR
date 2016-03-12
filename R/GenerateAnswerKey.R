GenerateAnswerKey <-
function (questions, scoring = NULL) {
    answerkey <- newXMLNode("AnswerKey", namespaceDefinitions = "http://mechanicalturk.amazonaws.com/AWSMechanicalTurkDataSchemas/2005-10-01/AnswerKey.xsd")
    for (i in 1:length(questions)) {
        question <- newXMLNode("Question", parent = answerkey)
        id <- newXMLNode("QuestionIdentifier", questions[[i]]$QuestionIdentifier, parent = question)
        addChildren(question, id)
        answeropts <- questions[[i]][!names(questions[[i]]) %in% c("QuestionIdentifier","DefaultScore")]
        for (j in 1:length(answeropts)) {
            opt <- newXMLNode("AnswerOption", parent = question)
            selid <- newXMLNode("SelectionIdentifier", answeropts[[j]]$SelectionIdentifier, parent = opt)
            score <- newXMLNode("AnswerScore", answeropts[[j]]$AnswerScore, parent = opt)
            addChildren(opt, c(selid, score))
        }
        if (!is.null(questions[[i]]$DefaultScore)) {
            default <- newXMLNode("DefaultScore", questions[[i]]$DefaultScore, parent = question)
            addChildren(question, default)
        }
        addChildren(answerkey, question)
    }
    if (!is.null(scoring)) {
        if (!is.null(scoring$Type) && !scoring$Type %in% c("PercentageMapping", "ScaleMapping", "RangeMapping")) {
            stop("'scoring$Type' must be PercentageMapping | ScaleMapping | RangeMapping | NULL")
        } else if(!is.null(scoring$Type)) {
            qualnode <- newXMLNode("QualificationValueMapping", parent = answerkey)
            scorenode <- newXMLNode(scoring$Type, parent = qualnode)
            if (scoring$Type == "RangeMapping") {
                for (i in 1:dim(scoring$RangeMapping)[1]) {
                  scorerange <- newXMLNode("SummedScoreRange", parent = scorenode)
                  lower <- newXMLNode("InclusiveLowerBound", scoring$RangeMapping$InclusiveLowerBound[i], 
                                      parent = scorerange)
                  upper <- newXMLNode("InclusiveUpperBound", scoring$RangeMapping$InclusiveUpperBound[i], 
                                      parent = scorerange)
                  value <- newXMLNode("QualificationValue", scoring$RangeMapping$QualificationValue[i], 
                                      parent = scorerange)
                  addChildren(scorerange, c(lower, upper, value))
                  addChildren(scorenode, scorerange)
                }
                rangenode <- newXMLNode("OutOfRangeQualificationValue", 
                                        scoring$OutOfRangeQualificationValue, parent = scorenode)
                addChildren(scorenode, rangenode)
            } else if (scoring$Type == "PercentageMapping") {
                valuenode <- newXMLNode("MaximumSummedScore", scoring$MaximumSummedScore, parent = scorenode)
                addChildren(scorenode, valuenode)
            } else if (scoring$Type == "ScaleMapping") {
                valuenode <- newXMLNode("ScaleMapping", scoring$SummedScoreMultiplier, parent = scorenode)
                addChildren(scorenode, valuenode)
            }
            addChildren(qualnode, scorenode)
            addChildren(answerkey, qualnode)
        }
    }
    string <- toString.XMLNode(answerkey)
    encoded <- curl_escape(string)
    return(list(xml.parsed = xmlParse(string), string = string, url.encoded = encoded))
}

AnswerKeyTemplate <- function(xml.parsed = NULL){
    qformdf <- as.data.frame.QuestionForm(xml.parsed)$Question
    answerkey <- list()
    for (i in 1:nrow(qformdf)) {
        answerkey[[i]] <- list()
        answerkey[[i]]$QuestionIdentifier <- as.character(qformdf$QuestionIdentifier[i])
        answerspecification <- xmlChildren(xmlChildren(xmlParse(as.character(qformdf$AnswerSpecification[i])))$AnswerSpecification)
        if (!names(answerspecification)=="SelectionAnswer") {
            stop("Question", answerkey[[i]]$QuestionIdentifier, "is not a SelectionAnswer. AnswerKey cannot be generated.")
        }
        opts <- xmlChildren(xmlChildren(answerspecification$SelectionAnswer)$Selections)
        for (j in 1:length(opts)) {
            answerkey[[i]][[j+1]] <- list(
                Text = xmlValue(xmlChildren(opts[[j]])$Text),
                SelectionIdentifier = xmlValue(xmlChildren(opts[[j]])$SelectionIdentifier),
                AnswerScore = NULL)
        }
    }
    return(answerkey)
}
