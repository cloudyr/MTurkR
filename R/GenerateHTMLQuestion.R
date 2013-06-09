GenerateHTMLQuestion <- 
function (character = NULL, file = NULL, frame.height = 450) {
	if (!is.null(character)) 
		html <- character
	else if (!is.null(file))
		html <- paste(readLines(file, warn = FALSE), collapse="")
	string <- paste0(
		"<HTMLQuestion xmlns='http://mechanicalturk.amazonaws.com/AWSMechanicalTurkDataSchemas/2011-11-11/HTMLQuestion.xsd'><HTMLContent><![CDATA[",
		html,"]]></HTMLContent><FrameHeight>",frame.height,"</FrameHeight></HTMLQuestion>")
	invisible(list(	xml.parsed = xmlParse(string),
					string = string, 
					url.encoded = curlEscape(string)))
}
