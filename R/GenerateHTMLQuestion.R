GenerateHTMLQuestion <- 
function (character = NULL, file = NULL, frame.height = 450) {
    if (!is.null(character)) {
        html <- paste0(character, collapse = "\n")
    } else if (!is.null(file)) {
        html <- paste0(readLines(file, warn = FALSE), collapse="\n")
    }
    string <- paste0(
        "<HTMLQuestion xmlns='http://mechanicalturk.amazonaws.com/AWSMechanicalTurkDataSchemas/2011-11-11/HTMLQuestion.xsd'><HTMLContent><![CDATA[",
        html,"]]></HTMLContent><FrameHeight>",frame.height,"</FrameHeight></HTMLQuestion>")
    return(structure(list(xml.parsed = xmlParse(string),
                          string = string, 
                          url.encoded = curl_escape(string)), 
                     class='HTMLQuestion'))
}
