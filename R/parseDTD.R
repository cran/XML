parseDTD <- 
function(extId, asText = FALSE, name = "", isURL = FALSE, error = xmlErrorCumulator())
{
  extId <- as.character(extId)
  if(missing(isURL)) {
    isURL <- length(grep("http://",extId))  > 0 ||  length(grep("ftp://",extId)) > 0
  }

  if(missing(name))
     name <- extId

  .oldErrorHandler = setXMLErrorHandler(error)
  on.exit(.Call("RS_XML_setStructuredErrorHandler", .oldErrorHandler, PACKAGE = "XML"), add = TRUE)

  if(asText) {
    f = tempfile()
    cat(extId, "\n", file = f)
    extId = f
    asText = FALSE
  }
  
 .Call("RS_XML_getDTD", as.character(name), as.character(extId),  
                          as.logical(asText), as.logical(isURL), error, PACKAGE = "XML")
}
