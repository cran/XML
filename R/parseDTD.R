parseDTD <- 
function(extId, asText=FALSE, name="", isURL=FALSE, error = xmlErrorCumulator())
{
  extId <- as.character(extId)
  if(missing(isURL)) {
    isURL <- length(grep("http://",extId)) | length(grep("ftp://",extId))
  }

  if(missing(name))
     name <- extId

  .oldErrorHandler = setXMLErrorHandler(error)
  on.exit(.Call("RS_XML_setStructuredErrorHandler", .oldErrorHandler), add = TRUE)
  
 .Call("RS_XML_getDTD", as.character(name), as.character(extId),  
                          as.logical(asText), as.logical(isURL), error)
}
