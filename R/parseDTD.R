parseDTD <- 
function(extId, asText=FALSE, name="", isURL=FALSE)
{
  extId <- as.character(extId)
  if(missing(isURL)) {
    isURL <- length(grep("http://",extId)) | length(grep("ftp://",extId))
  }

  if(missing(name))
    name <- extId
 .Call("RS_XML_getDTD", as.character(name), as.character(extId),  
                          as.logical(asText), as.logical(isURL))
}
