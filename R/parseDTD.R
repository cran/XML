parseDTD <- 
function(extId, asText=F, name="", isURL=F)
{
  if(missing(isURL)) {
    isURL <- length(grep("http://",extId)) | length(grep("ftp://",extId))
  }

 .Call("RS_XML_getDTD", as.character(name), as.character(extId),  
                          as.logical(asText), as.logical(isURL))
}
