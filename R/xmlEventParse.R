xmlEventParse <- 
#
# Parses an XML file using an event parser which calls user-level functions in the
# `handlers' collection when different XML nodes are encountered in the parse stream.
#
# See also xmlParseTree()
#
function(file, handlers=xmlEventHandler(), ignoreBlanks=F, addContext = T, useTagName = T,
           asText = F, trim=T, useExpat = F, isURL=F) 
{
  if(missing(isURL)) { 
        # check if this is a URL or regular file.
    isURL <- length(grep("http://",file)) | length(grep("ftp://",file))
  }

 if(isURL == F & asText == F) {
  if(file.exists(file) == F)
     stop(paste("File", file, "does not exist "))
 }

 handlers <- .Call("RS_XML_Parse", as.character(file), handlers, 
                    as.logical(addContext), as.logical(ignoreBlanks),  
                     as.logical(useTagName), as.logical(asText), as.logical(trim), 
                      as.logical(useExpat))
 return(invisible(handlers))
}
