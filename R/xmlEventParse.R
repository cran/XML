xmlEventParse <- 
#
# Parses an XML file using an event parser which calls user-level functions in the
# `handlers' collection when different XML nodes are encountered in the parse stream.
#
# See also xmlParseTree()
#
function(file, handlers=xmlEventHandler(), ignoreBlanks=FALSE, addContext = TRUE,
          useTagName = TRUE, asText = FALSE, trim=TRUE, useExpat = FALSE, isURL=FALSE) 
{
  if(missing(isURL)) { 
        # check if this is a URL or regular file.
    isURL <- length(grep("http://",file)) | length(grep("ftp://",file))
  }

 if(isURL == FALSE & asText == FALSE) {
  if(file.exists(file) == FALSE)
     stop(paste("File", file, "does not exist "))
 }

 handlers <- .Call("RS_XML_Parse", as.character(file), handlers, 
                    as.logical(addContext), as.logical(ignoreBlanks),  
                     as.logical(useTagName), as.logical(asText), as.logical(trim), 
                      as.logical(useExpat))
 return(invisible(handlers))
}
