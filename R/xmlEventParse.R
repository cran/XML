xmlEventParse <- 
#
# Parses an XML file using an event parser which calls user-level functions in the
# `handlers' collection when different XML nodes are encountered in the parse stream.
#
# See also xmlParseTree()
#
function(file, handlers=xmlEventHandler(), ignoreBlanks=FALSE, addContext = TRUE,
          useTagName = TRUE, asText = FALSE, trim=TRUE, useExpat = FALSE, isURL=FALSE, state = NULL,
          replaceEntities = TRUE) 
{
  if(libxmlVersion()$major < 2 && !is.character(file))
    stop("Without libxml2, the source of the XML can only be specified as a URI.")
  
  if(inherits(file, "connection")) {
    if(!isOpen(file)) {
      open(file, "r")
      on.exit(close(file))
    }
  } else if(is.function(file)) {
      # call with -1 to allow us to close the connection
      # if necessary.
    on.exit(file(-1))
  } else {
   if(missing(isURL)) { 
        # check if this is a URL or regular file.
     isURL <- length(grep("http://",file)) | length(grep("ftp://",file)) | length(grep("file://",file))
   }

   if(isURL == FALSE & asText == FALSE) {
    if(file.exists(file) == FALSE)
     stop(paste("File", file, "does not exist "))
   }
   file = as.character(file)
 }

 state <- .Call("RS_XML_Parse", file, handlers, 
                    as.logical(addContext), as.logical(ignoreBlanks),  
                     as.logical(useTagName), as.logical(asText), as.logical(trim), 
                      as.logical(useExpat), state, as.logical(replaceEntities))
 if(!is.null(state))
   return(state)
 else
   return(invisible(handlers))
}
