xmlTreeParse <- 
#
# XML parser that reads the entire `document' tree into memory
# and then converts it to an R/S object. 
# Uses the libxml from Daniel Veillard at W3.org. 
#
# asText  treat the value of file as XML text, not the name of a file containing
#       the XML text, and parse that.
# See also xml
#
function(file, ignoreBlanks = TRUE, handlers=NULL,
           replaceEntities=FALSE, asText=FALSE, trim=TRUE, validate=FALSE, getDTD=TRUE,
           isURL=FALSE, asTree = FALSE, addAttributeNamespaces = FALSE)
{
  if(missing(isURL)) {
    isURL <- length(grep("^http://",file)) | length(grep("^ftp://",file)) | length(grep("^file://", file))
  }

    # check whether we are treating the file name as
    # a) the XML text itself, or b) as a URL.
    # Otherwise, check if the file exists and report an error.
 if(isURL == FALSE) {
  if(file.exists(file) == FALSE)
    if(!missing(asText) && asText == FALSE)
     stop(paste("File", file, "does not exist "))
    else
     asText <- TRUE
 }


 ans <- .Call("RS_XML_ParseTree", as.character(file), handlers, 
              as.logical(ignoreBlanks), as.logical(replaceEntities),
              as.logical(asText), as.logical(trim), as.logical(validate), as.logical(getDTD),
              as.logical(isURL), as.logical(addAttributeNamespaces))

 if(!missing(handlers) & !as.logical(asTree))
   return(handlers)

 ans
}
