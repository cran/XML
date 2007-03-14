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
           isURL=FALSE, asTree = FALSE, addAttributeNamespaces = FALSE,
           useInternalNodes = FALSE, isSchema = FALSE,
           fullNamespaceInfo = FALSE)
{

  if(length(file) > 1) {
    file = paste(file, collapse = "\n")
    if(!missing(asText) && !asText) 
      stop("multiple URIs passed to xmlTreeParse. If this is the content of the file,  specify asText = TRUE")
    asText = TRUE
  }
  
  if(missing(isURL)) 
    isURL <- length(grep("^http://", file)) | length(grep("^ftp://",file)) | length(grep("^file://", file))


  oldValidate = xmlValidity()
  xmlValidity(validate)
  on.exit(xmlValidity(oldValidate))
  
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

 if(asText && length(file) > 1)
   file = paste(file, collapse = "\n")

 ans <- .Call("RS_XML_ParseTree", as.character(file), handlers, 
              as.logical(ignoreBlanks), as.logical(replaceEntities),
              as.logical(asText), as.logical(trim), as.logical(validate), as.logical(getDTD),
              as.logical(isURL), as.logical(addAttributeNamespaces),
              as.logical(useInternalNodes), FALSE, as.logical(isSchema),
              as.logical(fullNamespaceInfo))


 if(!missing(handlers) & !as.logical(asTree))
   return(handlers)

 ans
}




xmlValidity =
function(val = integer(0))
{
  .Call("RS_XML_getDefaultValiditySetting", as.integer(val))
}

