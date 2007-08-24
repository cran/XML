
xmlTreeParse <- 
#
# XML parser that reads the entire `document' tree into memory
# and then converts it to an R/S object. 
# Uses the libxml from Daniel Veillard at W3.org. 
#
# asText  treat the value of file as XML text, not the name of a file containing
#       the XML text, and parse that.
#
#
function(file, ignoreBlanks = TRUE, handlers=NULL,
           replaceEntities=FALSE, asText=FALSE, trim=TRUE, validate=FALSE, getDTD=TRUE,
           isURL=FALSE, asTree = FALSE, addAttributeNamespaces = FALSE,
           useInternalNodes = FALSE, isSchema = FALSE,
           fullNamespaceInfo = FALSE, encoding = character(),
           useDotNames = length(grep("^\\.", names(handlers))) > 0,  # will be switched to TRUE in the future.
           xinclude = TRUE, addFinalizer = TRUE)
{

  if(length(file) > 1) {
    file = paste(file, collapse = "\n")
    if(!missing(asText) && !asText) 
      stop(structure(list(message = "multiple URLs passed to xmlTreeParse. If this is the content of the file, specify asText = TRUE"),
                     class = c("MultipleURLError", "XMLParserError", "simpleError", "error", "condition")))
    asText = TRUE
  }
  
  if(missing(isURL)) 
    isURL <- length(grep("^http://", file)) | length(grep("^ftp://",file)) | length(grep("^file://", file))


  checkHandlerNames(handlers, "DOM")


  if(missing(fullNamespaceInfo) && inherits(handlers, "RequiresNamespaceInfo"))
    fullNamespaceInfo = TRUE
  

  oldValidate = xmlValidity()
  xmlValidity(validate)
  on.exit(xmlValidity(oldValidate))
  
    # check whether we are treating the file name as
    # a) the XML text itself, or b) as a URL.
    # Otherwise, check if the file exists and report an error.
 if(isURL == FALSE) {
  if(file.exists(file) == FALSE)
    if(!missing(asText) && asText == FALSE) {
     e = simpleError(paste("File", file, "does not exist"))
     class(e) = c("FileNotFound", class(e))
     stop(e)
    }
    else
     asText <- TRUE
 }

 if(asText && length(file) > 1)
   file = paste(file, collapse = "\n")

 old = setEntitySubstitution(replaceEntities)
 on.exit(setEntitySubstitution(old))

  if(asText && length(grep("^\\s*<", file, perl = TRUE)) == 0) {
    e = simpleError(paste(file, " does not seem to be XML, nor to identify a file name"))
    class(e) = c("XMLInputError", class(e))
    stop(e)
  }
  

 if(!is.logical(xinclude)) {
   # if(is(xinclude, "numeric"))
   #  xinclude = bitlist(xinclude) # see bitList.R
   # else
     xinclude = as.logical(xinclude)
 }

  
 ans <- .Call("RS_XML_ParseTree", as.character(file), handlers, 
              as.logical(ignoreBlanks), as.logical(replaceEntities),
              as.logical(asText), as.logical(trim), as.logical(validate), as.logical(getDTD),
              as.logical(isURL), as.logical(addAttributeNamespaces),
              as.logical(useInternalNodes), FALSE, as.logical(isSchema),
              as.logical(fullNamespaceInfo), as.character(encoding), as.logical(useDotNames),
              xinclude)

  if(!missing(handlers) & !as.logical(asTree))
    return(handlers)

  if(inherits(ans, "XMLInternalDocument"))
    addDocFinalizer(ans, addFinalizer)

  ans
}




xmlValidity =
function(val = integer(0))
{
  .Call("RS_XML_getDefaultValiditySetting", as.integer(val))
}



