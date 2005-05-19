htmlTreeParse <- 
#
# HTML parser that reads the entire `document' tree into memory
# and then converts it to an R/S object. 
# Uses the libxml from Daniel Veillard at W3.org. 
#
# asText  treat the value of file as XML text, not the name of a file containing
#       the XML text, and parse that.
# See also xml
#
function(file, ignoreBlanks = TRUE, handlers=NULL,
           replaceEntities=FALSE, asText=FALSE, trim=TRUE, 
            isURL=FALSE, asTree = FALSE, useInternalNodes = FALSE)
{
  if(missing(isURL)) {
    isURL <- length(grep("http://",file)) | length(grep("ftp://",file))
  }

    # check whether we are treating the file name as
    # a) the XML text itself, or b) as a URL.
    # Otherwise, check if the file exists and report an error.
 if(asText == FALSE & isURL == FALSE) {
  if(file.exists(file) == FALSE)
     stop(paste("File", file, "does not exist "))
 }

# ans <- .Call("RS_XML_HtmlParseTree", as.character(file), handlers, 
#         as.logical(ignoreBlanks), as.logical(replaceEntities),
#          as.logical(asText), as.logical(trim), as.logical(isURL))

 ans <- .Call("RS_XML_ParseTree", as.character(file), handlers, 
         as.logical(ignoreBlanks), as.logical(replaceEntities),
          as.logical(asText), as.logical(trim), 
           FALSE, FALSE, 
           as.logical(isURL), FALSE, 
           as.logical(useInternalNodes), TRUE)

 if(!missing(handlers) & !as.logical(asTree))
   return(handlers)

 ans
}
