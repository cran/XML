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
function(file, ignoreBlanks = TRUE, handlers = NULL,
           replaceEntities = FALSE, asText = FALSE, trim = TRUE, 
            isURL = FALSE, asTree = FALSE, useInternalNodes = FALSE,
            encoding = character())
{

  if(length(file) > 1) {
   file = paste(file, collapse = "\n")
    if(!missing(asText) && !asText) 
      stop("multiple URIs passed to xmlTreeParse. If this is the content of the file,  specify asText = TRUE")   
   asText = TRUE
 }


  
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

 if(!asText && !isURL)
   file = path.expand(file)

 ans <- .Call("RS_XML_ParseTree", as.character(file), handlers, 
         as.logical(ignoreBlanks), as.logical(replaceEntities),
          as.logical(asText), as.logical(trim), 
           FALSE, FALSE, 
           as.logical(isURL), FALSE, 
           as.logical(useInternalNodes), TRUE, FALSE, FALSE, as.character(encoding))

 if(!missing(handlers) & !as.logical(asTree))
   return(handlers)

 ans
}




parseURI =
function(uri)
{
  u = .Call("R_parseURI", as.character(uri))
  if(u$port == 0)
    u$port = as.integer(NA)

  class(u) = "URI"
  
  u
}  

setOldClass("URI")
