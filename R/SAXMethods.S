.InitSAXMethods <-
  # Defines S4 classes for use with the SAX parser and specifically to do with the
  # state variable.
  # This also defines methods for the 
function() {  
   require(methods)

   setClass("SAXState",  "VIRTUAL")

   setGeneric("startElement.SAX", function(name, atts, .state = NULL)  standardGeneric("startElement.SAX"))
   setGeneric("endElement.SAX", function(name, .state = NULL) { standardGeneric("endElement.SAX")})
   setGeneric("comment.SAX", function(content,  .state = NULL) { standardGeneric("comment.SAX")})
   setGeneric("text.SAX", function(content,  .state = NULL) { standardGeneric("text.SAX")})
   setGeneric("processingInstruction.SAX", function(target, content, .state = NULL) { standardGeneric("processingInstruction.SAX")})
   setGeneric("entityDeclaration.SAX", function(name, base, sysId, publicId, notationName, .state = NULL) { standardGeneric("entityDeclaration.SAX")})

   setMethod("startElement.SAX", signature(.state = "SAXState"),
                function(name, atts, .state = NULL) .state)
   setMethod("endElement.SAX", signature(.state = "SAXState"),
                function(name, .state = NULL) .state)
   setMethod("comment.SAX", signature(.state = "SAXState"),
                function(content, .state = NULL) .state)
   setMethod("text.SAX", signature(.state = "SAXState"),
                function(content, .state = NULL) .state)
   setMethod("processingInstruction.SAX", signature(.state = "SAXState"),
                function(target, content, .state = NULL) .state)
   setMethod("entityDeclaration.SAX", signature(.state = "SAXState"),
                function(name, base, sysId, publicId, notationName, .state = NULL) .state)

   return(TRUE)
}

genericSAXHandlers  <-
function(include, exclude)
{
 if(!exists("startElement.SAX"))
   stop("You must call .InitSAXMethods before calling genericSAXHandlers()n")


 ans <- list(startElement = startElement.SAX,
             endElement = endElement.SAX,
             comment = comment.SAX,
             text = text.SAX,
             processingInstruction = processingInstruction.SAX,
             entityDeclaration = entityDeclaration.SAX)

 if(!missing(include))
   ans <- ans[include]
 else if(!missing(exclude)) {
   which <- match(exclude, names(ans))
   ans <- ans[-which]
 }

  ans
}


