xmlRoot <-
function(x, ...)
{
 UseMethod("xmlRoot")
}

xmlRoot.XMLDocument <-
function(x, ...)
{
#  x$children[[1]]
# x$doc

  xmlRoot(x$doc, ...)
}

xmlRoot.XMLDocumentContent <-
function(x, ...)
{
  args <- list(...)
  if("skip" %in% names(args))
   skip <- args[["skip"]]
  else
   skip <- TRUE

  a <- x$children[[1]]
  if(skip & inherits(a, "XMLComment")) {
     which <- sapply(x$children, function(x) !inherits(x, "XMLComment"))
     if(any(which)) {
       which <- (1:length(x$children))[which]
       a <- x$children[[which[1]]]
     } 
  }

 a
}


xmlRoot.HTMLDocument <-
function(x, ...)
{
   x$children[[1]]
}

xmlApply <-
function(X, FUN, ...)
{
  UseMethod("xmlApply")
}

xmlSApply <-
function(X, FUN, ...)
{
  UseMethod("xmlSApply")
}

xmlApply.XMLNode <- 
function(X, FUN, ...) { 
  lapply(xmlChildren(X), FUN, ...) 
} 


xmlApply.XMLDocument <-
function(X, FUN, ...)
{
  xmlApply(xmlRoot(X), FUN, ...)
}

xmlSApply.XMLDocument <-
function(X, FUN, ...)
{
  xmlSApply(xmlRoot(X), FUN, ...)
}


xmlSApply.XMLNode <- 
function(X, FUN, ...) { 
  sapply(xmlChildren(X), FUN, ...) 
} 

xmlApply.XMLDocumentContent <-
function(X, FUN, ...)
{
  xmlSApply(X$children, FUN, ...)
}

xmlSApply.XMLDocumentContent <-
function(X, FUN, ...)
{
  xmlSApply(X$children, FUN, ...)
}


xmlValue <- 
function(x, ignoreComments = FALSE)
{
 UseMethod("xmlValue")
}

xmlValue.XMLNode <- 
function(x, ignoreComments = FALSE)
{
 if(xmlSize(x) == 1) # && (inherits(x[[1]], "XMLTextNode"))
    return(xmlValue(x[[1]], ignoreComments))

 x$value
}

xmlValue.XMLTextNode <- 
function(x, ignoreComments = FALSE)
{
 x$value
}

xmlValue.XMLComment <- 
function(x, ignoreComments = FALSE)
{
 if(ignoreComments)
   return("")

 x$value
}

xmlValue.XMLCDataNode <- 
function(x, ignoreComments = FALSE)
{
 x$value
}

xmlValue.XMLProcessingInstruction <- 
function(x, ignoreComments = FALSE)
{
 x$value
}


xmlNamespaceDefinitions <-
function(x)
{
  unclass(x)$namespaceDefinitions
}

xmlNamespace <-
function(x)
{
 UseMethod("xmlNamespace")
}


xmlNamespace.XMLNode <-
function(x)
{
  x$namespace
}


xmlGetAttr <-
function(node, name, default = NULL, converter = NULL)
{
  a <- xmlAttrs(node)
  if(is.null(a) || is.na(match(name, names(a)))) 
    return(default)

  if(!is.null(converter))
    converter(a[[name]])
  else
    a[[name]]
}  

