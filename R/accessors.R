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
function(x, skip=T)
{
  a <- x$children[[1]]
#  a <- x$children
  if(skip & inherits(a, "XMLComment")) {
     which <- sapply(x$children, function(x) !inherits(x, "XMLComment"))
     if(any(which)) {
       which <- (1:length(x$children))[which]
       a <- x$children[[which[1]]]
     } 
  }

 a
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
function(x)
{
 UseMethod("xmlValue")
}

xmlValue.XMLNode <- 
function(x)
{
 x$value
}

xmlValue.XMLTextNode <- 
function(x)
{
 x$value
}

xmlValue.XMLCDataNode <- 
function(x)
{
 x$value
}

xmlValue.XMLProcessingInstruction <- 
function(x)
{
 x$value
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
