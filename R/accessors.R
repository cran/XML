if(!exists("Sys.setenv", baseenv()))
    Sys.setenv <- get("Sys.putenv", "package:base")


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


getNextSibling =
  # Access the next field in the xmlNodePtr object.
  # not exported.
function(x)
{
  if(!inherits(x, "XMLInternalNode"))
    stop("can only operate on an internal node")

  .Call("RS_XML_getNextSibling", x)
}

xmlNamespaceDefinitions <-
function(x, addNames = TRUE, recursive = FALSE)
{
  UseMethod("xmlNamespaceDefinitions")
}

xmlNamespaceDefinitions.XMLInternalDocument =
function(x, addNames = TRUE, recursive = FALSE)
{
  r = xmlRoot(x)
  while(!is.null(r) && !inherits(r, "XMLInternalElementNode")) 
     r = getNextSibling(r)

  if(is.null(r))
    return(NULL)
  
  xmlNamespaceDefinitions(r)
}

xmlNamespaceDefinitions.XMLNode =
  function(x, addNames = TRUE, recursive = FALSE) {
    ans = unclass(x)$namespaceDefinitions


    if(recursive == TRUE) {
                   #      warning("recursive facility not yet implemented.")
      f = function(node) {
            if(!inherits(node, "XMLNode") || xmlName(node) == "")
              return(FALSE)
            ans <<- append(ans, unclass(node)$namespaceDefinitions)
            xmlApply(node, f)
          }
      xmlApply(x, f)
    }

    if(addNames && length(ans) && length(names(ans)) == 0)
        names(ans) = sapply(ans, function(x) x$id)
    
    ans
  }

xmlNamespaceDefinitions.XMLInternalNode =
  function(x, addNames = TRUE, recursive = FALSE) {
    ans = .Call("RS_XML_internalNodeNamespaceDefinitions", x, as.logical(recursive))
    if(addNames && length(ans) > 0)
      names(ans) = sapply(ans, function(x) x$id)
    ans
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

