xmlNode <-
function(name, ..., attrs=NULL, namespace="")
{
  kids <- lapply(list(...), asXMLNode)
  node <- list(name = name, attributes = attrs, children = kids, namespace=namespace)
  class(node) <- c("XMLNode")

  node
}

xmlTextNode <- 
function(value, namespace="")
{
  node <- xmlNode("text", namespace=namespace)
  node$value <- value
  class(node) <- c("XMLTextNode", class(node))
 node
}


xmlPINode <-
function(sys, value, namespace="")
{
  x <- xmlNode(name=sys, namespace=namespace)
  x$value <- value
  class(x) <- c("XMLProcessingInstruction", class(x))

 x
}

xmlCDataNode <-
function(...)
{
  txt <- paste(..., collapse="")  
 
  node <- xmlNode("text")
  node$value <- txt
  class(node) <- c("XMLCDataNode", class(node))

 node
}

asXMLNode <-
function(x)
{
  if(!inherits(x, "XMLNode")) {
    xmlTextNode(x)
  } else {
    x
  }
}

