xmlNode <-
function(name, ..., attrs = NULL, namespace = "", .children = list(...))
{
  kids <- lapply(.children, asXMLNode)
  kids = addNames(kids)

  node <- list(name = name, attributes = attrs, children = kids, namespace=namespace)
  class(node) <- c("XMLNode")

  node
}

addNames =
function(kids, fromTag = TRUE)
{
  if(fromTag)
     names(kids) = sapply(kids, xmlName)
  else if(length(names(kids)) == 0)
      names(kids) <- sapply(kids,xmlName)
  else if(any( idx <- names(kids) == "")) 
      names(kids)[idx] <- sapply(kids[idx], xmlName)

  kids
}

"xmlChildren<-" <-
function(x, value) {
  value = addNames(value)
  x$children <- value
  x
}

addChildren =
function(node, ..., kids = list(...), at = NA, cdata = FALSE)
  UseMethod("addChildren")

addChildren.XMLNode =  
function(node, ..., kids = list(...), at = NA, cdata = FALSE)
{
  kids = lapply(kids,
                function(i) {
                  if(!is(i, "XMLNode"))
                    xmlTextNode(as.character(i), cdata = cdata)
                  else
                    i
                })

  node$children = c(node$children, kids)
  node$children = addNames(node$children)
  
  node
}


# It would be better tokenize this, but ...
XMLEntities =
  c("&" = "amp",  # order is important as if we insert an entity, then the next time we will see the &.
    ">" = "gt",
    "<" = "lt",
    "'" = "apos",
    '"' = "quot")


insertEntities =
function(value, entities = XMLEntities)
{
    pat = names(entities)
    subs = paste("&", entities, ";", sep = "")
    for(i in seq(along = entities)) 
      value = gsub(pat[i], subs[i], value)

    value
}

xmlTextNode <- 
function(value, namespace = "", entities = XMLEntities, cdata = FALSE)
{
  node <- xmlNode("text", namespace = namespace)

  if(length(entities)) 
   value = insertEntities(value, XMLEntities)

  if(cdata)
    value = xmlCDataNode(value)
  
  node$value <- value
  class(node) <- c("XMLTextNode", class(node))
  if(length(entities))
    class(node) <- c(class(node), "EntitiesEscaped")
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

xmlCommentNode <-
function(text)
{
  node <- xmlTextNode(text)
  class(node) <- c("XMLCommentNode", class(node))

  node
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

