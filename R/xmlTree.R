xmlTree <-
function(tag=NULL, attrs = NULL, dtd=NULL, namespaces=list())
{
 doc <- newXMLDoc(dtd, namespaces)
 currentNodes <- list(doc)


 isXML2 <- libxmlVersion()$major != "1" 
 
 
 if(!is.null(dtd) && dtd != "") {
   if(isXML2) {
     node = .Call("R_newXMLDtd", doc, dtd, "", "")
     .Call("R_insertXMLNode", node, doc)
     currentNodes[[2]] <- node
   } else
     warning("DTDs not supported in R for libxml 1.*. Use libxml2 instead.")
 }
 
 definedNamespaces = list()
 
 asXMLNode <- function(x) {
        if(is.character(x)) {
          v <- .Call("R_newXMLTextNode", x)
        } else if(is.list(x)) {
          v <- lapply(x, asXMLNode)
        }  else {
          # Problem!
          browser()
        }

        v 
 }

 setNamespace <- function(node, namespace) {
     if(is.null(namespace))
       return(NULL)

     if(!is.na(match(namespace, names(namespaces))) && is.na(match(namespace, names(definedNamespaces)))) {
       ns <- .Call("R_xmlNewNs", node, namespaces[[namespace]], namespace)
       definedNamespaces[[namespace]] <<- ns
     }
     
     .Call("R_xmlSetNs", node, definedNamespaces[[namespace]])
 }

 
 addTag <- function(name, ..., attrs=NULL, close=TRUE, namespace=NULL) {

   if(!is.null(attrs))
    storage.mode(attrs) <- "character"

   node <- .Call("R_newXMLNode", name, attrs, namespace, doc)

   setNamespace(node, namespace)

   if(length(currentNodes) > 1)
     .Call("R_insertXMLNode", node, currentNodes[[1]])

   if(close == FALSE) {
    currentNodes <<- c(node, currentNodes)
   }

   kids <- list(...)
   if(length(kids)) {
    for(i in kids) {
      if(!inherits(i, "XMLNode")) {
        i <- asXMLNode(i)
      }
      .Call("R_insertXMLNode", i, node)
    }
   }

   invisible(return(node))
 }

 closeTag <- function(name="") {
  tmp <- currentNodes[[1]]
  currentNodes <<- currentNodes[-1]

  invisible(return(tmp))
 }

 addComment <- function(...) {
  node <- .Call("R_xmlNewComment", paste(as.character(list(...)), sep=""))
  .Call("R_insertXMLNode", node, currentNodes[[1]])   
 }


 addCData <- function(text) {
   node <- .Call("R_newXMLCDataNode", doc, as.character(text))
   if(length(currentNodes) > 1)
     .Call("R_insertXMLNode", node, currentNodes[[1]])
   node
 }

 addPI <- function(name, text) {
   node <- .Call("R_newXMLPINode", doc, as.character(name), as.character(text))
   if(length(currentNodes) > 1)
     .Call("R_insertXMLNode", node, currentNodes[[1]])
   node
 }  

 v <- list(
         addTag = addTag,
         addCData = addCData,
         addPI = addPI,
         closeTag = closeTag,
         addComment = addComment,
         value = function() doc,
         add = function(...){}
       )

 class(v) <- c("XMLInternalDOM", "XMLOutputStream")
 return(v)
}


xmlRoot.XMLInternalDocument = 
function(x, ...)
{
  .Call("R_xmlRootNode", x)
}

setOldClass("XMLNode")
setOldClass("XMLInternalNode")
setAs("XMLInternalNode", "XMLNode",
        function(from) {
           .Call("R_createXMLNode", from, NULL)
        })


xmlName.XMLInternalNode =
function(node, full = FALSE)
{
  .Call("RS_XML_xmlNodeName", node)
}

xmlNamespace.XMLInternalNode =
function(x)
{
  .Call("RS_XML_xmlNodeNamespace", x)
}

xmlAttrs.XMLInternalNode = 
function(node, addNamespace = TRUE)
{
  .Call("RS_XML_xmlNodeAttributes",  node, as.logical(addNamespace))
}

xmlChildren.XMLInternalNode =
function(x)
{
 .Call("RS_XML_xmlNodeChildrenReferences", x)
}


"[[.XMLInternalNode" <-
function(x, i, j, ...)
{
  kids = xmlChildren(x)
  if(is.numeric(i))
     kids[[i]]
  else {
     id = as.character(i)
     which = match(id, sapply(kids, xmlName))
     kids[[which]]
  }
}  



"[.XMLInternalNode" <-
function(x, i, j, ...)
{
  kids = xmlChildren(x)
  if(is.numeric(i))
     kids[i]
  else {
     id = as.character(i)
     which = (id == sapply(kids, xmlName))
     kids[which]
  }
}  


xmlValue.XMLInternalNode =
function(x, ignoreComments = FALSE)
{
  .Call("R_xmlNodeValue", x)
}  



xmlApply.XMLInternalNode =
function(X, FUN, ...)
{
   kids = xmlChildren(X)
   lapply(kids, FUN, ...)
}  

xmlSApply.XMLInternalNode =
function(X, FUN, ...)
{
   kids = xmlChildren(X)
   sapply(kids, FUN, ...)
}  



xmlParent =
function(x)
 UseMethod("xmlParent")

xmlParent.XMLInternalNode =
function(x)
{
  .Call("RS_XML_xmlNodeParent", x)
}




newXMLDoc <-
#
# Creates internal C-level libxml object for representing
# an XML document/tree of nodes.
#
function(dtd, namespaces = NULL)
{
  .Call("R_newXMLDoc", dtd, namespaces)
}

newXMLNode <-
#
# Create an internal C-level libxml node
#
function(name, ..., attrs=NULL, namespace="", doc = NULL)
{
 if(!is.null(attrs)) {
   tmp <- names(attrs)
   attrs <- as.character(attrs)
   names(attrs) <- tmp
 }

 children <- list(...)

 node <- .Call("R_newXMLNode", as.character(name), attrs, as.character(namespace), doc)
 if(!is.null(children)) {
   for(i in children)
     .Call("R_insertXMLNode", i, node)
 }

 node
}


saveXML <-
function(doc, file=NULL, compression=0, indent=TRUE, prefix = '<?xml version="1.0"?>\n',
         doctype = NULL, encoding = "")
{
 UseMethod("saveXML")
}

saveXML.XMLInternalDocument <-
function(doc, file=NULL, compression=0, indent=TRUE, prefix = '<?xml version="1.0"?>\n',
         doctype = NULL, encoding = "")
{
  if(is(doctype, "Doctype")) {
       # Check that the value in the DOCTYPE for the top-level name is the same as that of the
       # root element
       
     topname = xmlName(xmlRoot(doc))

     if(doctype@name == "")
        doctype@name = topname
     else if(topname == doctype@name)
       stop("The top-level node and the name for the DOCTYPE must agree", doctype@name, " ", topname)

     prefix = c(doctype@name, doctype@public, doctype@system)
  }

  .Call("R_saveXMLDOM", doc, file, as.integer(compression), as.logical(indent), as.character(prefix))
}

saveXML.XMLInternalDOM <-
function(doc, file=NULL, compression=0, indent=TRUE, prefix = '<?xml version="1.0"?>\n',
         doctype = NULL, encoding = "")
{
  NextMethod("saveXML", object = doc$value())
}


saveXML.XMLOutputStream =
function(doc, file = NULL, compression = 0, indent = TRUE, prefix = '<?xml version="1.0"?>\n',
         doctype = NULL, encoding = "")
{
  NextMethod("saveXML", object = doc$value())
}

saveXML.XMLNode =
#
# Need to handle a DTD here as the prefix argument..
#
function(doc, file = NULL, compression = 0, indent = TRUE, prefix = '<?xml version="1.0"?>\n',
         doctype = NULL, encoding = "")
{
  if(is.character(file))
    file = file(file, "w")
  
  if(inherits(file, "connection")) {
    sink(file)
    on.exit(sink())    
  }

  if(!is.null(prefix))
    cat(as.character(prefix))

  if(!is.null(doctype))
    cat(as.character(doctype), '\n')
  
  print(doc)
}


