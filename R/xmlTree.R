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
function(doc, file=NULL, compression=0, indent=TRUE, prefix = '<?xml version="1.0"?>\n')
{
 UseMethod("saveXML")
}

saveXML.XMLInternalDocument <-
function(doc, file=NULL, compression=0, indent=TRUE, prefix = '<?xml version="1.0"?>\n')
{
  .Call("R_saveXMLDOM", doc, file, as.integer(compression), as.logical(indent), as.character(prefix))
}

saveXML.XMLInternalDOM <-
function(doc, file=NULL, compression=0, indent=TRUE, prefix = '<?xml version="1.0"?>\n')
{
  saveXML(doc$value(), file=file, compression=compression, indent=indent)
}


saveXML.XMLOutputStream =
function(doc, file = NULL, compression = 0, indent = TRUE, prefix = '<?xml version="1.0"?>\n')
{
  saveXML(doc$value(), file=file, compression=compression, indent=indent)
}

saveXML.XMLNode =
function(doc, file = NULL, compression = 0, indent = TRUE, prefix = '<?xml version="1.0"?>\n')
{
  sink(file)
  if(!is.null(prefix))
    cat(prefix)
  on.exit(sink())
  print(doc)
}


