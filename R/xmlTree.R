xmlTree <-
function(tag=NULL, dtd=NULL, namespaces=list())
{
 doc <- newXMLDoc(dtd, namespaces)
 currentNodes <- list(doc)

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
 
 addTag <- function(name, ..., attrs=NULL, close=T, namespace=NULL) {

   if(!is.null(attrs))
    storage.mode(attrs) <- "character"

   node <- .Call("R_newXMLNode", name, attrs, namespace, doc)

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

 v <- list(
         addTag = addTag,
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
function(doc, file=NULL, compression=0, indent=T)
{
 UseMethod("saveXML")
}

saveXML.XMLInternalDocument <-
function(doc, file=NULL, compression=0, indent=T)
{
  .Call("R_saveXMLDOM", doc, file, as.integer(compression), as.logical(indent))
}

saveXML.XMLInternalDOM <-
function(doc, file=NULL, compression=0, indent=T)
{
  saveXML(doc$value(), file=file, compression=compression, indent=indent)
}
