xmlTree <-
function(tag = NULL, attrs = NULL, dtd=NULL, namespaces = list(),
          doc = newXMLDoc(dtd, namespaces))
  # Allows a DOCTYPE, etc. at the beginning by specifying dtd as 
  # a vector of 1, 2, 3 elements passed to newXMLDTDNode() or
  # as an XMLDTDNode directly.
  
  # With namespaces, we do the following....
  #
{
 currentNodes <- list(doc)
 
 isXML2 <- libxmlVersion()$major != "1" 

    # if we are given a DTD, add it to the document.
 if(!is.null(dtd)) {
   if(isXML2) {
     node = NULL
     if(is(dtd, "XMLDTDNode"))
       node = dtd
     else if(is.character(dtd) && dtd[1] != "")
       node = newXMLDTDNode(dtd, doc = doc)

     if(!is.null(node)) {
       addChildren(doc, node)
       currentNodes[[2]] <- node #???XXX
     }
   } else
     warning("DTDs not supported in R for libxml 1.*. Use libxml2 instead.")
 }
 
 definedNamespaces = list()
 defaultNamespace = NULL
 setActiveNamespace = function(ns) {
                         defaultNamespace <<- ns
                      }
 
 asXMLNode <- function(x) {
        if(is(x, "XMLInternalNode"))
          return(x)
        
        if(is.list(x)) {
          v <- lapply(x, asXMLNode)
        }  else {
          v <- newXMLTextNode(as.character(x), doc)
        }
        v 
      }


 
 setNamespace <- function(node, namespace = defaultNamespace) {
     if(length(namespace) == 0 || !(length(namespace) == 1 && is.null(names(namespace))))
       return(NULL)

     if(is.list(namespace))
       return(NULL)
     
     if(!is.na(match(namespace, names(namespaces))) && is.na(match(namespace, names(definedNamespaces)))) {
       ns <- .Call("R_xmlNewNs", node, namespaces[[namespace]], namespace)
       definedNamespaces[[namespace]] <<- ns
     }

     setInternalNamespace( node, definedNamespaces[[namespace]])
 }

 
 addTag <- function(name, ..., attrs = NULL, close = TRUE, namespace = defaultNamespace, .children = list(...) ) {

   if(!is.null(attrs))
      storage.mode(attrs) <- "character"

   node <- newXMLNode(name, attrs = attrs, namespace = namespace, doc = doc)

   setNamespace(node, namespace)

   if(length(currentNodes) > 1)
      addChildren(currentNodes[[1]], node)

   currentNodes <<- c(node, currentNodes)

   for(i in .children) 
      addChildren(node, asXMLNode(i))  # vectorize XXX

   if(close == TRUE)
     closeTag()
   
   invisible(return(node))
 }

 closeTag <- function(name="") {
  tmp <- currentNodes[[1]]
  currentNodes <<- currentNodes[-1]

  invisible(return(tmp))
 }


 add = function(node, parent = currentNodes[[1]]) {
        if(!is.null(parent))
            addChildren(currentNodes[[1]], node)
        invisible(node)
       }
 
 addComment <- function(...) {
   add(newXMLCommentNode(paste(as.character(list(...)), sep=""), doc))
 }


 addCData <- function(text) {
   add(newXMLCDataNode(text, doc))
 }

 addPI <- function(name, text) {
   add(newXMLPINode(name, text, doc))
 }


 if(!is.null(tag)) {

   if(is.character(tag)) {
     node = addTag(tag, attrs = attrs, namespace = namespaces, close = FALSE)
   } else if(is(tag, "XMLInternalNode")) {
     if(is.null(xmlParent(node))) # if we have a DTD node, need to add it to that or parallel to that?
       addChildren(doc, node)
   }
   

 }

 v <- list(
         addTag = addTag,
         addNode = addTag,           
         addCData = addCData,
         addPI = addPI,
         closeTag = closeTag,
         closeNode = closeTag,
         addComment = addComment,
         setNamespace = setActiveNamespace,
         value = function() doc,
         doc = function() doc,
         add = function(...){}
       )

 class(v) <- c("XMLInternalDOM", "XMLOutputStream")
 return(v)
}





