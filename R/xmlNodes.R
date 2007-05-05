xmlRoot.XMLInternalDocument = 
function(x, ...)
{
  .Call("R_xmlRootNode", x)
}

setOldClass("XMLInternalDocument")
setOldClass("XMLNode")
setOldClass("XMLInternalNode")

setOldClass(c("XMLInternalElementNode", "XMLInternalNode"))
setOldClass(c("XMLInternalTextNode", "XMLInternalNode"))

setOldClass("XMLNamespace")

setOldClass("XMLInternalDOM")


setAs("XMLInternalNode", "XMLNode",
        function(from) 
           asRXMLNode(from)
        )


setAs("XMLInternalDocument", "character", function(from) saveXML(from))
setAs("XMLInternalDOM", "character", function(from) saveXML(from))


setAs("XMLInternalDocument", "XMLInternalNode",
       function(from) xmlRoot(from))

setAs("XMLInternalNode", "XMLInternalDocument",
        function(from)
           .Call("R_getXMLNodeDocument", from)
      )



setGeneric("free", function(obj) standardGeneric("free"))

setMethod("free", "XMLInternalDocument",
           function(obj)  .Call("R_XMLInternalDocument_free", obj))


asRXMLNode =
function(node, converters = NULL, trim = TRUE, ignoreBlanks = TRUE)
   .Call("R_createXMLNode", node, converters, as.logical(trim), as.logical(ignoreBlanks))

"[.XMLInternalDocument" =
function(x, i, j, ...)
{
  if(is.character(i)) {
    getNodeSet(x, i, ...)
  } else
     stop("No method for subsetting an XMLInternalDocument with ", class(i))
}  

xmlName.XMLInternalNode =
function(node, full = FALSE)
{
  ans = .Call("RS_XML_xmlNodeName", node)
  if((is.logical(full) && full) || length(full)) {
    tmp = xmlNamespace(node)
    if(length(tmp) && length(names(tmp)) > 0 && names(tmp) != "")
       ans = paste(names(tmp), ans, sep = ":")
    else if(is.character(full) && full != "")
       ans = paste(full, ans, sep = ":")
  }
  ans
}

xmlNamespace.XMLInternalNode =
function(x)
{
  .Call("RS_XML_xmlNodeNamespace", x)
}

xmlAttrs.XMLInternalNode = 
function(node, addNamespace = TRUE, ...)
{
  .Call("RS_XML_xmlNodeAttributes",  node, as.logical(addNamespace))
}

xmlChildren.XMLInternalNode =
function(x)
{
 .Call("RS_XML_xmlNodeChildrenReferences", x)
}

xmlSize.XMLInternalNode =
function(obj)
  .Call("RS_XML_xmlNodeNumChildren", obj)

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


names.XMLInternalNode =
function(x)
  xmlSApply(x, xmlName)

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


newXMLDTDNode <-
function(nodeName, externalID = character(), systemID = character(), doc = NULL)  
{
  if(length(nodeName) > 1 && missing(externalID))
    externalID = nodeName[2]
  if(length(nodeName) > 2 && missing(systemID))
    systemID = nodeName[3]  

  .Call("R_newXMLDtd", doc, as.character(nodeName), as.character(externalID), as.character(systemID))
}

setInternalNamespace =
function(node, ns)
{
  .Call("R_xmlSetNs", node, ns, FALSE) # as.logical(append))
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
  #
  #  It is  possible to use a namespace prefix that is not defined.
  #  This is okay as it may be defined in another node which will become
  #  an ancestor of this newly created one.
  
function(name, ..., attrs = NULL, namespace = "", doc = NULL, .children = list(...))
{
    # make certain we have character
 ids = names(attrs)
 attrs = as(attrs, "character")
 names(attrs) = ids

        # allow the caller to specify the node name as  ns_prefix:name
        # but we have to create it as name and the set the namespace.
 ns = character()
 name = strsplit(name, ":")[[1]]
 if(length(name) == 2) {
   ns = name[1]
   name = name[2]
 }

   # create the node. Let's leave the namespace definitions and prefix till later.

  # xmlSetProp() routine in R_newXMLNode() handles namespaces on the attribute names,
  # even checking them.
 node <- .Call("R_newXMLNode", as.character(name), attrs, character(), doc)


 nsDefs = list()
 if(length(namespace) > 0) {
    if(length(names(namespace)) == 0) 
      names(namespace) <- rep("", length(namespace))

    if(length(namespace) > 1 && sum(names(namespace) == "") > 1)
      warning("more than one namespace to use as the default")

    nsDefs = lapply(seq(along = namespace),
                   function(i) {
                      prefix = names(namespace)[i]
                      ns <- .Call("R_xmlNewNs", node, namespace[[i]], prefix)
                      # Don't set the namespace. This is just a definition/declaration for
                      # this node, but not necessarily the namespace to use for this node.
                      # We set this below
                      ns
                   })
 }

 
 if(length(ns)) {
   i = match(ns, names(namespace))
   if(is.na(i)) {
       ns <- .Call("R_xmlNewNs", node, "", ns)
   } else
      ns <- nsDefs[[i]]
 } else  {
   i = match("", names(namespace))
   ns = if(is.na(i)) NULL else nsDefs[[i]]
 }

   # Here is where we set the namespace for this node.
 if(length(ns) && (is(ns, "XMLNamespaceDeclaration") || (is.character(ns) && ns != "")))
     .Call("R_xmlSetNs", node, ns, FALSE)

 
 if(!is.null(.children)) {
   for(i in .children) {
     if(!is(i, "XMLInternalNode"))
        i = as(i, "XMLInternalNode")
     
     .Call("R_insertXMLNode", i, node)
   }
 }

 node
}

checkNodeNamespace =
  #
  # can only be checked after we know the parent node, 
  # i.e. after it has been inserted.
  #
function(node, prefix = xmlNamespace(ns))
{
  if(length(prefix) == 0 || prefix == "")
    return(TRUE)
  
       # XXX should check that namespace is defined
       # walk the parents.

  okay = FALSE
  p = xmlParent(node)
  while(!is.null(p)) {
    okay = ns %in% names(xmlNamespaceDefinitions(p))
    if(okay)
      break
  }
  
  if(!okay)
    stop("using an XML namespace prefix '", ns, "' for a node that is not defined for this node or its node's ancestors")

  TRUE
}  

# Still to do:
#   element, entity, entity_ref, notation
# And more in libxml/tree.h, e.g. the declaration nodes
# 

newXMLTextNode =
function(text, doc = NULL)
  .Call("R_newXMLTextNode", as.character(text), doc)      

newXMLPINode <-
function(name,  text, doc = NULL)
{
  .Call("R_newXMLPINode", doc, as.character(name), as.character(text))
}

newXMLCDataNode <-
function(text, doc = NULL)
{
  .Call("R_newXMLCDataNode", doc,  as.character(text))
}

newXMLCommentNode <-
function(text, doc = NULL)  # doc is not used.XXX
{
  .Call("R_newXMLComment",  as.character(text))
}




addChildren =
function(node, ..., kids = list(...))
{
  for(i in kids) {
     if(is.character(i))
       i = newXMLTextNode(i)
     .Call("R_insertXMLNode", i, node)
  }
}

removeChildren =
function(node, ..., kids = list(...), free = FALSE)
{
   # idea is to get the actual XMLInternalNode objects
   # corresponding the identifiers in the kids list.
   # These are numbers, node names or node objects themselves
   # This could be fooled by duplicates, e.g. kids = list(2, 2)
   # or kids = list(2, "d") where "d" identifies the second node.
   # We can put in stricter checks in the C code if needed.
  nodes =  xmlChildren(node)
  nodeNames = xmlSApply(node, xmlName)
  v = lapply(kids,
             function(x)  {
                 if(is(x, "XMLInternalNode"))
                   x
                 else if(is.character(x)) {
                   i = match(x, nodeNames)
                   nodes[[i]]
                 } else
                   nodes[[as.integer(x)]]
               })

   free = rep(free, length = length(v))
   .Call("RS_XML_removeChildren", node, v, as.logical(free))
   node
}



setGeneric("toHTML",
            function(x) standardGeneric("toHTML"))


setMethod('toHTML', 'vector',
            function(x) {
              tb = newXMLNode("table")
              if(length(names(x)) > 0) 
                addChildren(tb, newXMLNode("tr", .children = sapply(names(x), function(x) newXMLNode("th", x))))


              addChildren(tb, newXMLNode("tr", .children = sapply(x, function(x) newXMLNode("th", format(x)))))
              tb
            })

setMethod('toHTML', 'matrix',
            function(x) {
              tb = newXMLNode("table")
              if(length(colnames(x)) > 0) 
                addChildren(tb, newXMLNode("tr", .children = sapply(names(x), function(x) newXMLNode("th", x))))

              rows = sapply(seq(length = nrow(x)), 
                             function(i) {
                               row = newXMLNode("tr")
                               if(length(rownames(x)) > 0)
                                 addChildren(row, newXMLNode("th", rownames(x)[i]))
                               addChildren(row,  .children = sapply(r, function(x) newXMLNode("th", format(x))))
                               row
                             })
              addChildren(tb, rows)

              tb
              })





setAs("vector", "XMLInternalNode",
      function(from) {
          newXMLTextNode(as(from, "character"))
      })


saveXML <-
function(doc, file=NULL, compression=0, indent=TRUE, prefix = '<?xml version="1.0"?>\n',
         doctype = NULL, encoding = "")
{
 UseMethod("saveXML")
}

saveXML.XMLInternalNode <-
function(doc, file=NULL, compression=0, indent=TRUE, prefix = '<?xml version="1.0"?>\n',
         doctype = NULL, encoding = "")  
{
  if(encoding == "")
    encoding = character()
  
  .Call("RS_XML_printXMLNode", doc, as.integer(0), as.integer(indent), as.logical(indent), as.character(encoding))
}

setAs("XMLInternalNode", "character",
          function(from) saveXML.XMLInternalNode(from))

saveXML.XMLInternalDocument <-
function(doc, file = NULL, compression=0, indent=TRUE, prefix = '<?xml version="1.0"?>\n',
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

  .Call("R_saveXMLDOM", doc, file, as.integer(compression), as.logical(indent),
                         as.character(prefix), as.character(encoding))
}

saveXML.XMLInternalDOM <-
function(doc, file=NULL, compression=0, indent=TRUE, prefix = '<?xml version="1.0"?>\n',
         doctype = NULL, encoding = "")
{
  saveXML(doc$value(), file, compression, indent, prefix, doctype, encoding)
}


saveXML.XMLOutputStream =
function(doc, file = NULL, compression = 0, indent = TRUE, prefix = '<?xml version="1.0"?>\n',
         doctype = NULL, encoding = "")
{
  saveXML(doc$value(), file, compression, indent, prefix, doctype, encoding)  
}


saveXML.sink =
#
# Need to handle a DTD here as the prefix argument..
#
function(doc, file = NULL, compression = 0, indent = TRUE, prefix = '<?xml version="1.0"?>\n',
         doctype = NULL, encoding = "")
{
  if(is.character(file)) {
    file = file(file, "w")
    on.exit(close(file))
  }
  
  if(inherits(file, "connection")) {
    sink(file)
    on.exit(sink())    
  }

  if(!is.null(prefix))
    cat(as.character(prefix))

  if(!is.null(doctype))
    cat(as(doctype, "character"), '\n')
  
  print(doc)
}
  


saveXML.XMLNode = saveXML.sink

saveXML.XMLFlatTree = saveXML.sink


setGeneric("addAttributes", function(node, ..., .attrs = NULL) standardGeneric("addAttributes"))

setMethod("addAttributes", "XMLInternalElementNode",
function(node, ..., .attrs = NULL)
{
   if(missing(.attrs)) {
     .attrs = list(...)
   }
   .attrs = structure(as.character(.attrs), names = names(.attrs))

   if(is.null(names(.attrs)) || any(names(.attrs) == ""))
     stop("all node attributes must have a name")

   .Call("RS_XML_addNodeAttributes", node, .attrs)
   node
})

setMethod("addAttributes", "XMLNode",
           function(node, ..., .attrs = NULL) {
             if(missing(.attrs)) {
               .attrs = list(...)
             }
             .attrs = structure(as.character(.attrs), names = names(.attrs))

             if(is.null(names(.attrs)) || any(names(.attrs) == ""))
               stop("all node attributes must have a name")

             i = match(names(.attrs), names(node$attributes))
             if(any(!is.na(i))) {
                 node$attributes[i[!is.na(i)]] =  .attrs[!is.na(i)]
                 .attrs = .attrs[is.na(i)]
             }
             node$attributes = c(node$attributes, .attrs)
             node
           })

setGeneric("removeAttributes", function(node, ..., .attrs = NULL, .namespace = FALSE) standardGeneric("removeAttributes"))

setMethod("removeAttributes", "XMLInternalElementNode",
#
# The idea here is to remove attributes by name
# We handle the case where these are a simple collection
# of character string identifiers given via the ... or as a character
# vector using, e.g., .attrs = c("a", "b")
#
# Each identifier can be of the form  "name" or "ns:name" giving
# the namespace prefix. We resolve the namespace and 
#

#  If we are dealing with regular attributes (no namespace attributes)
#  then we expect these as a character vector.
#
# The intent of the .namespace argument was originally to indicate that
# we wanted to remove the namespace definition. It appears that libxml2 does
# not support that. (And it would seem that this is a real pain as the xmlNsPtr
# objects can be shared across numerous places in a linked list, so it would 
# be very difficult to remove it from one node.)
#
#
#
function(node, ..., .attrs = NULL, .namespace = FALSE)
{
   if(missing(.attrs)) 
     .attrs = list(...)
   
   .attrs = as.character(.attrs)

   if(is(.namespace, "XMLNamespaceDeclaration"))
     .namespace = list(.namespace)
#XXX   

   tmp = strsplit(.attrs, ":")
   prefix = sapply(tmp, function(x) if(length(x) > 1) x[1] else "")
   ids = sapply(tmp, function(x) if(length(x) == 1) x[1] else x[2])   

   if(any(prefix != "") && is.logical(.namespace))
     .namespace = TRUE
   
   if(is.logical(.namespace) && .namespace) {
     ns = namespaceDeclarations(node, TRUE)
     # need to create a list with the elements corresponding to the
     # (potentially repeated) ns elements

     i = match(prefix, names(ns))
     ns = ns[i]
     names(ns) = gsub("^.*:", "", .attrs) # or ids from above
     
     .attrs = ns
   }

   .Call("RS_XML_removeNodeAttributes", node, .attrs, .namespace)
   node
})


setMethod("removeAttributes", "XMLNode",
function(node, ..., .attrs = NULL, .namespace = FALSE)
{
  a = node$attributes

   if(missing(.attrs)) 
     .attrs = list(...)
   
   .attrs = as.character(.attrs)  

  i = match(.attrs, names(a))
  if(any(is.na(i)) ) 
     warning("Can't locate attributes ", paste(.attrs[is.na(i)], collapse = ", "), "in XML node ", x$name)

  a = a[is.na(i)]
  
  node$attributes <- a
  node
})

namespaceDeclarations =
function(node, ref = FALSE)
{
  .Call("RS_XML_getNsList", node,  as.logical(ref))
}  


"xmlName<-" =
function(x, value)
{
   UseMethod("xmlName<-")
}  

"xmlName<-.XMLNode" <-
function(x, value)
{
   x$name <- value
   x
}

"xmlName<-.XMLInternalElementNode" <-
function(x, value)
{
   # we could handle a new namespace by accepting value as
   # a character vector with a name
   # e.g.   c(r:array = 'http://www.r-project.org')
   # Alternatively, just define the namespace on the node _before_
   # changing the name.
   id = names(value)
   if(!is.null(id) && length( (tmp <- strsplit(id, ":")[[1]])) > 1) {
       names(value) = tmp[1]
       newXMLNamespaces(x, .values = as(value, "character"))
       value = id
   }
  
   .Call("RS_XML_setNodeName", x, value)
   x
}  

newXMLNamespaces =
  # allow for multiple namespaces
  # and also allow for "r:value"
  #
  #  newXMLNamespaces(node, r = "http://www.r-project.org", ...)
  #
function(node, ..., .values = list(...))
{
  ids = names(.values)
  ans = lapply(ids, function(id)
                      .Call("R_xmlNewNs", id, as.character(.values[[id]])))

  names(ans) = ids
  ans
}
