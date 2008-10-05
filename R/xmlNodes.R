xmlRoot.XMLInternalDocument = 
function(x, skip = TRUE, ...)
{
  .Call("R_xmlRootNode", x, as.logical(skip))
}


setAs("XMLNode", "XMLInternalNode",
       function(from) {
           con = textConnection("tmp", "w", local = TRUE)
           sink(con)
           on.exit({sink(file = NULL); close(con)})
           print(from)
           
           doc = xmlInternalTreeParse(tmp, asText = TRUE)
           node = xmlRoot(doc)
           removeChildren(node)
           node
        }
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
           function(obj) {
              invisible(.Call("R_XMLInternalDocument_free", obj))
           })


addFinalizer =
function(obj, fun, ...)
{
  UseMethod("addFinalizer")
}

addCFinalizer.XMLInternalDocument =
function(obj, fun, ...)
{
  if(missing(fun) || fun == NULL)
    fun = getNativeSymbolInfo("RSXML_free_internal_document")$address
  else if(!is.function(obj)) {

  }
    
  .Call("R_addXMLInternalDocument_finalizer", obj, fun)
}   


asRXMLNode =
function(node, converters = NULL, trim = TRUE, ignoreBlanks = TRUE)
   .Call("R_createXMLNode", node, converters, as.logical(trim), as.logical(ignoreBlanks))

"[.XMLInternalDocument" =
function(x, i, j, ..., namespaces = xmlNamespaceDefinitions(x, simplify = TRUE))
{
  if(is.character(i)) {
    getNodeSet(x, i, ...)
  } else
     stop("No method for subsetting an XMLInternalDocument with ", class(i))
}  

"[[.XMLInternalDocument" =
function(x, i, j, ..., exact = NA, namespaces = xmlNamespaceDefinitions(x, simplify = TRUE))
{
  ans = x[i]
  if(length(ans) > 1)
    warning(length(ans), " elements in node set. Returning just the first one! (Use [])")
  ans[[1]]
}



xmlName.XMLInternalNode =
function(node, full = FALSE)
{
  ans = .Call("RS_XML_xmlNodeName", node)
  if((is.logical(full) && full) || (!is.logical(full) && length(full))) {
    tmp = xmlNamespace(node)
    if(length(tmp) && length(names(tmp)) > 0 && names(tmp) != "")
       ans = paste(names(tmp), ans, sep = ":")
    else if(is.character(full) && full != "")
       ans = paste(full, ans, sep = ":")
  }
  ans
}

if(useS4)
 setMethod("xmlName", "XMLInternalNode", xmlName.XMLInternalNode)


xmlNamespace.XMLInternalNode =
function(x)
{
  .Call("RS_XML_xmlNodeNamespace", x)
}



xmlAttrs.XMLInternalNode = 
function(node, addNamespace = FALSE, ...)
{
  .Call("RS_XML_xmlNodeAttributes",  node, as.logical(addNamespace))
}



xmlChildren.XMLInternalNode =
function(x, addNames = TRUE)
{
 .Call("RS_XML_xmlNodeChildrenReferences", x, as.logical(addNames))
}


xmlChildren.XMLInternalDocument =
function(x, addNames = TRUE)
{
# .Call("RS_XML_xmlDocumentChildren", x, as.logical(addNames))
 xmlChildren.XMLInternalNode(x, addNames)
}


if(useS4) {
setMethod("xmlAttrs", "XMLInternalNode", xmlAttrs.XMLInternalNode)
setMethod("xmlChildren", "XMLInternalNode", xmlChildren.XMLInternalNode)
setMethod("xmlChildren", "XMLInternalDocument", xmlChildren.XMLInternalDocument)
}


xmlSize.XMLInternalNode =
function(obj)
  .Call("RS_XML_xmlNodeNumChildren", obj)

"[[.XMLInternalNode" <-
function(x, i, j, ...)
{
  kids = xmlChildren(x)
  if(length(kids) == 0)
    return(NULL)
  
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
  if(is.logical(i))
    i = which(i)
  if(is(i, "numeric"))
     kids[i]
  else {
     id = as.character(i)
     which = match(sapply(kids, xmlName), id)
     kids[!is.na(which)]
  }
}  


xmlValue.XMLInternalNode =
function(x, ignoreComments = FALSE)
{
  .Call("R_xmlNodeValue", x)
}

setS3Method("xmlValue", "XMLInternalNode")


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


addDocFinalizer =
function(doc, finalizer)
{
  fun = NULL
  if(is.logical(finalizer)) {
    if(!finalizer)
      return()
    else
      fun = NULL
  } else {
    fun = finalizer
    if(inherits(fun, "NativeSymbolInfo"))
      fun = fun$address
  }

  if(!is.null(fun) && !is.function(fun) && typeof(fun) != "externalptr")
    stop("need an R function, address of a routine or NULL for finalizer")

  .Call("R_addXMLInternalDocument_finalizer", doc, fun)
}

newXMLDoc <-
#
# Creates internal C-level libxml object for representing
# an XML document/tree of nodes.
#
function(dtd = NA, namespaces = NULL, addFinalizer = TRUE, name = character(), node = NULL) 
{
  ans = .Call("R_newXMLDoc", dtd, namespaces)

  addDocFinalizer(ans, addFinalizer)

  if(length(name))
    docName(ans) = as.character(name)

  if(length(node)) {
#   if(missing(shallow)) {
#     shallow = !is.null(as(node, "XMLInternalDocument"))
#   }
#    if(!deep) 
#      .Call("R_insertXMLNode", ans, node, as.integer(NA), FALSE)
    addChildren(ans, node)
  }

  ans
}

XMLOptions = new.env()
getOption =
function(name, default = NULL, converter = NULL)
{
  if(!exists(name, XMLOptions, inherits = FALSE)) 
    return(default)

   ans = get(name, XMLOptions)

   if(is.function(converter))
     converter(ans)
    else
      ans
}

setOption =
function(name, value)
{
   prev = getOption(name)
   assign(name, value, XMLOptions)
   prev
}  


newXMLNode <-
#
###XXX Note that there is another definition of this in dups.R
 # Which is now elided.
  
# Create an internal C-level libxml node
#
  #
  #  It is  possible to use a namespace prefix that is not defined.
  #  This is okay as it may be defined in another node which will become
  #  an ancestor of this newly created one.

  # XXX Have to add something to force the namespace prefix into the node
  # when there is no corresponding definition for that prefix.
  
function(name, ..., attrs = NULL,
         namespace = "", namespaceDefinitions = character(),
         doc = NULL, .children = list(...), parent = NULL,
         at = NA,
         cdata = FALSE,
         suppressNamespaceWarning = getOption('suppressXMLNamespaceWarning', FALSE) #  i.e. warn.
        )
{
    # make certain we have a character vector for the attributes.
 ids = names(attrs)
 attrs = as(attrs, "character")
 names(attrs) = ids

   # Find any attributes that are actually namespace definitions.
 i = grep("^xmlns", names(attrs))
 if(length(i)) {
   warning("Don't specify namespace definitions via 'attrs'; use namespaceDefinitions")
   namespace = c(namespace, structure(attrs[i], names = gsub("^xmlns:", "", names(attrs)[i])))
   attrs = attrs[ -i]
 }
 

     # allow the caller to specify the node name as  ns_prefix:name
     # but we have to create it as name and the set the namespace.
 ns = nsPrefix = character()
 name = strsplit(name, ":")[[1]]
 if(length(name) == 2) {
   ns = nsPrefix = name[1]
   name = name[2]
 }


 if(missing(doc) && !missing(parent) &&
     inherits(parent, "XMLInternalDocument")) {
   doc = parent
   parent = NULL
 }
 
 if(is.null(doc) && !is.null(parent)) 
   doc = as(parent, "XMLInternalDocument")


   # create the node. Let's leave the namespace definitions and prefix till later.

  # xmlSetProp() routine in R_newXMLNode() handles namespaces on the attribute names,
  # even checking them.
 node <- .Call("R_newXMLNode", as.character(name), attrs, character(), doc, namespaceDefinitions)

 if(!is.null(parent))
   addChildren(parent, node, at = at)


if(TRUE) { # Create the name space definitions here rather than in C code.
  nsDefs = lapply(seq(along  = namespaceDefinitions),
                   function(i) {
                                  
                     newNamespace(node, namespaceDefinitions[[i]], names(namespaceDefinitions)[i], set = FALSE)
                   })
  names(nsDefs) = names(namespaceDefinitions)
}
else
  nsDefs = xmlNamespaceDefinitions(node)

   # Now that the namespaces are defined, we can define the attributes which _may_ use them.
  addAttributes(node, .attrs = attrs, suppressNamespaceWarning = suppressNamespaceWarning)
 

 if(length(namespace) > 0) {
      # a single element with no name so this is the prefix.
    if(length(namespace) == 1 && length(names(namespace)) == 0 && length(grep("^(http|ftp):", namespace)) == 0) {
      if(namespace != "")
        ns = nsPrefix = namespace
    } else {

           # we have names and/or more than one element. So these are namespace definitions
      if(length(names(namespace)) == 0) 
        names(namespace) <- rep("", length(namespace))

      if(length(namespace) > 1 && !is.na(match(namespace[1], names(namespace)[-1]))) {
        if(length(ns)) 
          warning("ignoring first element of namespace and using prefix from node name, ", ns)
        else {
          ns = namespace[1]
          namespace = namespace[-1]
        }
      }
        
      

      if(length(namespace) > 1 && sum(names(namespace) == "") > 1)
        warning("more than one namespace to use as the default")

      nsDefs = lapply(seq(along = namespace),
                      function(i) {
                        prefix = names(namespace)[i]

                        newNamespace(node, namespace[[i]], prefix)
                                # Don't set the namespace. This is just a definition/declaration for
                                # this node, but not necessarily the namespace to use for this node.
                                # We set this below
                      })
      names(nsDefs) = names(namespace)
    }
  }


   # Now handle the prefix for this node.
 if(length(ns)) {
   i = match(ns, names(nsDefs))
   if(is.na(i)) {
      if(!is.null(parent)) 
        ns = findNamespaceDefinition(node, ns)
      else {
         if(is.character(suppressNamespaceWarning))
            f = get(suppressNamespaceWarning, mode = "function")
         else if(is.logical(suppressNamespaceWarning) && !suppressNamespaceWarning) {
            f = warning 
         } else
            f = function(...) {}
             
         f("cannot find namespace definition because the node is not in the document and there are no local namespace definitions for this node")
         attr(node, "xml:namespace") = ns
         ns = NULL
      }
#      ns <- newNamespace(node, "", ns)
   } else
      ns <- nsDefs[[i]]
 } else  {
   i = match("", names(nsDefs))
   ns = if(is.na(i)) NULL else nsDefs[[i]]
 }


 
   # Here is where we set the namespace for this node.
 if(length(ns) && (inherits(ns, c("XMLNamespaceRef", "XMLNamespaceDeclaration")) || (is.character(ns) && ns != "")))
     setXMLNamespace( node, ns) # should this be append = FALSE ?

 if(!is.null(.children))  {
   if(!is.list(.children))
      .children = list(.children)
   addChildren(node, kids = .children, cdata = cdata)
 }

 node
}

findNamespaceDefinition =
  #
  # Search up the node hierarchy looking for a namespace
  # matching that prefix.
  #
function(node, namespace, error = TRUE)
{
  ptr = node
  while(!is.null(ptr)) {
     tmp = namespaceDeclarations(ptr, TRUE)
     i = match(namespace, names(tmp))
     if(!is.na(i))
       return(tmp[[i]])
     ptr = xmlParent(ptr)
  }

  if(error)
    stop("no matching namespace definition for prefix ", namespace)

  NULL
}  

setXMLNamespace =
  #
  # Set the specified namespace as the namespace for this
  # node. 
  # namespace can be a prefix in which case we find it in the
  # definition in this node or its ancestors.
  # Otherwise, we expect a name = value character vector giving the
  # prefix and URI and we create a new namespace definition.
  # Alternatively, if you already have the namespace reference object
  # from earlier, you can pass that in.
  # Then we set the namespace on the node. 
function(node,  namespace, append = FALSE)
{
  if(is.character(namespace) && is.null(names(namespace))) 

     namespace = findNamespaceDefinition(node, namespace)

  else if(is.character(namespace))

     namespace = newNamespace(node, namespace)

  else if(!inherits(namespace, c("XMLNamespaceRef", "XMLNamespaceDeclaration")))
    stop("Must provide a namespace definition, a prefix of existing namespace or a reference to a namespace definition")

  .Call("R_xmlSetNs", node, namespace, FALSE) 
}  


setAs("XMLNamespace", "character",
       function(from)
         unclass(from))

setAs("XMLNamespaceDefinition", "character",
       function(from)
         structure(from$uri, names = from$id))



setGeneric("xmlNamespaces<-",
            function(x, append = TRUE, set = FALSE, value)
              standardGeneric("xmlNamespaces<-"))


setMethod("xmlNamespaces<-", "XMLNode",
            function(x, append = TRUE, set = FALSE, value) {

                if(is(value, "XMLNamespace"))
                  value = as(value, "character")
                else if(is.null(names(value)))
                   names(value) = ""

                    # check for duplicates?
                i = duplicated(names(value))
                if(any(i)) {
                    warning("discarding duplicated namespace prefixes ", paste(names(value)[i], collapse = ", "))
                    value = value[!i]
                }
                
                 if(append) {
                     cur = as(x$namespaceDefinitions, "character")
                     cur[names(value)] = value
                     value = cur
                 }

                x$namespaceDefinitions = as(value, "XMLNamespaceDefinitions")
               
                if(set) 
                  x$namespace = names(value)

               x
            })




setMethod("xmlNamespaces<-", "XMLInternalNode",
            function(x, append = TRUE, set = FALSE, value) {

                value = as(value, "character")
                
                if(is.null(names(value)))
                   names(value) = ""

                    # check for duplicates?
                i = duplicated(names(value))
                if(any(i)) {
                    warning("discarding duplicated namespace prefixes ", paste(names(value)[i], collapse = ", "))
                    value = value[!i]
                }

                if(append) {
                      # Work with existing ones
                   curDefs = namespaceDeclarations(x)
                   i = names(value) %in% names(curDefs)
                   if(any(i)) {
                       warning("discarding duplicated namespace prefixes ", paste(names(value)[i], collapse = ", "))
                       value = value[!i]
                   }
                }

                if(length(value) == 0)
                    # Should worry about the set.
                  return()

                if(length(set) == 1 && set == TRUE && length(value) > 1)
                   set = c(set, rep(FALSE, length(value) - 1))
                else
                   set = rep(set, length.out = length(value))

                
                for(i in seq(along = value))
                  newXMLNamespace(x, value[i], set = set[i])
                
                x
            })
 


newXMLNamespace = newNamespace =
  # Create a new namespace reference object.
function(node, namespace, prefix = names(namespace), set = FALSE)
{
   ns <- .Call("R_xmlNewNs", node, namespace, as.character(prefix))
   if(set)
      setXMLNamespace(node, ns)
   ns     
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
  #
  #  cdata allows the caller to specify that the text be 
  #  wrapped in a newXMLCDataNode
function(text,  parent = NULL, doc = NULL, cdata = FALSE)
{
  if(cdata) 
    return(newXMLCDataNode(text, parent, doc))

  a = .Call("R_newXMLTextNode", as.character(text), doc)
  
  if(!is.null(parent))
     addChildren(parent, a)

  a
}

newXMLPINode <-
function(name,  text,  parent = NULL, doc = NULL, at = NA)
{
  a = .Call("R_newXMLPINode", doc, as.character(name), as.character(text))
  if(!is.null(parent))
    addChildren(parent, a, at = at)
  a  
}

newXMLCDataNode <-
function(text, parent = NULL, doc = NULL, at = NA)
{
  a = .Call("R_newXMLCDataNode", doc,  as.character(text))
  if(!is.null(parent))
    addChildren(parent, a, at = at)
  a  
}

newXMLCommentNode <-
function(text, parent = NULL, doc = NULL, at = NA) 
{
  a = .Call("R_xmlNewComment",  as.character(text), doc)
  if(!is.null(parent))
    addChildren(parent, a, at = at)
  a  
}

replaceNodes =
function(oldNode, newNode, ...)
{
  UseMethod("replaceNodes")
}

replaceNodes.XMLInternalNode =
function(oldNode, newNode, ...)
{
  oldNode = as(oldNode, "XMLInternalNode")
  newNode = as(newNode, "XMLInternalNode")
  
  .Call("RS_XML_replaceXMLNode", oldNode, newNode)
}  

"[[<-.XMLInternalNode" =
function(x, i, j, ..., value)
{
   if(!is.list(value))
     value = list(value)

   addChildren(x, kids = value, at = i)

   x
}  


addChildren.XMLInternalNode =
addChildren.XMLInternalDocument =
  #
  # XXX need to expand/recycle the at if it is given as a scalar
  # taking into account if the subsequent elements are lists, etc.
  #
  # Basically, if the caller specifies at as a scalar
  # we expand this to be the sequence starting at that value
  # and having length which is the total number of nodes
  # in kids.  This is not just the length of kids but
  # the number of nodes since some of the elements might be lists.
  #
function(node, ..., kids = list(...), at = NA, cdata = FALSE)
{
  kids = unlist(kids, recursive = FALSE)
  
  if(!is.na(at)) {

       # if at is the name of a child node, find its index (first node with that name)
    if(is.character(at)) 
      at = match(at, names(node))

    
    if(length(at) == 1) 
      at = seq(as.integer(at), length = sum(sapply(kids, function(x) if(is.list(x)) length(x) else 1)))
    else  # pad with NAs
      length(at) = length(kids)
    
    return(lapply(seq(along = kids),
            function(j) {
               i = kids[[j]]

               if(is.character(i))
                 i = newXMLTextNode(i, cdata = cdata)

               if(!is(i, "XMLInternalNode"))
                 i = as(i, "XMLInternalNode")

               if(is.na(at[j]))
                 .Call("R_insertXMLNode", i, node, -1L, FALSE)
               else {
                  after = at[j] > 0
                  if(!after)
                     at[j] = 1

                  if(xmlSize(node) < at[j])
                    .Call("R_insertXMLNode", i, node, as.integer(NA), FALSE)
                  else
                    .Call("RS_XML_xmlAddSiblingAt", node[[ at[j] ]], i, after) # if at = 0, then shove it in before the sibling.
               }
            }))
  }

  for(j in seq(along = kids)) {
      i = kids[[j]]
      
      if(is.list(i)) {  # can't happen now since we unlist()
         for(k in i)
            addChildren(node, k)
      } else {

        if(is.null(i))
           next
     
        if(is.character(i))
          i = newXMLTextNode(i, cdata = cdata)

        if(!is(i, "XMLInternalNode"))
           i = as(i, "XMLInternalNode")

        .Call("R_insertXMLNode", i, node, at[j], FALSE)

        ns = attr(i, "xml:namespace")
        if(!is.null(ns)) {
           nsdef = findNamespaceDefinition(node, ns)
           if(!is.null(nsdef) && (inherits(nsdef, c("XMLNamespaceRef", "XMLNamespaceDeclaration")) || (is.character(nsdef) && nsdef != ""))) {
             setXMLNamespace( i, nsdef)
             attr(i, "xml:namespace") = NULL
           }
        }
     }
    }

  node
}

addSibling =
function(node, ..., kids = list(...), after = NA)
{
  UseMethod("addSibling")
}

addSibling.XMLInternalNode =
function(node, ..., kids = list(...), after = TRUE)  
{
   #XXX Why add as children?
   if(FALSE && is.na(after))
     addChildren(node, kids = kids, at = NA)
   else {
     lapply(kids,
            function(x) {
              .Call("RS_XML_xmlAddSiblingAt", node, x, as.logical(after))
            })
   }
  
}  



removeNodes =
function(node, free = rep(FALSE, length(node)))
{
  if(is.list(node))  {
    if(!all(sapply(node, inherits, "XMLInternalNode")))
      stop("removeNode only works on internal nodes at present")

    free = as.logical(free)
    free = rep(free, length = length(node))
  } else if(!inherits(node, "XMLInternalNode"))
     stop("removeNode is intended only for internal nodes")
  else {            
    node = list(node)
    free = as.logical(free)
  }
     
  .Call("R_removeInternalNode", node, free)
}  

removeChildren =
function(node, ..., kids = list(...), free = FALSE)
{
  UseMethod("removeChildren")
}

removeChildren.XMLNode =
  #
  #
function(node, ..., kids = list(...), free = FALSE)
{

  kidNames = names(node)
  w = sapply(kids,
              function(i)  {
                orig = i
                if(length(i) > 1)
                  warning("each node identifier should be a single value, i.e. a number or a name, not a vector. Ignoring ",
                           paste(i[-1], collapse = ", "))
                
                if(!is(i, "numeric"))
                    i = match(i, kidNames)

                if(is.na(i)) {
                  warning("can't find node identified by ", orig)
                  i = 0
                }
                i
              })

  x$children = unclass(x)$children[ - w ]
  x
}  

removeChildren.XMLInternalNode =
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
            function(x, context = NULL) standardGeneric("toHTML"))


setMethod('toHTML', 'vector',
            function(x, context = NULL) {
              tb = newXMLNode("table")
              if(length(names(x)) > 0) 
                addChildren(tb, newXMLNode("tr", .children = sapply(names(x), function(x) newXMLNode("th", x))))


              addChildren(tb, newXMLNode("tr", .children = sapply(x, function(x) newXMLNode("th", format(x)))))
              tb
            })

setMethod('toHTML', 'matrix',
            function(x, context = NULL) {
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



SpecialCallOperators =
  c("+", "-", "*", "/", "%*%", "%in%", ":")

#XXX Not necessarily working yet! See RXMLDoc
setMethod('toHTML', 'call',
            function(x, context) {
                # handle special operators like +, -, :, ...
              if(as.character(v[[1]]) %in% SpecialCallOperators) {

              }

              v = newXMLNode(x[[1]], "(")
              for(i in v[-1]) 
                 addChildren(v, toHTML( i , context))

              v
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
function(doc, file = NULL, compression = 0, indent = TRUE, prefix = '<?xml version="1.0"?>\n',
         doctype = NULL, encoding = "")  
{
  if(encoding == "")
    encoding = character()

  ans = .Call("RS_XML_printXMLNode", doc, as.integer(0), as.integer(indent), as.logical(indent), as.character(encoding))

  if(length(file)) {
    cat(ans, file = file)
    file
  } else
    ans
}

print.XMLInternalDocument =
function(x, ...)
{
  cat(as(x, "character"), "\n")
}

print.XMLInternalNode =
function(x, ...)
{
  cat(as(x, "character"), "\n")
}  


setAs("XMLInternalNode", "character",
          function(from) saveXML.XMLInternalNode(from))

saveXML.XMLInternalDocument <-
function(doc, file = NULL, compression = 0, indent = TRUE,
          prefix = '<?xml version="1.0"?>\n',  doctype = NULL, encoding = "")
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

  if(length(file))
    file = path.expand(file)
  
  ans = .Call("R_saveXMLDOM", doc, file, as.integer(compression), as.logical(indent),
                               as.character(prefix), as.character(encoding))

  if(length(file))
    file
  else
    ans
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
  if(inherits(file, c("character", "connection"))) {
    sink(file)
    on.exit(sink())
  }

  if(!is.null(prefix))
    cat(as.character(prefix))

  if(!is.null(doctype))
    cat(as(doctype, "character"), '\n')

  #XXX Should we return file if it is not missing() || NULL ???
  
  print(doc)
}
  


saveXML.XMLNode = saveXML.sink

saveXML.XMLFlatTree = saveXML.sink

checkAttrNamespaces =
function(nsDefs, .attrs, suppressNamespaceWarning)
{
      ns = sapply(strsplit(names(.attrs), ":"),
                   function(x)  if(length(x) > 1) x[1] else NA)
      i = which(!is.na(ns))
      m = match(ns[i], names(nsDefs))
      if(any(is.na(m))) {
         f = if(is.character(suppressNamespaceWarning))
                get(suppressNamespaceWarning, mode = "function")
             else
                warning

         f(paste("missing namespace definitions for prefix(es)", paste(ns[i][is.na(m)])))
      }
}

setGeneric("addAttributes",
           function(node, ..., .attrs = NULL, suppressNamespaceWarning = getOption('suppressXMLNamespaceWarning', FALSE), append = TRUE)
             standardGeneric("addAttributes"))

setMethod("addAttributes", "XMLInternalElementNode",
function(node, ..., .attrs = NULL, suppressNamespaceWarning = getOption('suppressXMLNamespaceWarning', FALSE), append = TRUE)
{
   if(missing(.attrs)) 
     .attrs = list(...)

   .attrs = structure(as.character(.attrs), names = names(.attrs))

   if(length(.attrs) == 0)
      return(node)
   
   if(is.null(names(.attrs)) || any(names(.attrs) == ""))
     stop("all node attributes must have a name")


   if(is.character(suppressNamespaceWarning) || !suppressNamespaceWarning) 
      checkAttrNamespaces(getEffectiveNamespaces(node), .attrs, suppressNamespaceWarning)

   if(!append)
     removeAttributes(node, .all = TRUE)
   
   .Call("RS_XML_addNodeAttributes", node, .attrs)
   node
})

if(!isGeneric("xmlAttrs<-"))
 setGeneric("xmlAttrs<-", function(node, append = TRUE, suppressNamespaceWarning = getOption('suppressXMLNamespaceWarning', FALSE), value)
                          standardGeneric("xmlAttrs<-"))

tmp =
function(node, append = TRUE, suppressNamespaceWarning = getOption('suppressXMLNamespaceWarning', FALSE), value)
{
   addAttributes(node, .attrs = value, suppressNamespaceWarning = suppressNamespaceWarning, append = append)
   node
}

setMethod("xmlAttrs<-", "XMLInternalElementNode", tmp)
setMethod("xmlAttrs<-", "XMLNode", tmp)


setMethod("addAttributes", "XMLNode",
           function(node, ..., .attrs = NULL, suppressNamespaceWarning = getOption('suppressXMLNamespaceWarning', FALSE), append = TRUE) {
             if(missing(.attrs)) 
               .attrs = list(...)

             .attrs = structure(as.character(.attrs), names = names(.attrs))

             if(is.null(names(.attrs)) || any(names(.attrs) == ""))
               stop("all node attributes must have a name")

             if(is.character(suppressNamespaceWarning) || !suppressNamespaceWarning) 
                 checkAttrNamespaces(getEffectiveNamespaces(node), .attrs, suppressNamespaceWarning)             

             if(append) {
                i = match(names(.attrs), names(node$attributes))
                if(any(!is.na(i))) {
                  node$attributes[i[!is.na(i)]] =  .attrs[!is.na(i)]
                  .attrs = .attrs[is.na(i)]
                }
                node$attributes = c(node$attributes, .attrs)
             } else
                node$attributes = .attrs
             node
           })

setGeneric("removeAttributes", function(node, ..., .attrs = NULL, .namespace = FALSE,
                                        .all = (length(list(...)) + length(.attrs)) == 0)
                                 standardGeneric("removeAttributes"))


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
function(node, ..., .attrs = NULL, .namespace = FALSE,
          .all = (length(list(...)) + length(.attrs)) == 0)
{
   if(missing(.attrs)) 
     .attrs = list(...)
   
   .attrs = as.character(.attrs)

  
   if(.all) {
     if(length(list(...)) || length(.attrs))
         stop(".all specified as TRUE and individual values specified via .../.attrs")
     .attrs = names(xmlAttrs(node))
   }
   

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
function(node, ..., .attrs = NULL, .namespace = FALSE,
         .all = (length(list(...)) + length(.attrs)) == 0)
{
   a = node$attributes

   if(missing(.attrs)) 
     .attrs = list(...)
   
   .attrs = as.character(.attrs)

  if(.all) {
    if(length(.attrs))
      stop("Both individual attribute names and .all specified")
    node$attributes = character()
    return(node)
  }

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
                      newNamespace(node, id, as.character(.values[[id]])))

  names(ans) = ids
  ans
}




xmlNodeMatch =
function(x, table, nomatch = NA_integer_)
{
  .Call("R_matchNodesInList", x, table, as.integer(nomatch))
}  
