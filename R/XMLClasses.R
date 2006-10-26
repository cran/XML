#
# This file contains the definitions of methods
# for operating on the XMLNode objects to make
# the more user-friendly.  Specifically, these
# methods are 
#       print   displays the contents of a node and children
#               as XML text rather than R/S list
#
#       size    returns the number of children
#
#       name    retrieves the tag name
#
#       attrs   retrieves the attributes element of the XML node
#
#    [ and [[   access the children 
#                 (To get at the regular R/S fields in the object, use $
#                    e.g.  node$name, node$attributes, node$value)

#
# In S4/Splus5, we should use the new class mechanism.
#

xmlChildren <-
function(x)
{
 UseMethod("xmlChildren")
}

xmlChildren.XMLNode <-
#
# Retrieve the list of children (sub-nodes) within
# an XMLNode object.
#
function(x)
{
  x$children
}

xmlName <-
#
#
#
function(node, full = FALSE)
{
  UseMethod("xmlName", node)
}

xmlName.XMLComment <-
function(node, full = FALSE) {
 return("comment")
}

xmlName.XMLNode <-
#
# Get the XML tag name of an XMLNode object
#
function(node, full = FALSE)
{
  if(full && !is.null(node$namespace) && node$namespace != "") {
    tmp <- ifelse(is.character(node$namespace), node$namespace, node$namespace$id)
    paste(tmp, node$name, sep=":")
  }
  else
    node$name
}

xmlAttrs <-
function(node)
{
  UseMethod("xmlAttrs", node)
}

xmlAttrs.XMLNode <-
#
# Get the named list of attributes
# for an XMLNode object.
#
function(node)
{
 node$attributes
}



"[.XMLNode" <-
#
# Extract the  children (sub-nodes) within
# the specified object identified by ...
# and return these as a list
#
function(obj, ...)
{
 obj <- obj$children
 NextMethod("[")
}

"[[.XMLDocumentContent" <-
function(obj, ...) 
{
  obj$children[[...]]
}

"[[.XMLNode" <-
#
# Extract the  children (sub-nodes) within
# the specified object identified by ...
#
function(obj, ...)
{
# print("[.XMLNode")
 obj <- obj$children
 NextMethod("[[")
}

names.XMLNode <-
function(x)
{
 names(xmlChildren(x))
}

length.XMLNode <-
function(x)
{
  xmlSize(x)
}

xmlSize <-
#
# The number of elements within (or length of) a collection
#
function(obj)
{
 UseMethod("xmlSize", obj)
}

xmlSize.XMLDocument <-
function(obj)
{
 return(length(obj$doc$children))
}

xmlSize.default <-
#
# The number of elements within (or length of) a collection
#
function(obj)
{
  length(obj)
}

xmlSize.XMLNode <-
#
# Determine the number of children (or sub-nodes) within an XML node.
#
function(obj)
{
  length(obj$children) 
}


print.XMLComment <- print.XMLCommentNode <-
function(x, ..., indent = "", tagSeparator = "\n")
{
  if(is.logical(indent) && !indent)
    indent <- ""
  
  cat(indent, "<!--", xmlValue(x), "-->", tagSeparator, sep="")
}

print.XMLTextNode <-
function(x, ..., indent = "", tagSeparator = "\n")
{
  if(is.logical(indent) && !indent)
    indent <- ""

  if(inherits(x, "EntitiesEscaped"))
    txt = xmlValue(x)
  else
    txt = insertEntities( xmlValue(x) )
  
  cat(indent, txt, tagSeparator, sep="")
}  

print.XMLNode <-
#
# displays a node and attributes (and its children)
# in its XML format.
# 
function(x, ..., indent = "", tagSeparator = "\n")
{
 if(! is.null(xmlAttrs(x))) {
   tmp <- paste(names(xmlAttrs(x)),paste("\"", xmlAttrs(x),"\"", sep=""), sep="=", collapse=" ")
 } else 
   tmp <- ""

 if(!is.null(x$namespaceDefinitions)) {
   ns <- paste(sapply(x$namespaceDefinitions, 
                       function(x) {
                            paste("xmlns", ifelse(nchar(x$id) > 0, ":", ""), x$id, "=", "\"", x$uri, "\"", sep="")
                       }), collapse=" ")

 } else 
   ns <- ""


   # Add one space to the indentation level for the children.
   # This will accumulate across successive levels of recursion. 
  subIndent <- paste(indent, " ", sep="")
  if(is.logical(indent) && !indent) {
    indent <- ""
    subIndent <- FALSE
  }


    if (length(xmlChildren(x)) == 0) {
      ## Empty Node - so difference is <nodename />
      cat(indent, paste("<", xmlName(x, TRUE), ifelse(tmp != "", 
          " ", ""), tmp, ifelse(ns != "", " ", ""), ns, "/>", tagSeparator, 
          sep = ""), sep = "")
    } else if (length(xmlChildren(x))==1 &&
               is(xmlChildren(x)[[1]],"XMLTextNode")) {
      ## Sole child is text node, print without extra white space.
      cat(indent, paste("<", xmlName(x, TRUE), ifelse(tmp != "", 
          " ", ""), tmp, ifelse(ns != "", " ", ""), ns, ">",
          sep = ""), sep = "")
      kid = xmlChildren(x)[[1]]
      if(inherits(kid, "EntitiesEscaped"))
        txt = xmlValue(kid)
      else
        txt = insertEntities( xmlValue(kid) )
      
      cat(txt,sep="")
      cat(paste("</", xmlName(x, TRUE), ">", tagSeparator, 
          sep = ""), sep = "")
    } else {
      cat(indent, paste("<", xmlName(x, TRUE), ifelse(tmp != "", 
          " ", ""), tmp, ifelse(ns != "", " ", ""), ns, ">", tagSeparator, 
          sep = ""), sep = "")
      for (i in xmlChildren(x))
        print(i, indent = subIndent, tagSeparator = tagSeparator)
      cat(indent, paste("</", xmlName(x, TRUE), ">", tagSeparator, 
          sep = ""), sep = "")
    }

  
##  cat(indent, paste("<",xmlName(x, TRUE),
##                        ifelse(tmp != "", " ", ""), tmp,
##                        ifelse(ns != "", " ", ""), ns,
##                    ">",
##                    tagSeparator,
##                    sep=""), sep = "")
##  
##  
##   for(i in xmlChildren(x)) 
##      print(i, indent = subIndent, tagSeparator = tagSeparator)
##  
##  cat(indent, paste("</", xmlName(x, TRUE), ">", tagSeparator, sep=""), sep = "")
}

print.XMLEntityRef <-
function(x, ..., indent="", tagSeparator = "\n")
{
  if(is.logical(indent) && !indent)
    indent <- ""
  
  cat(indent, x$value)
}



print.XMLCDataNode <-
function(x, ..., indent="", tagSeparator = "\n")
{
  if(is.logical(indent) && !indent)
    indent <- ""
  
 cat(indent, "<![CDATA[", tagSeparator, sep = "")
   # Want new lines in value to be replaced by paste("\n", indent, sep="")
 cat(indent, x$value, sep = "")
 cat(indent, "]]>", tagSeparator, sep = "")
}


print.XMLProcessingInstruction <-
function(x, ..., indent="", tagSeparator = "\n")
{
  if(is.logical(indent) && !indent)
    indent <- ""
  
 cat(indent, paste("<?", x$name," ", x$value, "?>", tagSeparator, sep=""), sep = "")
}


xmlElementsByTagName <-
#
# Extract all the sub-nodes within an XML node
# with the tag name `name'.
#
function(el, name, recursive = FALSE)
{
    kids = xmlChildren(el)
    idx  =  (names(kids) == name)
    els  = kids[idx]    
#    idx <-  (names(el$children) == name)
#    els = el$children[idx]

    if(!recursive  || xmlSize(el) == 0)
      return(els)
    
    subs = xmlApply(el, xmlElementsByTagName, name, TRUE)
    subs = unlist(subs, recursive = FALSE)
    
    append(els, subs[!sapply(subs, is.null)])
  }



getNodeSet =
function(doc, path, namespaces = character(), fun = NULL, ...)
{
  xpathApply(doc, path, fun, ...,  namespaces = namespaces)
}  

xpathApply =
function(doc, path, fun, ... , namespaces = character())
{
  if(!is.character(namespaces) || ( length(namespaces) > 0 && length(names(namespaces)) == 0))
     stop("Namespaces must be a named character vector")

  if(!is.null(fun) && !is.call(fun))
    fun = match.fun(fun)

  # create an expression of the form fun(x, ...) and the C code will insert x for each node.
  args = list(...)
  if(length(args))  
    fun = as.call(c(fun, append(1, args)))

  .Call("RS_XML_xpathEval", doc, as.character(path), namespaces, fun)  
}
