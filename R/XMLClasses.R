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
function(node)
{
  UseMethod("xmlName", node)
}

xmlName.XMLComment <-
function(node) {
 return("comment")
}

xmlName.XMLNode <-
#
# Get the XML tag name of an XMLNode object
#
function(node)
{
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
 print("[.XMLNode")
 obj <- obj$children
 NextMethod("[")
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


xmlSize <-
#
# The number of elements within (or length of) a collection
#
function(obj)
{
 UseMethod("xmlSize", obj)
}

xmlSize.XMLDocument <-
function(doc)
{
 return(length(doc$doc$children))
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


print.XMLNode <-
#
# displays a node and attributes (and its children)
# in its XML format.
# 
function(x,...)
{
 if(xmlName(x) == "text" || xmlName(x) == "comment") {
   cat(x$value,"\n")
   return()
 }

 if(! is.null(xmlAttrs(x))) {
   tmp <- paste(names(xmlAttrs(x)),paste("\"", xmlAttrs(x),"\"", sep=""), sep="=", collapse=" ")
 } else 
    tmp <- ""

 cat(paste("<",xmlName(x),ifelse(!is.null(xmlAttrs(x))," ",""),tmp,">\n", sep=""))
  for(i in xmlChildren(x)) {
     print(i)
  }
 cat(paste("</",xmlName(x),">\n",sep=""))
}

print.XMLEntityRef <-
function(node)
{
 cat(node$value)
}

print.XMLProcessingInstruction <-
function(node)
{
 cat(paste("<?", node$name," ", node$value, ">\n", sep=""))
}


xmlElementsByTagName <-
#
# Extract all the sub-nodes within an XML node
# with the tag name `name'.
#
function(el, name) {
  idx <-  (names(el$children) == name)
      el$children[idx]
}

