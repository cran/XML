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
function(x, ..., indent = "")
{
 if(xmlName(x) == "text" || xmlName(x) == "comment") {
   cat(indent, x$value,"\n", sep="")
   return()
 }

 if(! is.null(xmlAttrs(x))) {
   tmp <- paste(names(xmlAttrs(x)),paste("\"", xmlAttrs(x),"\"", sep=""), sep="=", collapse=" ")
 } else 
   tmp <- ""

 cat(indent, paste("<",xmlName(x),ifelse(!is.null(xmlAttrs(x))," ",""),tmp,">\n", sep=""))
   # Add one space to the indentation level for the children.
   # This will accumulate across successive levels of recursion.
  subIndent <- paste(indent, " ", sep="")
  for(i in xmlChildren(x)) {
     print(i, indent= subIndent)
  }
 cat(indent, paste("</",xmlName(x),">\n",sep=""))
}

print.XMLEntityRef <-
function(x, ..., indent="")
{
 cat(indent, x$value)
}


print.XMLCDataNode <-
function(x, ..., indent="")
{
 cat(indent, "<![CDATA[\n")
   # Want new lines in value to be replaced by paste("\n", indent, sep="")
 cat(indent, x$value)
 cat(indent, "]]>\n")
}


print.XMLProcessingInstruction <-
function(x, ..., indent="")
{
 cat(indent, paste("<?", x$name," ", x$value, ">\n", sep=""))
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

