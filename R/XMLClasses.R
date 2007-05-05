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
  if(!full || is.null(node$namespace) || node$namespace == "")
    return(node$name)
  
  # 
  if(!is.character(node$namespace)) {
    tmp = node$namespace$id
  } else if(inherits(node$namespace, "XMLNamespace"))
    tmp = names(node$namespace)
  else
    tmp = node$namespace

  if(length(tmp))
     paste(tmp, node$name, sep=":")
  else
     node$name
}

xmlAttrs <-
function(node, ...)
{
  UseMethod("xmlAttrs", node)
}

xmlAttrs.XMLNode <-
#
# Get the named list of attributes
# for an XMLNode object.
#
function(node, ...)
{
 node$attributes
}



"[.XMLNode" <-
#
# Extract the  children (sub-nodes) within
# the specified object identified by ...
# and return these as a list
#
function(obj, ..., all = FALSE)
{
 obj <- obj$children

 if(all) # "all" %in% names(list(...)) && list(...)[["all"]] == TRUE)
   obj[ names(obj) %in% list(...)[[1]] ]
 else
   obj[...] # NextMethod("[") 
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

"names<-.XMLNode" <-
function(x, value)
{
 names(x$children) <- value
 x
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


getDefaultNamespace =
function(doc)
{
  ns = xmlNamespaceDefinitions(doc)
  val = unlist(sapply(ns, function(x) if(x$id == "") x$uri))
  if(length(val))
     val[1]
  else   
     character()
}  

matchNamespaces =
  # d = xmlTreeParse("data/namespaces.xml", useInternal = TRUE)

  #   "omg"
  #   c("ns", "omegahat", "r")
  #   c("ns", omegahat = "http://www.omegahat.org", "r")  
  #   c("ns" = "http://www.omegahat.org", "omg" = "http://www.omegahat.org/XML", "r")
  #
  #  Error because z and rs are not defined in the document.
  #  matchNamespaces(d, c("omg", "z", "rs"))
  #
  #
function(doc, namespaces,
          nsDefs = xmlNamespaceDefinitions(doc, recursive = TRUE),
          defaultNs = getDefaultNamespace(doc)
        )
{


    # 3 cases:
    #  i) we are given a single string (e.g. "r") which we use as a prefix for the default namespace
    # ii) we are given a vector of namespaces, but one has no name and we want to use that as the
    #  prefix for the default namespace
    #    e.g.  sum(names(namespaces) == "") == 1)
    # iii) given several elements with no name and we match these to those in the document
    #    if the first doesn't have a match, we use it as the default one.
    # iv) mixture of prefix = uri values and strings with no names.
    #

    # if it is a single "prefix" and we have a default namespace, then map the prefix to the default URI
    # and return.
  if(is.character(namespaces) && length(namespaces) == 1 && is.null(names(namespaces)) && length(defaultNs) > 0) {
       tmp = defaultNs
       names(tmp)[names(tmp) == ""] = namespaces
       namespaces = tmp
       return(namespaces)       
   }

    # fix the names so that we empty ones.
  if(is.null(names(namespaces)))
    names(namespaces) = rep("", length(namespaces))

   # which need to be fixed.
  i = (names(namespaces) == "")
  
  if(any(i)) {

     # from parameters now: nsDefs = xmlNamespaceDefinitions(xmlRoot(doc), recursive = TRUE)

      # deal with the first one as a special case. If this has no match,
      # we will map it to the default namespace's URI.
    if(i[1] && is.na(match(namespaces[1], names(nsDefs)))) {
      names(namespaces)[1] = namespaces[1]
      namespaces[1] = defaultNs
      warning("using ", names(namespaces)[i[1]], " as prefix for default namespace ", namespaces[i[1]])
      i[1] = FALSE
    }
    
    if(sum(i) > 0) {
        dups = names(nsDefs)[duplicated(names(nsDefs))]
        tmp = match(namespaces[i], dups)
        if(length(dups) > 0 && any(is.na(tmp)))
          stop("cannot match namespace prefix(es) ",
                paste(namespaces[i][is.na(tmp)], collapse = ", "),
                " in ", paste(unique(names(nsDefs)), collapse= ", "))

        idx = match(namespaces[i], names(nsDefs))
        if(any(is.na(idx)))
           stop("cannot find defined namespace(s) with prefix(es) ", paste(namespaces[i][is.na(idx)], collapse = ", "))
        names(namespaces)[i] = namespaces[i]
        namespaces[i] = sapply(nsDefs[idx], function(x) x$uri)
      
      #  warning("namespaces without a name/prefix are not handled as you might expect in XPath. Use a prefix")
    } else if(length(defaultNs) == 0)
        stop("There is no default namespace on the target XML document")
  }

  
  if(!is.character(namespaces) || ( length(namespaces) > 1 && length(names(namespaces)) == 0))
     stop("Namespaces must be a named character vector")

  if(length(namespaces) && (length(names(namespaces)) == 0 || any(names(namespaces) == "")))
     warning("namespaces without a name/prefix are not handled as you might expect in XPath. Use a prefix")

  namespaces
}  



getNodeSet =
function(doc, path, namespaces = getDefaultNamespace(doc), fun = NULL, ...)
{
  xpathApply(doc, path, fun, ...,  namespaces = namespaces)
}



xpathApply =
  #
  # the caller can give the same prefixes of the namespaces defined in 
  # the target document as simple names.
  #
  #   xpathApply(d, "/o:a//c:c", fun = NULL, namespaces = c("o", "c"))
  #
  #
function(doc, path, fun = NULL, ... , namespaces = getDefaultNamespace(doc),
          resolveNamespaces = TRUE)
{
  UseMethod("xpathApply")
}  

xpathApply.XMLInternalDocument =
function(doc, path, fun = NULL, ... , namespaces = getDefaultNamespace(doc),
          resolveNamespaces = TRUE)
{
#  if(!inherits(doc, "XMLInternalDocument"))
#    stop("Need XMLInternalDocument object for XPath query")

  if(resolveNamespaces)
    namespaces = matchNamespaces(doc, namespaces)
  
  if(!is.null(fun) && !is.call(fun))
    fun = match.fun(fun)

  # create an expression of the form fun(x, ...) and the C code will insert x for each node.
  args = list(...)
  if(length(args))  
    fun = as.call(c(fun, append(1, args)))


  ans = .Call("RS_XML_xpathEval", doc, as.character(path), namespaces, fun)

  if(length(ans) == 0 && length(getDefaultNamespace(xmlRoot(doc))) > 0) {
    tmp = strsplit(path, "/")[[1]]
       # if they have a function call, ignore.
    tmp = tmp[ - grep("\\(", path) ]
    if(length(grep(":", tmp)) != length(tmp))
        warning("the XPath query has no namespace, but the target document has a default namespace. This is often an error and may explain why you obtained no results")
  }

  ans
}


xmlDoc =
function(node)
{
  .Call("RS_XML_createDocFromNode", node)
}


xpathApply.XMLInternalNode =
function(doc, path, fun = NULL, ... , namespaces = getDefaultNamespace(doc),
          resolveNamespaces = TRUE)
{
  if(FALSE) {
    node = doc
    doc = as(doc, "XMLInternalDocument")
    ns = getDefaultNamespace(doc)
    prefix = ""
    if(length(ns)) 
      prefix = if(length(names(ns)))  names(ns)[1] else "ns"

    path = paste(XML:::getXMLPath(node, prefix), path, sep = "/")
  } else {
    doc = xmlDoc(doc)
  }

  xpathApply(doc, path, fun, ..., namespaces = namespaces, resolveNamespaces = resolveNamespaces)
}



# d = xmlTreeParse("data/book.xml", useInternal = TRUE)
# ch = getNodeSet(d, "//chapter")
# xpathApply(ch[[1]], "//section/title", xmlValue)


# d = xmlTreeParse("data/mtcars.xml", useIntern = TRUE); z = getNodeSet(d, "/dataset/variables")
#  xpathApply(z[[1]], "variable[@unit]", NULL, namespaces = character())

getXMLPath =
function(node, defaultPrefix = "ns")
{
  paste(unlist(c("", XML:::xmlAncestors(node, xmlName, defaultPrefix))), collapse = "/")
}

xmlAncestors =
function(x, fun = NULL, ...)
{
  ans = list()
  tmp = x
  while(!is.null(tmp)) {
    if(!is.null(fun))
      ans = c(fun(tmp, ...), ans)
    else
      ans = c(tmp, ans)
    tmp = xmlParent(tmp)    
  }
  ans
}  
