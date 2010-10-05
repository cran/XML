if(FALSE) {
saveXML <-
function(doc, file=NULL, compression=0, indent=TRUE, prefix = '<?xml version="1.0"?>\n',
         doctype = NULL, encoding = "", ...)
{
 UseMethod("saveXML")
}
}


saveXML.XMLInternalNode <-
function(doc, file = NULL, compression = 0, indent = TRUE, prefix = '<?xml version="1.0"?>\n',
         doctype = NULL, encoding = "", ...)  
{
  if(encoding == "")
    encoding = character()

  ans = .Call("RS_XML_printXMLNode", doc, as.integer(0), as.integer(indent), as.logical(indent), as.character(encoding), PACKAGE = "XML")

  if(length(file)) {
    cat(ans, file = file)
    file
  } else
    ans
}



saveXML.XMLInternalDocument <-
function(doc, file = NULL, compression = 0, indent = TRUE,
          prefix = '<?xml version="1.0"?>\n',  doctype = NULL, encoding = "", ...)
{
  havePrefix = !missing(prefix)

  isDocType = is(doctype, "Doctype")
  if(isDocType) {
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
                               if(is.character(prefix)) prefix else character(), 
                                as.character(encoding), PACKAGE = "XML")

  if(!isDocType && havePrefix) {

      prefix = as(prefix, "character") # allow for an XMLInternalNode.

     if(length(file)) {
         txt = c(prefix, readLines(file)[-1])
         cat(txt, file = file)
     } else {
         tmp = strsplit(ans, "\\\n")[[1]]
         tmp = c(prefix, tmp[-1])
         ans = paste(tmp, collapse = "\n")
     }

  }

  if(length(file))
    file
  else
    ans
}

saveXML.XMLInternalDOM <-
function(doc, file=NULL, compression=0, indent=TRUE, prefix = '<?xml version="1.0"?>\n',
         doctype = NULL, encoding = "", ...)
{
  saveXML(doc$value(), file, compression, indent, prefix, doctype, encoding)
}


saveXML.XMLOutputStream =
function(doc, file = NULL, compression = 0, indent = TRUE, prefix = '<?xml version="1.0"?>\n',
         doctype = NULL, encoding = "", ...)
{
  saveXML(doc$value(), file, compression, indent, prefix, doctype, encoding)  
}


saveXML.sink =
#
# Need to handle a DTD here as the prefix argument..
#
function(doc, file = NULL, compression = 0, indent = TRUE, prefix = '<?xml version="1.0"?>\n',
         doctype = NULL, encoding = "", ...)
{
  asString = is.null(file)
  if(asString)
    file = textConnection(NULL, "w")
  
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

  if(asString)
    textConnectionValue(file)
  else
    file
}


saveXML.XMLNode = saveXML.sink

saveXML.XMLFlatTree = saveXML.sink



setGeneric("saveXML",
function(doc, file=NULL, compression=0, indent=TRUE, prefix = '<?xml version="1.0"?>\n',
         doctype = NULL, encoding = "", ...)
           standardGeneric("saveXML"))

setMethod("saveXML", "XMLInternalNode", saveXML.XMLInternalNode)
setMethod("saveXML", "XMLInternalDocument", saveXML.XMLInternalDocument)
setMethod("saveXML", "XMLInternalDOM", saveXML.XMLInternalDOM)
setMethod("saveXML", "XMLOutputStream", saveXML.XMLOutputStream)
setMethod("saveXML", "XMLNode", saveXML.sink)
setOldClass("XMLFlatTree")
setOldClass(c("XMLFlatListTree", "XMLFlatTree"))
setMethod("saveXML", "XMLFlatTree", saveXML.sink)


setMethod("saveXML", "HTMLInternalDocument",
          function(doc, file = NULL, compression = 0, indent = TRUE,
             prefix = '<?xml version="1.0"?>\n',  doctype = NULL, encoding = "", ...) {
               ans = .Call("RS_XML_dumpHTMLDoc", doc, as.integer(indent), as.character(encoding), as.logical(indent), PACKAGE = "XML")

               if(length(file)) {
                 cat(ans, file = file)
                 file
               } else
                 ans
          })
