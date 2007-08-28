
#XXX Deal with line numbers in the original document.

  #
  # We could also do things this way
  #  source(textConnection(saveXML(xsltApplyStyleSheet("cityTemperatures.xml", "~/Projects/org/omegahat/XML/Literate/segment.xsl"))))
  #
  #
  # Allow the user to specify a subset of nodes in which to find the code, etc. nodes.
  # Or an XPath query to restrict the search.
  # For example, suppose we have a document with two sections and we want to run the code
  # in only one of those sections.
  #  getNodeSet(section[@id='second one'])
  #

setOldClass("XMLNodeSet")

DefaultXPathNamespaces = 
                 c(r = "http://www.r-project.org",
                   s = "http://cm.bell-labs.com/stat/S4",
                   omg = "http://www.omegahat.org",
                 
                   mlb = "http://www.mathworks.com",  # matlab

                   sh="http://www.shell.org",
                   perl = "http://www.perl.org",
                   py = "http://www.python.org",  

                   fo="http://www.w3.org/1999/XSL/Format",
                   xsl="http://www.w3.org/1999/XSL/Transform",
                   xi="http://www.w3.org/2001/XInclude"                   
                  )

setGeneric("xmlSource",
function(url, ...,
          envir =globalenv(),
          xpath = character(),
          ids = character(),
          omit = character(),
          ask = FALSE,
          example = NA,
          fatal = TRUE, verbose = FALSE,
          xnodes = c("//r:function", "//r:init[not(@eval='false')]", "//r:code[not(@eval='false')]", "//r:plot[not(@eval='false')]"),         
          namespaces = DefaultXPathNamespaces)
{

  standardGeneric("xmlSource")
})



# Break up into methods for character and xmlInternalDocument


setMethod("xmlSource", c("character"),
function(url, ..., envir =globalenv(),
          xpath = character(),         
          ids = character(),
          omit = character(),
          ask = FALSE,
          example = NA,         
          fatal = TRUE, verbose = FALSE,
          xnodes = c("//r:function", "//r:init[not(@eval='false')]", "//r:code[not(@eval='false')]", "//r:plot[not(@eval='false')]"),
          namespaces = DefaultXPathNamespaces)
{
  library(XML)
  doc = xmlTreeParse(url, ..., useInternal = TRUE)
#  on.exit(free(doc))

  if(is(verbose, "numeric"))
    verbose = verbose - 1
  
  if(length(example))  {
    egs = getNodeSet(doc, "//r:example", namespaces)
    if(length(egs)) {
      ids = sapply(egs, xmlGetAttr, "id")
      if(length(example) == 1 && is.na(example)) {
        cat("Select an example\n")
        example = ids[w <- menu(ids)]
      } 

      if(is(example, "numeric")) {
        i = example
      } else {
        i = pmatch(example, ids)
        if(all(is.na(i)))
          stop("no example named ", example)
      }

       # find any r:init nodes which are not inside an example.
      init = getNodeSet(doc, "//r:init[not(ancestor::r:example)]", namespace = c(r = "http://www.r-project.org"))
      if(length(init)) {
        xmlSource(init, envir = envir, omit = omit, verbose = verbose, namespaces = namespaces)
        cat("Done doc-level init", length(init), "\n")
      }
      
      ans = sapply(i, function(x) {
                        nodes = getNodeSet(egs[[x]], paste(xnodes, collapse = "|"), namespaces)
                        if(verbose) 
                          cat("Example", ids[x], "\n")

                             #XXX put the correct ids in her.
                        xmlSource(nodes, envir = envir, omit = omit, verbose = verbose, namespaces = namespaces)
                        
                      })
      return(ans)
   }
  }
  
  if(length(xpath)) {
       # do an XPath query and then look inside the resulting nodes
       # for the xnodes of interest.
    nodes = getNodeSet(doc, xpath, namespaces)
    v =
      unlist(lapply(nodes, function(n) {
                     unlist(lapply(xnodes,
                                   function(p)
                                      getNodeSet(n, p, namespaces)),
                             recursive = FALSE)
                  }), recursive = FALSE)
  } else {
     v = getNodeSet(doc, "//r:function", namespaces)
     w = getNodeSet(doc, paste(xnodes, collapse = "|"), namespaces)
     v = c(v, w)
  }

  class(v) <- "XMLNodeSet"

    # deal with a top-level node r:codeIds which is of the form
    #   abc
    #   def
    #   ghi
    # i.e. a single entry on each line which identifies the nodes that are to be read.
  if(missing(ids) && missing(xnodes) && length(ids <- getNodeSet(doc, "/*/r:codeIds|/*/invisible/r:codeIds",
                                               namespaces = c(r = "http://www.r-project.org")))) {

     if(length(ids) > 1) {
       warning("more than one r:codeIds node. Using the first one")
     }
#     txt = paste(sapply(ids, xmlValue))
     ids = strsplit(xmlValue(ids[[1]]), "\\\n")[[1]]
     ids = unique(ids)
     ids = ids[ids != ""]
   }

  xmlSource(v, ids = ids, omit = omit, ask = ask, fatal = fatal, verbose = verbose, envir = envir)
})

setMethod("xmlSource", "XMLNodeSet",
function(url, ..., envir =globalenv(),
          xpath = character(),         
          ids = character(),
          omit = character(),
          ask = FALSE,
          example = NA,         
          fatal = TRUE, verbose = FALSE,
          xnodes = c("r:function", "r:init[not(@eval='false')]", "r:code[not(@eval='false')]", "//r:plot[not(@eval='false')]"),         
          namespaces =  DefaultXPathNamespaces)
{
  if(ask) {
     doc = as(url[[1]], "XMLInternalDocument")     #XXXX  no doc here now.
     v = getNodeSet(doc, "//r:function|//r:init|//r:code|//r:plot", namespaces)
     funs = sapply(v, xmlName) == "function"
     if(any(funs)) {
#XXX
     }
   }
  
  ans = sapply(url, evalNode, envir = envir, verbose = verbose, ids = ids, omit = omit)
  invisible(ans)
})


evalNode = 
function(node, envir = globalenv(), ids = character(), verbose = FALSE, omit = character(),
         namespaces = c(r = "http://www.r-project.org"))
{
            #XXX check all ancestors. Ideally exclude them in the XPath query
   if(xmlName(xmlParent(node)) == "ignore")
     return(FALSE)

   tmp = xmlGetAttr(node, "id", NA)
   if(is.na(tmp) && length(ids) > 0 && !("" %in% ids))
     return()
   
   if(!is.na(tmp)) {
     if(length(omit) > 0 && tmp %in% omit) {
       if(verbose)
         warning("skipping id ", tmp)
       return()
     } else if(length(ids) > 0 && !(tmp %in% ids)) {
       if(verbose)
         warning("ignoring id ", tmp)
       return()
     }
   }

   tmp = xmlGetAttr(node, "ignore", NA, converter = as.logical)
   if(!is.na(tmp) && tmp) {
     if(verbose)
        warning("ignoring node", as(node, "character"))
     return()
   }

     # go through the node and see if there are any r:code nodes
     # with a ref attribute
     # and go fetch the corresponding <r:code id='@ref'> node.
   txt = paste(xmlSApply(node,
                          function(x) {

                            if(inherits(x, c("XMLInternalCommentNode", "XMLInternalPINode"))) {

                            } else if(is(x, "XMLInternalElementNode") && xmlName(x, full = TRUE) == "r:code") {
                               ref = xmlGetAttr(x, "ref")
                               if(!is.na(ref)) {
                                   v = getNodeSet(as(x, "XMLInternalDocument"),
                                                  paste(sapply(c("code", "frag"),
                                                                function(x) paste("//r:", x , "[@id='", ref, "']", sep = "")), collapse = "|"),
                                                  namespaces)
                                   xmlValue(v[[1]])
                               } else
                                   xmlValue(x)                                 
                            } else if(is(x, "XMLInternalElementNode") && xmlName(x, full = TRUE) == "r:output") {
                            }  else
                               xmlValue(x)
                          }),
                collapse = "\n")

#   txt = xmlValue(node)
   if(verbose)
     cat("*************\nEvaluating node\n", txt, "\n")
   cmd = parse(text = txt)
   eval(cmd, globalenv())
}  
