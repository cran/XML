setGeneric("simplifyNamespaces",
function(doc, ...)
  standardGeneric("simplifyNamespaces"))

setMethod("simplifyNamespaces", "character",
            function(doc, ...) {
               pdoc = xmlParseDoc(doc, NSCLEAN)
               simplifyNamespaces(pdoc, ...)
            })   

setMethod("simplifyNamespaces", "XMLInternalDocument",
            function(doc, alreadyCleaned = FALSE, ...) {           
browser()
                 # find all the nodes, but discard the root node.
               allNodes = getNodeSet(doc, "//node()") # [-1]
               root = xmlRoot(doc)

                 # For each node, get its namespace definitions,
                 # and then zoom in on the nodes that have namespace definitions.
               nsDefs = lapply(allNodes, xmlNamespaceDefinitions, simplify = TRUE)
               w = sapply(nsDefs, length) > 0
               tmp = structure(unlist(nsDefs[w]), names = sapply(nsDefs[w], names))


               d = data.frame(uri = tmp, prefix = names(tmp), stringsAsFactors = FALSE)
   
               multi = unlist(by(d, d$prefix, function(x) if(length(unique(x$uri)) == 1) character() else x$prefix[1]))
               if(length(multi))
                 d = d[ ! (d$prefix %in% multi), ]

               #  Now we can move these namespace definitions to the top.
               #
               #
               #
               by(d, nsDefs,
                               function(x) {
                                    u = unique(x$prefix)
                               })
             
                  # remove the 
               sapply(allNodes[w], removeXMLNamespaces)
               
               nsDefs
})
