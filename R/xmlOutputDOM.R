xmlOutputDOM <-
function(dtd=NULL, nameSpace=NULL)
{
 buf <- xmlNode("doc")
 current <- integer(0)

 reset <-
 function() {
  buf <<- xmlNode("doc")
  current <<- integer(0)
  NULL
 }


 addTag <- 
 function(tag, ..., attrs=NULL, close=T, namespace=NULL) {
   if(missing(namespace))
     namespace <- nameSpace

   addNode(n <- xmlNode(tag, ..., attrs= attrs, namespace=namespace))
   if(close == F) {
     current <<- c(current, xmlSize(getCurrent()))
   }

  invisible(n)
 }

 getCurrentExpr <- 
 function() {
   if(length(current) > 0) {
     p <- seq(2, length=length(current)-1)
     kall <- call("[[", as.name("buf"), current[1])
     for(i in p) {
       kall <- call("[[", kall, current[i])
     }
   } else
     kall <- as.name("buf")
     
   kall
 }

 getCurrent <- function() {
    eval(getCurrentExpr())
 }

  # We want to append this to the currently open (or active)
  # node as defined by `current'
  #   d[[1]] <- append.xmlNode(d[[1]], xmlNode("phone"))
 addNode <- 
 function(node) {
   kall <- getCurrentExpr()
  if(length(current) > 0){
   lhs <- kall
   kall <- call("append.xmlNode", kall, node)
   kall <- call("<<-", lhs, kall)
  } else {
   kall <- call("append.xmlNode", kall, node)
  }
   val <- eval(kall) 
   if(length(current) == 0)
     buf <<- val

  invisible(node)
 }

 closeTag <-
 function(name="")  {
   current <<- current[-length(current)]
 }

 con <- list( value=function() {buf},
              addTag = addTag,
              closeTag = closeTag,
              reset = reset,
              addNode = addNode,
              current = function(){current}
            ) 
  class(con) <- c("XMLOutputDOM", "XMLOutputStream")

 return(con)
}
