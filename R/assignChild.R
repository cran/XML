"[<-.XMLNode" <-
function(x,i,value)
{
  x$children[i] <- value
 x
}


"[[<-.XMLNode" <-
function(x,i,value)
{
  x$children[[i]] <- value
 x
}


append.xmlNode <-
function(to, ...)
{
 UseMethod("append")
}

append.XMLNode <-
function(to, ...)
{
 args <- list(...)
 if(!inherits(args[[1]], "XMLNode") && is.list(args[[1]]))
   args <- args[[1]]
    
 idx <- seq(length(to$children) + 1, length=length(args))

 if(is.null(to$children))
   to$children <- args
 else  {
   to$children[idx] <- args  
#   names(to$children)[idx] <- names(args)
 }
 to
}
