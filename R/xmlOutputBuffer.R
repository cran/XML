xmlOutputBuffer <-
#
#
# Want to check with the DTD whether a tag is legitimate
#  attributes are valid, etc.
#
# Add an indentation level.
#
#  Need to escape characters via entities:
#      <-   => %sgets;
#      <    => %lt;
#      >    => %gt;
#   etc.
#
#
# Allow xmlEndTag with no argument to allow closing the current one.
#  (Maintain a stack)
#
# Allow addTag(tag, addTag(), addTag(),)
#
#
#  The handling of the connection (i.e. the buf argument) can
# be cleaned up using the OOP package from Omegahat. This will be done
# in the future.
#
#
# sapply(names(nameSpace), function(i, x){paste("xmlns:",i,"=\"",x[[i]],"\"", sep="")}, x=nameSpace)
#
#
#
function(dtd = NULL, nameSpace=NULL, buf=NULL, nsURI=NULL,
         header="<?xml version=\"1.0\"?>")
{
    # If the user gave as an existing buffer and header is non-NULL,
    # we appendd it to the buffer. This can be used for adding things
    # like section breaks, etc.
    #
    # If the user did not give us a buffer, then we use the header.
    #
    # This is done immediately the function is called, rather than
    # in the calls to the functions of the closure after it is returnd.
  if(is.null(buf))
    buf <- header
  else if(!is.null(header))
    cat(header,"\n", sep="", file=buf)


  openTags <- NULL  #list()
  lastTag <- 0


   # This is called from addTag() when the tag being 
   # emitted into the stream is left open by that call.
   # We store the tag name, its namespace and the URI of the
   # namespace if there is one in this call.
   # This triple is appended as the last row of the openTags
   # matrix and lastTag is incremented.
  addOpenTag <- function(tag, ns, xmlns) {
    lastTag <<- lastTag+1
    if( lastTag == 1 ) {
      rval <- matrix(c(tag,ifelse(is.null(ns),"",ns),ifelse(is.null(xmlns),"",xmlns)), nr=1,
                     dimnames=list(NULL, c("tagname","nsprefix", "nsURI")) )
    } else
      rval <- rbind(openTags, c(tag, ifelse(is.null(ns),openTags[lastTag-1,2], ns), ifelse(is.null(xmlns),"",xmlns)))
    openTags <<- rval
  }


  checkNamespace <- function(ns) {
    return(TRUE)

    if( (lastTag == 0) )
      stop(paste("Namespace `",ns, "' is not defined\n",sep=""))
    m <- match(ns, openTags$nsprefix, NULL)
    if( any(!is.null(openTags[m,"nsURI"])) )
      return(FALSE)
    stop(paste("Namespace:",ns, "is not defined\n",sep=" "))
  }

 

   # The namespace is the prefix for the tag name. 
   # For example, if the namespace is shelp and the tag is arg
   # the element is  shelp:tag.
   # In this function, we try to infer the ``current'' namespace
   # if the user doesn't specify it. We also have to ensure that 
   # the namespace has a definition before it is used.
   #
   # We also need to allow the user specify an empty namespace
   # so that tags 
  addTag <- function(tag, ..., attrs=NULL, sep="\n", close=TRUE,
                       namespace=NULL, xmlns=NULL) {

    tmp <- ""

      # Flag indicating whether this is the very first, top-level tag.
      # shoulld be shared across these functions and part of the state of 
      # the output buffer instance ?
    startingTag <- is.null(getOpenTag())


      # The user didn't specify a namespace, then
      # we need to check about the xmlns.
      # If the user specified that, then there is an inconsistency.
      # Otherwise, no namespace and no xmlns. So need to get the 
      # current nameSpace.
    if(is.null(namespace)) {
      if( !is.null(xmlns) )
        stop("you must specify the namespace as well as xmlns")
      else {
         # so there is no xmlns. 
         # We need to determine what the currently active
         # namespace is.
       cur <- getOpenTag()
       if(is.null(cur)) {
         namespace <- nameSpace
#         xmlns <- nsURI
       } else {
         startingTag <- FALSE
         namespace <- cur[["nsprefix"]]
       }
      }
    }

      # if you remap prefixes this could be a problem
    if(!is.null(namespace) && namespace == nameSpace && is.null(xmlns) ) {
      tmp1 <- getOpenTag()

      if(is.null(tmp1)) { # || tmp1[["nsURI"]] != nsURI) {
        xmlns <- nsURI
      } # else  namespace <- NULL
    }

   
      #if xmlns is given but not the namespace, then
      # check this.
    if( !is.null(namespace) && is.null(xmlns) )
      checkNamespace(namespace)


    if( !is.null(namespace) && !is.null(xmlns) ) {
      if(!is.null(names(xmlns))) {
         tmpp <- xmlns
         names(tmpp) <- paste("xmlns", names(tmpp), sep=":")
         attrs <- c(attrs, tmpp)
      } else
        attrs[[paste("xmlns", namespace, sep=":")]] <- xmlns
    }

     # if the namespace is non-trivial (null or ""), then concatenate with the
     # tag name. Also handle the case that this is the starting tag
     # and so no namespaces are defined at this point.

    tagName <- ifelse(!startingTag && !is.null(namespace) && namespace != "", paste(namespace,tag,sep=":"), tag)


    if(!is.null(attrs)) {
      tmp <- paste(" ", paste(names(attrs),
                              paste("\"",attrs,"\"",  sep=""),sep="=",
                              collapse=" "),sep="")  
    }
    add(paste("<", tagName, tmp, ">", sep=""))

    if(length(list(...)) > 0) {
      add(..., sep=sep)
    }

    if(close) 
      add(paste(ifelse(sep=="\n","","\n"), "</",tagName, ">", "\n", sep=""), sep="")
    else 
      addOpenTag(tag, namespace, xmlns)

    NULL
  }

   closeTag <- function(name=NULL, namespace=nameSpace) {
    if(is.null(name)) {
      tmp <- getOpenTag() 
      name <- tmp[1] 
      if(length(tmp)>1)
        namespace <- tmp[2] 

      openTags <<- openTags[-lastTag]
      lastTag <<- lastTag-1
    }
    add("</", ifelse(!is.null(namespace) && namespace != "", paste(namespace,name,sep=":"), name),">\n", sep="")
   }



   # This returns the last entry in the matrix openTags
   # which should contain the currently open tag, namespace and 
   # associated URI.
  getOpenTag <-  function() {
    if(lastTag > 0)
      openTags[lastTag, ]
    else 
      NULL
  }


  # 


  paste0 <- function(..., sep="", collapse="") paste(..., sep = sep, collapse=collapse)

  reset <-  function() {
     buf <<- header
     openTags <<- list()
     lastTag <<- 0
  }

  addComment <- function(..., sep="\n") {
    add("<!--", ..., "-->", sep=sep)
  }

  add <- function(..., sep="\n") {
   if(is.character(buf))
     buf <<- paste(buf, paste0(..., collapse=sep), sep=sep) 
   else
     cat(paste0(..., collapse=sep), sep, sep="", file=buf)
  }


  tagString <- function(tag, ..., attrs, close=FALSE) {

    tmp <- ""

    if(!missing(attrs)) {
     tmp <- paste(" ", paste(names(attrs), paste("\"",attrs,"\"", sep=""), sep="=", collapse=" "),sep="")
    }
    return(paste0("<", tag,tmp, ">",...,"</",tag,">"))
  }

  con <- list( value=function() {buf},
               addTag = addTag,
               closeTag = closeTag,
               addEndTag = closeTag,
               reset = reset,
               tagString = tagString,
               add = add,
               addComment = addComment,
               getOpenTag=getOpenTag,
               addOpenTag=addOpenTag
              ) 

  class(con) <- c("XMLOutputBuffer", "XMLOutputStream")

  con 
}



