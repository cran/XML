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
function(dtd = NULL)
{
  paste0 <- function(...) paste(..., sep = "")
  buf <- "<?xml version=\"1.0\"?>"

  reset <-  function() {
     buf <<- "<?xml version=\"1.0\"?>"
  }

  add <- function(..., sep="\n") {
    buf <<- paste(buf, paste0(...), sep=sep) 
  }

  tagString <- function(tag, ..., attrs, close=F) {
   tmp <- ""
    if(!missing(attrs)) {
     tmp <- paste(" ", paste(names(attrs), paste("\"",attrs,"\"", sep=""), sep="=", collapse=" "),sep="")
    }
   return(paste0("<", tag,tmp, ">",...,"</",tag,">"))
  }

  addTag <- function(tag, ..., attrs=NULL, sep="\n", close=T) {
    tmp <- ""
    if(!missing(attrs)) {
      tmp <- paste(" ", paste(names(attrs),  paste("\"",attrs,"\"", sep=""),sep="=", collapse=" "),sep="")
    }

    add(paste("<",tag, tmp, ">", sep=""))

    if(length(list(...)) > 0) {
      add(..., sep=sep)
    }
    if(close) {
      add(paste("</",tag, ">", sep=""), sep="")
    } 

    NULL
  }

   addEndTag <- function(name) {
     add("\n", "</",name,">", sep="")
   }


  con <- list( value=function() {buf},
               add = add,
               addTag = addTag,
               addEndTag = addEndTag,
               tagString = tagString,
               reset = reset
             ) 

  class(con) <- c("XMLOutputBuffer", "XMLOutputStream")

  con 

}
