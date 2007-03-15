# These are classes and facilities for referring to a DTD for the
# DOCTYPE field of an XML document

setClass("Doctype", representation(name = "character",
                                   system = "character",
                                   public = "character"))

Doctype =
function(system = "", public = "", name = "")
{
  new("Doctype", name = name, system = system, public = public)
}

ddQuote =
function(x)
{
  if(length(x) == 0)
    return(character())

  paste('"', x, '"', sep = "")
}  

setAs("Doctype", "character",
       function(from) {

         if(sum(nchar(from@public), nchar(from@system)))
            extra = c("SYSTEM", ifelse(from@public != "", ddQuote(from@public), ""), 
                                ifelse(from@system != "", ddQuote(from@system), ""))
         paste("<!DOCTYPE", from@name, paste(extra, collapse = " "), ">")
       })



