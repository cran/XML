libxmlVersion <-
function()
{
 v <- .Call("RS_XML_libxmlVersion")
 v <- as.character(v)
 els <- substring(v, 1:nchar(v), 1:nchar(v))
 list(major=els[1], minor=paste(els[2:3],sep="", collapse=""), patch=paste(els[4:5], sep="", collapse=""))
}