\name{xmlTree}
\alias{xmlTree}
\title{An internal, updatable DOM object for building XML trees}
\description{
This is a mutable object (implemented via a closure)
for representing an XML tree, in the same
spirit as \code{\link{xmlOutputBuffer}}
and \code{\link{xmlOutputDOM}}
but that uses the internal structures of
libxml.
This can be used to create a DOM that can be 
constructed in R and exported to another system
such as XSLT (\url{http://www.omegahat.org/Sxslt})
}
\usage{
xmlTree(tag, dtd=NULL, namespaces=list())
}
\arguments{
  \item{tag}{the tagname of the top-level element. This is basically ignored!}
  \item{dtd}{the name of the external DTD for this document. See \code{\link{newXMLDoc}}}
  \item{namespaces}{a named character vector with each element giving the name space identifier and the
  corresponding URI, e.g c(shelp="http://www.omegahat.org/XML/SHelp")}
}
\details{
 This creates a collection of functions that manipulate a shared
state to build and maintain an XML tree in C-level code.
}
\value{
An object of class
\code{XMLInternalDOM}
that extends \code{XMLOutputStream}
and has the same interface (i.e. ``methods'') as
\code{\link{xmlOutputBuffer}} 
and \code{\link{xmlOutputDOM}}.
Each object has methods for 
adding a new XML tag, 
closing a tag, adding an XML comment,
and retrieving the contents of the tree.
\item{addTag}{create a new tag at the current position,
optionally leaving it as the active open tag to which
new nodes will be added as children}
\item{closeTag}{close the currently active tag
making its parent the active element into
which new nodes will be added.}
\item{addComment}{add an XML comment node
as a child of the active node in the document.}
\item{value}{retrieve an object representing the 
XML tree. See \code{\link{saveXML}} to serialize the
contents of the tree.}
\item{add}{degenerate method in this context.}
}


\references{\url{http://www.w3.org/XML}, \url{http://www.xmlsoft.org},
\url{http://www.omegahat.org}  }
\author{ Duncan Temple Lang }

\note{This is an early version of this function and I need to iron out some
of the minor details.}

\seealso{
\code{\link{saveXML}}
\code{\link{newXMLDoc}}
\code{\link{newXMLNode}}
\code{\link{xmlOutputBuffer}}
\code{\link{xmlOutputDOM}}
}

\examples{
  tr <- xmlTree("Duncan")
  tr$addTag("name", attrs=c(a=1,b="xyz"), close=F)
    tr$addTag("first", "Larry")
    tr$addTag("last", "User")
  tr$closeTag()
  tr$value()
  cat(saveXML(tr$value()))
}
\keyword{XML}
\keyword{IO}
