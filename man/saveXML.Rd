\name{saveXML}
\alias{saveXML}
\alias{saveXML.XMLInternalDocument}
\alias{saveXML.XMLInternalDOM}
\title{Output internal XML Tree}
\description{
Methods for writing the representation of an XML tree stored in 
internal memory to a string or file.
}
\usage{
saveXML(doc, file=NULL, compression=0, indent=T)
saveXML.XMLInternalDocument(doc, file=NULL, compression=0, indent=T)
saveXML.XMLInternalDOM(doc, file=NULL, compression=0, indent=T)
}
\arguments{
  \item{doc}{}
  \item{file}{}
  \item{compression}{ ~~Describe \code{compression} here~~ }
  \item{indent}{ ~~Describe \code{indent} here~~ }
}
\details{
 One can create an internal XML tree (or DOM)
 using \code{\link{newXMLDoc}} and \code{\link{newXMLNode}}.
 \code{saveXML} allows one to generate a textual representation of
 that DOM in human-readable and reusable XML format.
 \code{saveXML} is a generic function that allows one to call
 the rendering operation with either the top-level node
 of the DOM or of the document object (of class \code{XMLInternalDocument}
 that is used to 
 accumulate the nodes and with which the developer 
 adds nodes.
}
\value{
 
}
\references{\url{http://www.w3.org/XML}, \url{http://www.omegahat.org/RSXML}}
\author{Duncan Temple Lang}

\seealso{
\code{\link{newXMLDoc}}
\code{\link{newXMLNode}}
\code{\link{xmlOutputBuffer}}
\code{\link{xmlOutputDOM}}
}

\examples{

}
\keyword{XML}
\keyword{file}

