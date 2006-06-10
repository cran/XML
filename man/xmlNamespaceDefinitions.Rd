\name{xmlNamespaceDefinitions}
\alias{xmlNamespaceDefinitions}
\title{Get definitions of any namespaces defined in this XML node}
\description{
  If the given node has any namespace definitions declared within it,
  i.e. of the form \code{xmlns:myNamespace="http://www.myNS.org"},
  this function provides access to these definitions.
  While they appear in the XML node in the document as attributes,
  they are treated differently by the parser and so do not show up
  in the nodes attributes via \code{\link{xmlAttrs}}.
}
\usage{
xmlNamespaceDefinitions(x)
}
\arguments{
  \item{x}{the \code{XMLNode} object in which to find any namespace definitions}
}
\details{
}
\value{
  A list with as many elements as there are namespace definitions.
  Each element is an object of class XMLNameSpace,
  containing fields giving the local identifier, the associated defining
  URI and a logical value indicating whether the definition is local to
  this node.
}
\references{\url{http://www.w3.org/XML}}
\author{ Duncan Temple Lang }

\seealso{
  \code{\link{xmlTreeParse}}
  \code{\link{xmlAttrs}}
  \code{\link{xmlGetAttr}}  
}
\examples{
  f = system.file("exampleData", "longitudinalData.xml", package = "XML")
  n = xmlRoot(xmlTreeParse(f))
  xmlNamespaceDefinitions(n)
}
\keyword{IO}
\concept{XML}
