\name{xmlChildren}
\alias{xmlChildren}
\alias{xmlChildren.XMLNode}
\title{ Gets the sub-nodes within an XMLNode object. }
\description{
 This returns a list of XMLNode objects within an
 XMLNode object.
}
\usage{
xmlChildren(node)
}
\arguments{
  \item{node}{an object of class XMLNode.}
}
\value{
 A list whose elements are sub-nodes of the user-specified
XMLNode. These are also of class XMLNode.
}
\references{\url{http://www.w3.org/XML}}
\author{ Duncan Temple Lang }

\seealso{
\code{\link{xmlChildren}},\code{\link{xmlSize}},
\code{\link{xmlTreeParse}}
}


\examples{
  fileName <- system.file("data", "mtcars.xml", pkg="XML")
  doc <- xmlTreeParse(fileName)
  names(xmlChildren(doc$doc$children[["dataset"]]))
}
\keyword{IO}
