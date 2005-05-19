\name{xmlParent}
\alias{xmlParent}
\alias{xmlParent.XMLInternalNode}
\title{Get parent node of XMLInternalNode}
\description{
 This operates on an internal C-level XML node
 and returns a reference to its parent node 
 within the document tree.
}
\usage{
xmlParent(x)
}
\arguments{
  \item{x}{an object of class \code{XMLInternalNode} whose parent is being requested. }
}
\details{
 This uses the internal libxml structures to access the parent in the DOM tree. 
 This function is generic so that we can add methods for other types of nodes
 if we so want in the future.
}
\value{
 An object of class \code{XMLInternalNode}.
}

\references{\url{http://www.w3.org/XML}}
\author{ Duncan Temple Lang }

\seealso{
 \code{\link{xmlChildren}}
 \code{\link{xmlTreeParse}}
 \code{\link{xmlNode}}
}
\examples{

}
\keyword{file}
\keyword{IO}


