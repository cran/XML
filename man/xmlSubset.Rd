\name{[.XMLNode}
\alias{[.XMLNode}
\alias{[[.XMLNode}
\title{Convenience accessors for the children of XMLNode objects.}
\description{
 These provide a simplified syntax for extracting the children
of an XML node.
}
\usage{
[.XMLNode(obj, ...)
[[.XMLNode(obj, ...)
}
\arguments{
  \item{obj}{the XML node whose children are to be accessed}
  \item{\dots}{the identifiers for the children to be retrieved,
given as integer indices, names, etc. in the usual format for the
generic \code{link{[}} and \code{link{[[}} operators}
}
\details{

}
\value{
 A list or single element containing the
children of the XML node given by \code{obj}
and identified by \dots.
}
\references{\url{http://www.w3.org/XML}, \url{http://www.omegahat.org/RSXML}}
\author{Duncan Temple Lang}


\seealso{
 \code{\link{xmlAttrs}}
}

\examples{

}
\keyword{XML}
\keyword{IO}

