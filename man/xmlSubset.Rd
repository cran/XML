\name{[.XMLNode}
\alias{[.XMLNode}
\alias{[[.XMLNode}
\alias{[[.XMLDocumentContent}
\title{Convenience accessors for the children of XMLNode objects.}
\description{
 These provide a simplified syntax for extracting the children
of an XML node.
}
\usage{
obj[[i]]
"[.XMLNode"(obj, ...)
"[[.XMLNode"(obj, ...)
"[[.XMLDocumentContent"(obj, ...)
}
\arguments{
  \item{obj}{the XML node or the top-level document content in which the children are to be accessed.
  The \code{XMLDocumentContent} is the container for the top-level node that also contains information
  such as the URI/filename and XML version. This accessor method is merely a convenience to get 
  access to children of the top-level node.}
 \item{i}{index of the child of interest or the name of an XML element
   of interest. In this latter case, only the first matching element is
   returned, if any.}
  \item{\dots}{the identifiers for the children to be retrieved,
given as integer indices, names, etc. in the usual format for the
generic \code{link{[}} and \code{link{[[}} operators}
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
 \code{\link{[<-.XMLNode}}
 \code{\link{[[<-.XMLNode}}
}

\examples{

}
\keyword{IO}
\keyword{file}

