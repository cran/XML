\name{xmlValue}
\alias{xmlValue}
\alias{xmlValue.XMLCDataNode}
\alias{xmlValue.XMLNode}
\alias{xmlValue.XMLProcessingInstruction}
\alias{xmlValue.XMLTextNode}
\title{Extract the contents of a leaf XML node}
\description{
 Some types of XML nodes have no children, but simply
text. Examples are \code{XMLTextMode},
\code{XMLProcessingInstruction}.
This function provides access to their contents. 
}
\usage{
xmlValue(x)
}
\arguments{
  \item{x}{the \code{XMLNode} object whose
    contents are to be  returned.}
}
\value{
 The object stored in the 
 \code{value} slot of the \code{XMLNode} object.
 This is typically a string.
}
\references{\url{http://www.w3.org/XML}, \url{http://www.jclark.com/xml},
\url{http://www.omegahat.org}  }
\author{ Duncan Temple Lang }

\seealso{
 \code{\link{xmlChildren}}
 \code{\link{xmlName}}
 \code{\link{xmlAttrs}}
 \code{\link{xmlNamespace}}
}

\examples{
  
}
\keyword{IO}
