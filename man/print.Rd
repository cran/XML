\name{print.XMLAttributeDef}
\alias{print.XMLAttributeDef}
\alias{print.XMLCDataNode}
\alias{print.XMLElementContent}
\alias{print.XMLElementDef}
\alias{print.XMLEntity}
\alias{print.XMLEntityRef}
\alias{print.XMLNode}
\alias{print.XMLOrContent}
\alias{print.XMLSequenceContent}
\alias{print.XMLProcessingInstruction}
\title{Methods for displaying XML objects}
\description{
 These different methods attempt to provide a convenient
way to display R objects representing XML elements
when they are printed in the usual manner on
the console, files, etc. via the \code{\link{print}}
function.
Each typically outputs its contents in the way
that they would appear in an XML document.
}
\usage{
print.XMLAttributeDef(x)
print.XMLElementContent(x)
print.XMLElementDef(x)
print.XMLEntity(x)
print.XMLEntityRef(x)
print.XMLNode(x, ...)
print.XMLOrContent(x)
print.XMLSequenceContent(x)
}
\arguments{
  \item{x}{the XML object to be displayed}
  \item{...}{additional arguments to for controlling the output from
  print. Currently unused.}
}
\value{
 Currently, \code{NULL}.
}
\references{\url{http://www.w3.org}, \url{http://www.omegahat.org/RSXML}}
\author{Duncan Templle Lang}

\seealso{
\code{\link{xmlTreeParse}}
}

\examples{

}
\keyword{XML}
\keyword{file}

