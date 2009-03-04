\name{getLineNumber}
\alias{getLineNumber}
\title{Determine the line number of an (internal) XML node}
\description{
  This function is used to query the location of an internal/C-level
  XML node within its original "file". This gives us the line number.
  It can be combined with \code{\link{docName}} to determine the
  name of the file and so identify the node location.

  This is useful when we identify a node with a particular charactestic
  and want to view/edit the original document, e.g. when authoring an
  Docbook article.
}
\usage{
getLineNumber(node, ...)
}
\arguments{
  \item{node}{the node whose line number is of interest}
  \item{\dots}{additional parameters for methods should they be defined.}
}

\value{
 An integer. 
}
\references{libxml2 }
\author{Duncan Temple Lang}
\seealso{
  \code{\link{xmlParse}}
  \code{\link{getNodeSet}}
  \code{\link{xpathApply}}    
}
\examples{
f = system.file("exampleData", "xysize.svg", package = "XML")
doc = xmlParse(f)
e = getNodeSet(doc, "//ellipse")
sapply(e, getLineNumber)
}
\keyword{IO}
\concept{XML}
