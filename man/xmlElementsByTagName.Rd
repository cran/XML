\name{xmlElementsByTagName}
\alias{xmlElementsByTagName}
\title{Retrieve the children of an XML node with a specific tag name}
\description{
 This returns a list of the children or sub-elements of
an XML node whose tag name matches the one specified by
the user.
}
\usage{
xmlElementsByTagName(el, name)
}
\arguments{
  \item{el}{the node whose matching children are to be retrieved.}
  \item{name}{a string giving the name of the tag to match in each of 
\code{el}'s children.}
}
\details{
 This does a simple matching of names and subsets the XML node's
children list.
}
\value{
 A list containing those child nodes of \code{el} whose
tag name matches that specified by the user.
}
\references{\url{http://www.w3.org/XML}, \url{http://www.omegahat.org/RSXML},
}
\author{Duncan Temple Lang}
\seealso{
\code{\link{xmlChildren}}
\code{\link{xmlTreeParse}}
}

\examples{
\dontrun{
 doc <- xmlTreeParse("http://www.omegahat.org/Scripts/Data/mtcars.xml")
 xmlElementsByTagName(doc$children[[1]], "variable")
}

 doc <- xmlTreeParse(system.file("data", "mtcars.xml", pkg="XML"))
 xmlElementsByTagName(xmlRoot(doc)[[1]], "variable")
}
\keyword{XML}
\keyword{I/O}
