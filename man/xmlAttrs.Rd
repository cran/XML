\name{xmlAttrs}
\alias{xmlAttrs}
\alias{xmlAttrs.XMLElementDef}
\alias{xmlAttrs.XMLNode}
\title{ Get the list of attributes of an XML node. }
\description{
 This returns a named character vector giving the 
 name-value pairs of attributes of an XMLNode object which is part of
 an XML document.
}
\usage{
 xmlAttrs(node)
}
\arguments{
  \item{node}{The \code{XMLNode} object
 whose attributes are to be extracted.
 }
}
\value{
 A named character vector, where the names
are the attribute names and the 
elements are the corresponding values.
This corresponds to the (attr<i>, "value<i>")
pairs in the XML tag
\code{<tag attr1="value1"  attr2="value2"}
}
\references{\url{http://www.w3.org} }
\author{Duncan Temple Lang}
\seealso{ \code{\link{xmlChildren}}, \code{\link{xmlSize}},
\code{\link{xmlName}}}

\examples{
 fileName <- system.file("exampleData", "mtcars.xml", package="XML") 
 doc <- xmlTreeParse(fileName)

 xmlAttrs(doc$doc$children[["dataset"]])

 xmlAttrs(doc$doc$children[["dataset"]][["variables"]])
}
\keyword{XML}
\keyword{file}
