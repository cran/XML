\name{xmlName}
\alias{xmlName}
\alias{xmlName.XMLComment}
\alias{xmlName.XMLNode}
\title{ Extraces the tag name of an XMLNode object.}
\description{
  Each XMLNode object has a tag name introduced
in the \code{<name ...>} entry in an XML document.
This function returns that name.
}
\usage{
xmlName(node, full = FALSE)
}
\arguments{
  \item{node}{The XMLNode object whose tag name is being requested.}
  \item{full}{a logical value indicating whether to prepend the
    namespace prefix, if there is one, or return just the
    name of the XML element/node. \code{TRUE} means prepend the prefix.}
}

\value{
 A character vector of length \textit{1}
 which is the \code{node$name} entry.
}
\references{\url{http://www.w3.org/XML}, \url{http://www.jclark.com/xml},
\url{http://www.omegahat.org}  }
\author{ Duncan Temple Lang }


\seealso{
\code{\link{xmlChildren}},
\code{\link{xmlAttrs}},
\code{\link{xmlTreeParse}}
}

\examples{
 fileName <- system.file("exampleData", "test.xml", package="XML") 
 doc <- xmlTreeParse(fileName)
 xmlName(xmlRoot(doc)[[1]])
}
\keyword{file}
