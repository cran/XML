\name{xmlRoot}
\alias{xmlRoot}
\alias{xmlRoot.XMLDocument}
\alias{xmlRoot.XMLDocumentRoot}
\alias{xmlRoot.XMLDocumentContent}
\title{Get the top-level XML node.}
\description{
 These are a collection of methods for providing easy access to the
top-level \code{XMLNode} object resulting from parsing an XML
document.  They simplify accessing this node in the presence of
auxillary information such as DTDs, file name and version information
that is returned as part of the parsing.
}
\usage{
xmlRoot(x, ...)
xmlRoot.XMLDocumentContent(x, skip = T)
}
\arguments{
  \item{x}{the object whose root/top-level XML node is to be returned.}
  \item{skip}{a logical value that controls whether XMLComment objects that appear
    before the real top-leve node of the document should be ignored
   or not.}
  \item{...}{arguments that are passed by the generic to the different specialized 
       methods of this generic.}
}
\value{
 An object of class \code{XMLNode}.
}
\references{\url{http://www.w3.org/XML}, \url{http://www.jclark.com/xml},
\url{http://www.omegahat.org}  }
\author{ Duncan Temple Lang }
\note{One cannot obtain the parent or top-level node
  of an XMLNode object in S. This is different from 
  languages like C, Java, Perl, etc. and is primarily 
  because S does not provide support for references.}
\seealso{
\code{\link{xmlTreeParse}}
\code{\link{[[.XMLNode}}
}

\examples{
  doc <- xmlTreeParse(system.file("exampleData", "mtcars.xml", package="XML"))
  xmlRoot(doc)
}
\keyword{file}
