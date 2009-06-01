\name{isXMLString}
\Rdversion{1.1}
\alias{isXMLString}
\alias{xml}
\alias{xmlParseString}
\alias{XMLString-class}
\title{Facilities for working with XML strings}
\description{
 These functions and classes are used to represent and parse a
 string whose content is known to be XML.
 \code{xml} allows us to mark a character vector as containing XML,
 i.e. of class \code{XMLString}.

 \code{xmlParseString} is a convenience routine for converting an
 XML string into an XML node/tree.

 \code{isXMLString} is examines a strings content and heuristically 
 determines whether it is XML.
}
\usage{
isXMLString(str)
xmlParseString(content, doc = NULL, namespaces = RXMLNamespaces, clean = TRUE) 
xml(x) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{str,x,content}{the string containing the XML material.}
  \item{doc}{}
  \item{namespaces}{}
  \item{clean}{}
}
\details{
}
\value{

}
\references{
}
\author{Dncan Temple Lang}

\seealso{
\code{\link{xmlParse}}
\code{\link{xmlTreeParse}}
}
\examples{
 isXMLString("a regular string < 20 characters long")
 isXMLString("<a><b>c</b></a>")

 xmlParseString("<a><b>c</b></a>")

  # We can lie!
 isXMLString(xml("foo"))
}
\keyword{IO}
\concept{XML}
