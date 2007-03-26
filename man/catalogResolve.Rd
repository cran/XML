\name{catalogResolve}
\alias{catalogResolve}
\title{Look up an element via the XML catalog mechanism}
\description{
  XML parsers use a catalog to map generic system and public addresses
  to actual local files or potentially different remote files.
  We can use a catalog to map a reference such as
  \code{http://www.omegahat.org/XSL/} to a particular
  directory on our local machine and then not have to
  modify any of the documents if we move the local files to another
  directory, e.g. install a new version in an alternate directory.
   
  This function provides a mechanism to query the catalog to
  resolve a URI, PUBLIC or SYSTEM identifier.
  
}
\usage{
catalogResolve(id, type = "uri", debug = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{the name of the (generic) element to be resolved}
  \item{type}{a string, specifying whether the lookup is for a uri,
    system or public element}
  \item{debug}{logical value indicating whether to turn on debugging
    output written to the console (\code{TRUE}) or not (\code{FALSE}).}
}
\details{
}
\value{
  A character vector. If the element was resolved,
  the single element is the resolved value.
  Otherwise, the character vector will contain no elements.
}
\references{
  \url{http://www.xmlsoft.org}
  \url{http://www.sagehill.net/docbookxsl/Catalogs.html} provides a short, succinct tutorial on catalogs. 
}
\author{Duncan Temple Lang}

\seealso{
 \code{\link{xmlTreeParse}}
}
\examples{

Sys.putenv("XML_CATALOG_FILES" = system.file("exampleData", "catalog.xml", package = "XML"))

catalogResolve("-//OASIS//DTD DocBook XML V4.4//EN", "public")

catalogResolve("http://www.omegahat.org/XSL/foo.xsl")

catalogResolve("http://www.omegahat.org/XSL/article.xsl", "uri")
catalogResolve("http://www.omegahat.org/XSL/math.xsl", "uri")


  # This one does not resolve anything, returning an empty value.
catalogResolve("http://www.oasis-open.org/docbook/xml/4.1.2/foo.xsl", "uri")

}
\keyword{IO}
\concept{XML}

