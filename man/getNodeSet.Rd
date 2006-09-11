\name{getNodeSet}
\alias{getNodeSet}
\title{Find matching nodes in an internal XML tree/DOM}
\description{
  This function provides a way to find XML nodes that match a particular
  criterion. It uses the XPath syntax and allows quite powerful
  expressions for identifying nodes.  The XPath language requires some
  knowledge, but tutorials are available.
}
\usage{
getNodeSet(doc, path, namespaces = character())
}
\arguments{
  \item{doc}{an object of class \code{XMLInternalDocument}}
  \item{path}{a string (character vector of length 1) giving the
    XPath expression to evaluate.}
  \item{namespaces}{ a named character vector giving the
    namespace prefix and URI pairs that are to be used
    in the XPath expression and matching of nodes.
   The prefix is just a simple string that acts as a short-hand 
   or alias for the URI that is the unique identifier for the
   namespace.
   The URI is the element in this vector and the prefix is the
   corresponding element name.
    One only needs to specify the namespaces in the XPath expression and
    for the nodes of interest rather than requiring all the
    namespaces for the entire document.
    Also note that the prefix used in this vector is local only to the
    path. It does not have to be the same as the prefix used in the
    document to identify the namespace. However, the URI in this
    argument must be identical to the target namespace URI in the
    document.  It is the namespace URIs that are matched (exactly)
    to find correspondence. The prefixes are used only to refer to
    that URI.
  }
}
\details{
  This calls the libxml routine \code{xmlXPathEval}.
}
\value{
  The results can currently be different
  based on the returned value from the XPath expression evaluation:
  \item{list}{a node set}
  \item{numeric}{a number}
  \item{logical}{a boolean}
  \item{character}{a string, i.e. a single character element.}
}

\references{\url{http://xmlsoft.org}, 
  \url{http://www.w3.org/xml}
  \url{http://www.w3.org/TR/xpath}
  \url{http://www.omegahat.org/RSXML}
}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}

\note{
  More of the XPath functionality provided by libxml can and may be
  made available to the R package.
  Facilities such as compiled XPath expressions, functions, ordered node
  information,
   
  Please send requests to the maintainer.
}

\seealso{
 \code{\link{xmlTreeParse}} with \code{useInternalNodes} as \code{TRUE}.
}
\examples{
 doc = xmlTreeParse(system.file("exampleData", "tagnames.xml", package = "XML"), useInternalNodes = TRUE)
 getNodeSet(doc, "/doc//b[@status]")
 getNodeSet(doc, "/doc//b[@status='foo']")

 
 els = getNodeSet(doc, "/doc//a[@status]")
 sapply(els, function(el) xmlGetAttr(el, "status"))

  # Using a namespace
 f = system.file("exampleData", "SOAPNamespaces.xml", package = "XML") 
 z = xmlTreeParse(f, useInternal = TRUE)
 getNodeSet(z, "/a:Envelope/a:Body", c("a" = "http://schemas.xmlsoap.org/soap/envelope/"))
 getNodeSet(z, "//a:Body", c("a" = "http://schemas.xmlsoap.org/soap/envelope/"))

  # Get two items back with namespaces
 f = system.file("exampleData", "gnumeric.xml", package = "XML") 
 z = xmlTreeParse(f, useInternal = TRUE)
 getNodeSet(z, "//gmr:Item/gmr:name", c(gmr="http://www.gnome.org/gnumeric/v2"))

}
\keyword{file}
\keyword{IO}

