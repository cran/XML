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
    One only needs to specify the namespaces in the XPath expression and
    for the nodes of interest rather than requiring all the
    namespaces for the entire document.
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
}
\keyword{file}
\keyword{IO}

