\name{getNodeSet}
\alias{getNodeSet}
\alias{xpathApply}
\title{Find matching nodes in an internal XML tree/DOM}
\description{
  These functions provide a way to find XML nodes that match a particular
  criterion. It uses the XPath syntax and allows quite powerful
  expressions for identifying nodes.  The XPath language requires some
  knowledge, but tutorials are available on the Web and in books.
  XPath queries can result in different types of values such as numbers,
  strings, and node sets. 

  These sets of matching nodes are returned in R
  as a list.  And then one can iterate over these elements to process the 
  nodes in whatever way one wants. Unfortunately, this involves two loops -
  one in the XPath query over the entire tree, and another in R.
  Typically, this is fine as the number of matching nodes is reasonably small.
  However, if repeating this on numerous files, speed may become an issue.
  We can avoid the second loop (i.e. the one in R) by applying a function to each node
  before it is returned to R as part of the node set.  The result of the function
  call is then returned, rather than the node itself.

  One can provide an expression rather than a function. This is expected to be a call
  and the first argument of the call will be replaced with the node.
  
}
\usage{
getNodeSet(doc, path, namespaces = character(), fun = NULL, ...)
xpathApply(doc, path, fun, ... , namespaces = character())
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
 \item{fun}{a function object, or an expression or call, which is used when the result is a node set
     and evaluated for each node element in the node set.  If this is a call, the first argument is replaced 
     with the current node.
 }
 \item{...}{any additional arguments to be passed to \code{fun} for each node in the node set.}
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

  If \code{fun} is supplied and the result of the XPath query is a node set, 
  the result in R is a list.
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


 #####
 # European Central Bank (ECB) exchange rate data

  # Data is available from "http://www.ecb.int/stats/eurofxref/eurofxref-hist.xml"
  # or locally.

 uri = system.file("exampleData", "eurofxref-hist.xml.gz", package = "XML")
 doc = xmlTreeParse(uri, useInternalNodes = TRUE)

   # The default namespace for all elements is given by
 namespaces <- c(ns="http://www.ecb.int/vocabulary/2002-08-01/eurofxref")


     # Get the data for Slovenian currency for all time periods.
     # Find all the nodes of the form <Cube currency="SIT"...>

 slovenia = getNodeSet(doc, "//ns:Cube[@currency='SIT']", namespaces )

    # Now we have a list of such nodes, loop over them 
    # and get the rate attribute
 rates = as.numeric( sapply(slovenia, xmlGetAttr, "rate") )
    # Now put the date on each element
    # find nodes of the form <Cube time=".." ... >
    # and extract the time attribute
 names(rates) = sapply(getNodeSet(doc, "//ns:Cube[@time]", namespaces ), 
                      xmlGetAttr, "time")

    #  Or we could turn these into dates with strptime()
 strptime(names(rates), "\%Y-\%m-\%d")


   #  Using xpathApply, we can do
 rates = xpathApply(doc, "//ns:Cube[@currency='SIT']", xmlGetAttr, "rate", namespaces = namespaces )
 rates = as.numeric(unlist(rates))

   # Using an expression rather than  a function and ...
 rates = xpathApply(doc, "//ns:Cube[@currency='SIT']", quote(xmlGetAttr(x, "rate")), namespaces = namespaces )
}
\keyword{file}
\keyword{IO}


