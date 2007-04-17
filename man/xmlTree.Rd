\name{xmlTree}
\alias{xmlTree}
\title{An internal, updatable DOM object for building XML trees}
\description{
This is a mutable object (implemented via a closure)
for representing an XML tree, in the same
spirit as \code{\link{xmlOutputBuffer}}
and \code{\link{xmlOutputDOM}}
but that uses the internal structures of
libxml.
This can be used to create a DOM that can be 
constructed in R and exported to another system
such as XSLT (\url{http://www.omegahat.org/Sxslt})
}
\usage{
xmlTree(tag, attrs = NULL, dtd=NULL, namespaces=list(),
          doc = newXMLDoc(dtd, namespaces))
}
\arguments{
  \item{tag}{the node or element name to use to create the new top-level node in the tree
    or alternatively, an \code{XMLInternalNode} that was already
    created.
    This is optional. If it is not specified, no top-most node is
    created but can be added using \code{addNode}.
    If a top-level tag is added in the call to
    \code{xmlTree},  that becomes the currently active or open
    node (e.g. same as \code{addNode( ..., close = FALSE)})
    and nodes subsequently added to this
    }
  \item{attrs}{attributes for the top-level node, in the form of a named
    character vector.}
  \item{dtd}{the name of the external DTD for this document.
    If specified, this adds the DOCTYPE node to the resulting document.
    This can be a node created earlier with a call to 
    \code{\link{newXMLDTDNode}}, or alternatively it can be a
    character vector with 1, 2 or 3 elements
    giving the name of the top-level node, and the public identifier
    and the system identifier for the DTD in that order.
  }
  \item{namespaces}{a named character vector with each element giving the name space identifier and the
    corresponding URI, \\
    e.g \code{c(shelp = "http://www.omegahat.org/XML/SHelp")}
    If \code{tag} is specified as a character vector, these name spaces
    are defined within that new node.
  }
  \item{doc}{an internal XML document object, typically created with
    \code{\link{newXMLDoc}}. This is used as the host document for all
    the new nodes that will be created  as part of this document.
    If one wants to create nodes without an internal document ancestor,
    one can alternatively specify this is as  \code{NULL}.}
}
\details{
 This creates a collection of functions that manipulate a shared
state to build and maintain an XML tree in C-level code.
}
\value{
An object of class
\code{XMLInternalDOM}
that extends \code{XMLOutputStream}
and has the same interface (i.e. ``methods'') as
\code{\link{xmlOutputBuffer}} 
and \code{\link{xmlOutputDOM}}.
Each object has methods for 
adding a new XML tag, 
closing a tag, adding an XML comment,
and retrieving the contents of the tree.
\item{addTag}{create a new tag at the current position,
optionally leaving it as the active open tag to which
new nodes will be added as children}
\item{closeTag}{close the currently active tag
making its parent the active element into
which new nodes will be added.}
\item{addComment}{add an XML comment node
as a child of the active node in the document.}
\item{value}{retrieve an object representing the 
XML tree. See \code{\link{saveXML}} to serialize the
contents of the tree.}
\item{add}{degenerate method in this context.}
}


\references{\url{http://www.w3.org/XML}, \url{http://www.xmlsoft.org},
\url{http://www.omegahat.org}  }
\author{ Duncan Temple Lang }

\note{This is an early version of this function and I need to iron out some
of the minor details.}

\seealso{
\code{\link{saveXML}}
\code{\link{newXMLDoc}}
\code{\link{newXMLNode}}
\code{\link{xmlOutputBuffer}}
\code{\link{xmlOutputDOM}}
}

\examples{
  tr <- xmlTree("Duncan")
  tr$addTag("name", attrs=c(a=1,b="xyz"), close=FALSE)
    tr$addTag("first", "Larry")
    tr$addTag("last", "User")
  tr$closeTag()
  tr$value()
  cat(saveXML(tr$value()))

  tr <- xmlTree("CDataTest")
  tr$addTag("top", close=FALSE)
  tr$addCData("x <- list(1, a='&');\nx[[2]]")
  tr$addPI("S", "plot(1:10)")
  tr$closeTag()
  cat(saveXML(tr$value()))


  # Creating a node
x = rnorm(3)
z = xmlTree("r:data", namespaces = c(r = "http://www.r-project.org"))
z$addNode("numeric", attrs = c("r:length" = length(x)))


  # shows namespace prefix on an attribute, and different from the one on the node.
  z = xmlTree()
z$addNode("r:data",  namespace = c(r = "http://www.r-project.org", omg = "http://www.omegahat.org"), close = FALSE)
x = rnorm(3)
z$addNode("r:numeric", attrs = c("omg:length" = length(x)))

}
\keyword{IO}
