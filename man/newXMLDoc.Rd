\name{newXMLDoc}
\alias{newXMLDoc}
\alias{newXMLNode}
\title{Create internal XML node or document object}
\description{
These are used to create internal `libxml' nodes
and top-level document objects 
that are used to write XML trees.
While the functions are available,
their direct use is not encouraged.
Instead, use \code{\link{xmlTree}}
as the functions need to be 
used within a strict regime
to avoid corrupting C level structures.
}
\usage{
newXMLDoc(dtd, namespaces=NULL)
newXMLNode(name, ..., attrs = NULL, namespace="", doc=NULL)
}
\arguments{
  \item{dtd}{the name of the DTD to use for the XML document.}
  \item{namespaces}{a named character vector
  with each element specifying a name space identifier and the
  corresponding URI for that namespace
  that are to be declared and used in the XML document,
   e.g c(shelp="http://www.omegahat.org/XML/SHelp")}
  \item{name}{the tag/element name for the XML node}
  \item{...}{the children of this node. These can be other nodes created
  earlier or R strings that are converted to text nodes and added as children
  to this newly created node.}
  \item{attrs}{a named list of name-value pairs to be used as 
  attributes for the XML node.}
  \item{namespace}{the short or alias for the namespace to use for
      this XML node}
  \item{doc}{the \code{XMLInternalDocument} object created with
  \code{newXMLDoc} that is used to root the node.}
}
\details{
These create internal C level objects/structure instances
that can be added to a libxml DOM and subsequently
inserted into other document objects or ``serialized''
to textual form.
}
\value{
 Each function returns an R object that points to the
C-level structure instance.
These are of class \code{XMLInternalDocument}
and \code{XMLInternalNode}, respectively
}

\references{\url{http://www.w3.org/XML}, \url{http://www.xmlsoft.org},
\url{http://www.omegahat.org}}
\author{ Duncan Temple Lang }

\note{These functions are used to build up an internal XML tree.
This can be used in the Sxslt package (\url{http://www.omegahat.org/Sxslt})
when creating content in R that is to be dynamicall inserted into an XML
document.}

\seealso{
\code{\link{xmlTree}}
\code{saveXML}
}

\examples{

}
\keyword{XML}
