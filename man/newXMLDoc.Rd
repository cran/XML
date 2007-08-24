\name{newXMLDoc}
\alias{newXMLDoc}
\alias{newXMLNode}
\alias{newXMLPINode}
\alias{newXMLCDataNode}
\alias{newXMLCommentNode}
\alias{newXMLTextNode}
\alias{newXMLDTDNode}
\alias{xmlDoc}
\alias{coerce,vector,XMLInternalNode-method}
\title{Create internal XML node or document object}
\description{
These are used to create internal `libxml' nodes and top-level document objects 
that are used to write XML trees.  While the functions are available,
their direct use is not encouraged. Instead, use \code{\link{xmlTree}}
as the functions need to be used within a strict regime to avoid
corrupting C level structures.

\code{xmlDoc} creates a new XMLInternalDocument
object by copying the given node and all of its
descendants and putting them into a new document.
This is useful when we want to work with sub-trees
with general tools that work on documents, e.g. XPath queries.

}
\usage{
xmlDoc(node)
newXMLDoc(dtd, namespaces=NULL, addFinalizer = TRUE)
newXMLNode(name, ..., attrs = NULL, namespace="", doc=NULL, .children = list(...))
newXMLTextNode(text, doc = NULL)
newXMLCDataNode(text, doc = NULL)
newXMLCommentNode(text, doc = NULL)
newXMLPINode(name, text, doc = NULL)
newXMLDTDNode(nodeName, externalID = character(), systemID = character(), doc = NULL)  
}
\arguments{
  \item{node}{a \code{XMLInternalNode} object that will be copied to
    create a subtree for a new document.}
  \item{dtd}{the name of the DTD to use for the XML document.}
  \item{namespaces}{a named character vector
  with each element specifying a name space identifier and the
  corresponding URI for that namespace
  that are to be declared and used in the XML document, \\
   e.g. \code{c(shelp = "http://www.omegahat.org/XML/SHelp")}}
 \item{addFinalizer}{a logical value indicating whether the
   default finalizer routine should be registered to
   free the internal xmlDoc when R no longer has a reference to this
   external pointer object.
 }
 \item{name}{the tag/element name for the XML node and
   the for a Processing Instruction (PI) node, this is the "target",
   e.g. the identifier for the system for whose attention this PI node is intended.}
  \item{...}{the children of this node. These can be other nodes created
  earlier or R strings that are converted to text nodes and added as children
  to this newly created node.}
  \item{attrs}{a named list of name-value pairs to be used as 
  attributes for the XML node.}
  \item{namespace}{the short or alias for the namespace to use for
      this XML node}
  \item{doc}{the \code{XMLInternalDocument} object created with
    \code{newXMLDoc} that is used to root the node.}
  \item{.children}{a list containing XML node elements or content.
    This is an alternative form of specifying the child nodes than \dots
  which is useful for programmatic interaction when the "sub"-content is
  already in a list rather than a loose collection of values.}
\item{text}{the text content for the new XML node}
\item{nodeName}{the name of the node to put in the DOCTYPE element
  that will appear as the top-most node in the XML document.}
\item{externalID}{the PUBLIC identifier for the document type.
  This is a string of the form \code{A//B//C//D}.
  A is either + or -; B identifies the person or insitution that defined
  the format (i.e. the "creator");
  C is the name of the format; and language is an encoding for the
  language that comes from the ISO 639 document.}
\item{systemID}{the SYSTEM identifier for the DTD for the document.
     This is a URI}
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
when creating content in R that is to be dynamically inserted into an XML
document.}

\seealso{
\code{\link{xmlTree}}
\code{saveXML}
}

\examples{
x = summary(rnorm(1000))
d = xmlTree()
d$addNode("table", close = FALSE)

d$addNode("tr", .children = sapply(names(x), function(x) d$addNode("th", x)))
d$addNode("tr", .children = sapply(x, function(x) d$addNode("td", format(x))))

d$closeNode()


# Just doctype
z = xmlTree("people", dtd = "people")
# no public element
z = xmlTree("people", dtd = c("people", "", "http://www.omegahat.org/XML/types.dtd"))
# public and system
z = xmlTree("people", dtd = c("people", "//a//b//c//d", "http://www.omegahat.org/XML/types.dtd"))

# Using a DTD node directly.
dtd = newXMLDTDNode(c("people", "", "http://www.omegahat.org/XML/types.dtd"))
z = xmlTree("people", dtd = dtd)


x = rnorm(3)
z = xmlTree("r:data", namespaces = c(r = "http://www.r-project.org"))
z$addNode("numeric", attrs = c("r:length" = length(x)), close = FALSE)
lapply(x, function(v) z$addNode("el", x))
z$closeNode()
# should give   <r:data><numeric r:length="3"/></r:data>


# shows namespace prefix on an attribute, and different from the one on the node.
z = xmlTree()
z$addNode("r:data",  namespace = c(r = "http://www.r-project.org", omg = "http://www.omegahat.org"), close = FALSE)
x = rnorm(3)
z$addNode("r:numeric", attrs = c("omg:length" = length(x)))



z = xmlTree("people", namespaces = list(r = "http://www.r-project.org"))
z$setNamespace("r")

z$addNode("person", attrs = c(id = "123"), close = FALSE)
z$addNode("firstname", "Duncan")
z$addNode("surname", "Temple Lang")
z$addNode("title", "Associate Professor")
z$addNode("expertize", close = FALSE)
z$addNode("topic", "Data Technologies")
z$addNode("topic", "Programming Language Design")
z$addNode("topic", "Parallel Computing")
z$addNode("topic", "Data Visualization")
z$closeTag()
z$addNode("address", "4210 Mathematical Sciences Building, UC Davis")
}
\keyword{IO}
