\name{xmlNode}
\alias{xmlNode}
\alias{xmlTextNode}
\alias{xmlPINode}
\alias{xmlCDataNode}
\alias{xmlCommentNode}
\title{Create an XML node}
\description{
 These functions allow one to create XML nodes
as are created in C code when reading XML documents.
Trees of XML nodes can be constructed and 
integrated with other trees generated manually
or with via the parser.
}
\usage{
xmlNode(name, ..., attrs=NULL, namespace="")
xmlTextNode(value, namespace="")
xmlPINode(sys, value, namespace="")
xmlCDataNode(...)
xmlCommentNode(text)
}
\arguments{
  \item{name}{The tag or element name of the XML node. This is what appears
   in the elements as \code{<name> .. </name>}}
  \item{\dots}{The children nodes of this XML node.
    These can be objects of class \code{XMLNode}
   or arbitrary values that will be converted to a string
   to form an \code{XMLTextNode} object.}
  \item{attrs}{A named character vector giving the 
     name, value pairs of attributes for this XML node.}
  \item{value}{This is the text that is to be used when forming
     an \code{XMLTextNode}.}
  \item{namespace}{The XML namespace identifier for this node.}
  \item{sys}{the name of the system for which the processing instruction
    is targeted. This is the value that appears in the
     \code{<?sys value?>}}
  \item{text}{character string giving the contents of the comment.}
}
\value{
 An object of class \code{XMLNode}.
In the case of \code{xmlTextNode},
this also inherits from \code{XMLTextNode}.
The fields or slots that objects
of these classes have
include
\code{name}, \code{attributes}, \code{children} and \code{namespace}.
However, one should 
the accessor functions
\code{\link{xmlName}},
\code{\link{xmlAttrs}},
\code{\link{xmlChildren}}
and
\code{\link{xmlNamespace}}
}
\references{\url{http://www.w3.org/XML}, \url{http://www.jclark.com/xml},
\url{http://www.omegahat.org}  }
\author{ Duncan Temple Lang }

\seealso{
\code{\link{xmlTreeParse}}
\code{\link{asXMLNode}}
}

\examples{
a <- xmlNode("arg", attrs = c(default="T"), xmlNode("name", "foo"), xmlNode("defaultValue","1:10"))
}
\keyword{file}