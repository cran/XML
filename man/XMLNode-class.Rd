\name{XMLNode-class}
\docType{class}
\alias{XMLNode-class}
\alias{XMLInternalNode-class}
\alias{XMLInternalTextNode-class}
\alias{XMLInternalElementNode-class}
\alias{XMLInternalCommentNode-class}
\alias{XMLInternalPINode-class}
\alias{XMLInternalCDataNode-class}
\alias{XMLDTDNode-class}

\title{Classes to describe an XML node object.}
\description{These classes are intended to 
represent an XML node, either directly in S or a reference to an internal
 libxml node.  Such nodes respond to queries about 
 their name, attributes, namespaces and children.  
 These are old-style, S3 class definitions at present.
}
\section{Slots}{
  \describe{
  }
}
\section{Methods}{
No methods defined with class "XMLNode" in the signature.
}

\references{\url{http://www.w3.org/XML}, \url{http://www.xmlsoft.org}}
\author{Duncan Temple Lang}

\seealso{
 \code{\link{xmlTreeParse}}
 \code{\link{xmlTree}}
 \code{\link{newXMLNode}}
 \code{\link{xmlNode}}
}
\examples{

 # An R-level XMLNode object
a <- xmlNode("arg", attrs = c(default="T"),
               xmlNode("name", "foo"), xmlNode("defaultValue","1:10"))
  
}
\keyword{classes}
