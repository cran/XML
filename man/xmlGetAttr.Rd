\name{xmlGetAttr}
\alias{xmlGetAttr}
\title{Get the value of an attribute in an XML node}
\description{
  This is a convenience function that retrieves
  the value of a named attribute in an XML node,
  taking care of checking for its existence.
  It also allows the caller to provide a default
  value to use as the return value if the
  attribute is not present.
}
\usage{
xmlGetAttr(node, name, default = NULL, converter = NULL)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{node}{the XML node}
  \item{name}{the name of the attribute}
  \item{default}{a value to use as the default return if the attribute
    is not present in the XML node. }
  \item{converter}{an optional function which if supplied is invoked
    with the attribute value and the value returned.
    This can be used to convert the string to an arbitrary
    value which is useful if it is, for example, a number.
    This is only called if the attribute exists within the node.
   In other words, it is not applied to the \code{default} value.}
}
\details{
  This just checks that the attribute list is
  non-NULL and that there is an element with
  the specified name.
}
\value{
  If the
  attribute is present,
  the return value is  a string which is the value of the attribute.
  Otherwise, the value of \code{default} is returned.
}
\references{\url{http://www.w3.org/XML}, \url{http://www.jclark.com/xml},
\url{http://www.omegahat.org}  }
\author{ Duncan Temple Lang }

\seealso{
 \code{\link{xmlAttrs}}
}

\examples{
 node <- xmlNode("foo", attrs=c(a="1", b="my name"))

 xmlGetAttr(node, "a")
 xmlGetAttr(node, "doesn't exist", "My own default value")

 xmlGetAttr(node, "b", "Just in case")
}
\keyword{file}

