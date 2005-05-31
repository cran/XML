\name{Doctype}
\alias{Doctype}
\alias{coerce,Doctype,character-method}
\title{Constructor for DTD reference}
\description{
 This is a constructor for the \code{Doctype} class.
 Also, there is a method for converting such a \code{Doctype}
 object to a character string.
}
\usage{
Doctype(system = "", public = "", name = "")
}
\arguments{
  \item{system}{the system URI that locates the DTD. }
  \item{public}{the identifier for locating the DTD in a catalog, for example. }
  \item{name}{the name of the root element in the document}
}
\value{
 An object of class \code{Doctype}.
}
\references{\url{http://www.w3.org/XML}}
\author{ Duncan Temple Lang }


\seealso{
 \code{\link{saveXML}}
}
\examples{
}
\keyword{IO}

