\name{libxmlVersion}
\alias{libxmlVersion}
\title{Get the version of the libxml library.}
\description{
This currently retrieves the version of the libxml
library used when installing this XML package.
}
\usage{
libxmlVersion()
}
\details{
This computes the version at compile time.
In the future we will hopefully find a way to
find the run-time version.
}
\value{
A named list with
fields
\item{major}{the major version number, either 1 or 2
  indicating the old or new-style library.}
\item{minor}{the within version release number.}
\item{patch}{the within minor release version number}
}

\references{\url{http://www.w3.org/XML}, \url{http://www.xmlsoft.org},
\url{http://www.omegahat.org}  }
\author{ Duncan Temple Lang }

\seealso{}

\examples{
 ver <- libxmlVersion()
 if(is.null(ver)) {
   cat("Relly old version of libxml\n")
 } else {
   if(ver$major > 1) {
     cat("Using libxml2\n")
   }
 }
}
\keyword{IO}
