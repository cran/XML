\name{xmlHandler}
\alias{xmlHandler}
\title{ Example XML Event Parser Handler Functions}
\description{
 A closure containing simple functions for the different
 types of events potentially called by the \link{xmlEventParse},
 and some tag-specific functions to illustrate how one can
 add functions for specific DTDs and XML element types.
 Contains a local \link{list} which can be mutated
 by invocations of the closure's function. 
}
\usage{
xmlHandler()
}

\value{
 List containing the functions enumerated
 in the closure definition along with the
 \link{list}.

}
\author{Duncan Temple Lang}
\note{This is just an example.}

\seealso{\link{xmlEventParse}, \link{xmlTreeParse}}

\examples{
\dontrun{
xmlURL <- "http://www.omegahat.org/Scripts/Data/mtcars.xml"
xmlText <- paste(scan.url(xmlURL, what="", sep="\n"),"\n",collapse="\n")
}

xmlURL <- system.file("exampleData", "mtcars.xml", package="XML")
xmlText <- paste(scan(xmlURL, what="", sep="\n"), "\n", collapse="\n")
xmlEventParse(xmlText, handlers = NULL, asText=T)
xmlEventParse(xmlText, xmlHandler(), useTagName=T, asText=T)
}
\keyword{file}
\keyword{XML}
