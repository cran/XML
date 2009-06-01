\name{xmlParseDoc}
\Rdversion{1.1}
\alias{xmlParseDoc}
\alias{COMPACT}
\alias{DTDATTR}
\alias{DTDLOAD}
\alias{DTDVALID}
\alias{HUGE}
\alias{NOBASEFIX}
\alias{NOBLANKS}
\alias{NOCDATA}
\alias{NODICT}
\alias{NOENT}
\alias{NOERROR}
\alias{NONET}
\alias{NOWARNING}
\alias{NOXINCNODE}
\alias{NSCLEAN}
\alias{OLDSAX}
\alias{PEDANTIC}
\alias{RECOVER}
\alias{XINCLUDE}
\alias{OLD10}
\alias{SAX1}

\title{Parse an XML document with options controlling the parser.}

\description{
This function is a generalization of \code{\link{xmlParse}}
that parses an XML document.  With this function, we can specify
a combination of different options that control the operation of the
parser. The options control many different aspects the parsing process
 
}
\usage{
xmlParseDoc(file, options = 1L, encoding = character(), asText = !file.exists(file), baseURL = file)
}
\arguments{
  \item{file}{}
  \item{options}{}
  \item{encoding}{}
  \item{asText}{}
  \item{baseURL}{}
}
\details{}
\value{}
\references{libxml2}
\author{}
\note{
}

\seealso{}
\examples{}

\keyword{data}
\concept{XML}
