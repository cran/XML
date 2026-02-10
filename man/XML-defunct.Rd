\name{XML-defunct}
\alias{XML-defunct}
\alias{parseDTD}
\title{Defunct functions in the \CRANpkg{XML} package}
\description{
  The functions listed here no longer work.
}
\usage{
  parseDTD(extId, asText = FALSE, name = "", isURL = FALSE, error = xmlErrorCumulator())
}
\details{
  In the next version of \CRANpkg{XML}, the function \code{parseDTD}
  will stop working altogether (signal a \link{.Defunct} error) instead
  of returning an empty object.
}
\keyword{internal}
