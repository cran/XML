\name{XML-defunct}
\alias{XML-defunct}
\alias{dtdElementValidEntry}
\alias{dtdElement}
\alias{dtdEntity}
\alias{dtdIsAttribute}
\alias{dtdValidElement}
\alias{parseDTD}
\alias{xmlContainsElement}
\alias{xmlContainsEntity}
\title{Defunct functions in the \CRANpkg{XML} package}
\description{
  The functions listed here no longer work.
}
\usage{
  dtdElement(name, dtd)
  dtdElementValidEntry(element, name, pos=NULL)
  dtdEntity(name, dtd)
  dtdIsAttribute(name, element, dtd)
  dtdValidElement(name, within, dtd, pos=NULL)
  parseDTD(extId, asText = FALSE, name = "", isURL = FALSE, error = xmlErrorCumulator())
  xmlContainsElement(name, dtd)
  xmlContainsEntity(name, dtd)
}
\details{
  These functions relied on direct member access to DTD-related
  structures in \code{libxml2}, which is no longer supported.
}
\keyword{internal}
