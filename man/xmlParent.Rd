\name{xmlParent}
\alias{xmlParent}
\alias{xmlParent.XMLInternalNode}
\title{Get parent node of XMLInternalNode}
\description{
 This operates on an XML node
 and returns a reference to its parent node 
 within the document tree.
 This works for an internal, C-level
 \code{XMLInternalNode} object
 created, for examply, using \code{\link{newXMLNode}}
 and related functions or \code{\link{xmlTree}}
 or  from \code{\link{xmlTreeParse}} with the
 \code{useInternalNodes} parameter.

 It is possible to find the parent of an R-level
 XML node when using a tree
 created with, for example,  \code{\link{xmlHashTree}}
 as the parent information is stored separately.
}
\usage{
xmlParent(x)
}
\arguments{
  \item{x}{an object of class \code{XMLInternalNode} whose parent is being requested. }
}
\details{
 This uses the internal libxml structures to access the parent in the DOM tree. 
 This function is generic so that we can add methods for other types of nodes
 if we so want in the future.
}
\value{
 An object of class \code{XMLInternalNode}.
}

\references{\url{http://www.w3.org/XML}}
\author{ Duncan Temple Lang }

\seealso{
 \code{\link{xmlChildren}}
 \code{\link{xmlTreeParse}}
 \code{\link{xmlNode}}
}
\examples{

  top = newXMLNode("doc")
  s = newXMLNode("section", attr = c(title = "Introduction"))
  a = newXMLNode("article", s)
  addChildren(top, a)

  xmlName(xmlParent(s))
  xmlName(xmlParent(xmlParent(s)))


    # Find the root node.
  root = a
  while(!is.null(xmlParent(root)))
      root = xmlParent(root)

   # find the names of the parent nodes of each 'h' node.
   # use a global variable to "simplify" things and not use a closure.

  filename = system.file("exampleData", "branch.xml", package = "XML")
  parentNames <- character()
  xmlTreeParse(filename,
                handlers =
                  list(h = function(x) {
                   parentNames <<- c(parentNames, xmlName(xmlParent(x)))
                  }), useInternalNodes = TRUE)

  table(parentNames)
}
\keyword{file}
\keyword{IO}


