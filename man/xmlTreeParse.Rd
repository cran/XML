\name{xmlTreeParse}
\alias{xmlTreeParse}
\title{XML Parser}
\description{
 Parses an XML file or string, and generates an R 
 structure representing the XML tree.
}
\usage{
xmlTreeParse(file, ignoreBlanks=T, handlers=NULL, replaceEntities=F,asText=F, trim=T, validate=F, getDTD=F, isURL=F)
}

\arguments{
 \item{file}{ The name of the file containing the XML contents.
This can contain \~ which is expanded to the user's
home directory.
It can also be a URL. See \code{isURL}.
Additionally, the file can be compressed (gzip)
and is read directly without the user having
to de-compress (gunzip) it.}
 \item{ignoreBlanks}{ logical value indicating whether
text elements made up entirely of white space should be included
in the resulting `tree'. }
 \item{handlers}{Optional collection of functions
  used to map the different XML nodes to R
  objects. This is a named list of functions,
   and a closure can be used to provide local data.}
  \item{replaceEntities}{
   logical value indicating whether to substitute entity references
    with their text directly. This should be left as False.
    The text still appears as the value of the node, but there
    is more information about its source, allowing the parse to be reversed
    with full reference information.
    }
  \item{asText}{logical value indicating that the first argument,
    `file', 
     should be treated as the XML text to parse, not the name of 
     a file. This allows the contents of documents to be retrieved 
     from different sources (e.g. HTTP servers, XML-RPC, etc.) and still
     use this parser.}
 \item{trim}{
  whether to strip white space from the beginning and end of text strings.
}
\item{validate}{
logical indicating whether to use a validating parser or not, or in other words
check the contents against the DTD specification. If this is true, warning
messages will be displayed about errors in the DTD and/or document, but the parsing 
will proceed except for the presence of terminal errors.
}
\item{getDTD}{
logical flag indicating whether the DTD (both internal and external)
should be returned along with the document nodes. This changes the 
return type.
}
 \item{isURL}{
   indicates whether the \code{file}  argument refers to a URL
  (accessible via ftp or http) or a regular file on the system.
  If \code{asText} is TRUE, this should not be specified.
  The function attempts to determine whether the 
  data source is a URL by using \code{\link{grep}}
  to look for http or ftp at the start of the string.
  The libxml parser handles the connection to servers,
  not the R facilities (e.g. \code{\link{scan.url}}).
 }
}
\details{
 The \code{handlers} argument is used similarly
to those specifid in \link{xmlEventParse}.
 When an XML tag (element) is processed,
  we look for a function in this collection 
  with the same name as the tag's name. 
  If this is not found, we look for one named
  startElement. If this is not found, we use the default
  built in converter.
  The same works for comments, entity references, etc.
 The default entries should be named
\code{comment}, \code{startElement},
\code{externalEntity},
\code{processingInstruction}
\code{text}.
They should take the XMLnode as their first argument.
In the future, other information may be passed via \dots,
for example, the depth in the tree, etc.
Specifically, the second argument will be the parent node into which they
are being added, but this is not currently implemented,
so should have a default value (\code{NULL}).

Each of these functions can return arbitrary values that are then
entered into the tree in place of the default node passed to the
function as the first argument.  This allows the caller to generate
the nodes of the resulting document tree exactly as they wish.  If the
function returns \code{NULL}, in the future, we will drop this node
from the tree.

}
\value{
By default, an object of class XML doc is returned,
 which contains fields/slots named 
 \code{file}, \code{version} and \code{children}.
  \item{\code{file}}{The (expanded) name of the file
 containing the XML.}
  \item{\code{version}}{A string identifying the 
 version of XML used by the document.}
  \item{\code{children}}{
 A list of the XML nodes at the top of the document.
 Each of these is of class \code{XMLNode}.
 These are made up of 4 fields.
   \item{\code{name}}{The name of the element.}
   \item{\code{attributes}}{For regular elements, a named list
     of XML attributes converted from the 
       <tag x="1" y="abc">}
   \item{\code{children}}{List of sub-nodes.}
   \item{\code{value}}{Used only for text entries.}
 Some nodes specializations of \code{XMLNode}, such as 
 \code{XMLComment}, \code{XMLProcessingInstruction},
 \code{XMLEntityRef} are used.

If the value of the argument getDTD is TRUE, the return value is a
list of length 2.  The first element is as the document as described
above.  The second element is a list containing the external and
internal DTDs. Each of these contains 2 lists - one for elements
and another for entities. See \code{\link{parseDTD}}.
}
}
\references{\url{http://xmlsoft.org}, 
\url{http://www.w3.org/xml}}
\author{Duncan Temple Lang}
\note{Make sure that the necessary 3rd party libraries are available.}

\seealso{ \link{xmlEventParse} }

\examples{
 fileName <- system.file("data/test.xml", pkg="XML")
   # parse the document and return it in its standard format.
 xmlTreeParse(fileName)

   # parse the document, discarding comments.
  
 xmlTreeParse(fileName, handlers=list("comment"=function(x, parent){NULL}))

 invisible(xmlTreeParse(fileName,
            handlers=list(entity=function(x) {
                                    cat("In entity",x$name, x$value,"\n")}
                                  )
                          )
         )

 # Parse some XML text.
 # Read the text from the file
 xmlText <- paste(scan(fileName, what="",sep="\n"),"\n", collapse="\n")
 xmlTreeParse(xmlText, asText=T)

 # Read a MathML document and convert each node
 # so that the primary class is 
 #   <name of tag>MathML
 # so that we can use method  dispatching when processing
 # it rather than conditional statements on the tag name.
 # See plotMathML() in examples/.
 fileName <- system.file("data/mathml.xml",pkg="XML")
m <- xmlTreeParse(fileName, 
                  handlers=list(startElement=function(node){
                              cname <- paste(xmlName(node),"MathML",sep="",collapse="")
                              class(node) <- c(cname, class(node)); 
                              node
                }))

\dontrun{
 # Parse an XML document directly from a URL.
 # Requires Internet access.
 xmlTreeParse("http://www.omegahat.org/Scripts/Data/mtcars.xml", asText=T)
}
}

\keyword{file}
\keyword{XML}
\keyword{DTD}
