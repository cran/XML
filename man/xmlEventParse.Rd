\name{xmlEventParse}
\alias{xmlEventParse}

\title{ XML Event/Callback element-wise Parser}
\description{
 Reads and processes the contents of an XML file
  or string by
 invoking user-level functions associated with different
 components of the XML tree. These include beginning and end
 of XML elements, comments, CDATA (escaped character data), entities, processing
 instructions, etc.
 This allows the caller to create the appropriate data structure from the
 XML document contents rather than the default tree (see \link{xmlTreeParse}).
 Functions for specific tags/elements can be used in addition to the 
 standard callback names.
}
\usage{
xmlEventParse(file, handlers=xmlHandler(), ignoreBlanks = FALSE, addContext=TRUE,
               useTagName=TRUE, asText = FALSE, trim=TRUE, useExpat=FALSE, isURL = FALSE,
                state = NULL, replaceEntities = TRUE)
}
\arguments{
 \item{file}{ string identifying the file that is interpreted
  using the internal expansion mechanism so it can contain \~
and other environment variables.
As with \code{\link{xmlTreeParse}}, if \code{useExpat}
is false, this can be a URL (http or ftp) or a compressed file,
as well as a regular local file.
}
 \item{handlers}{ a closure object that contains  functions which will be invoked
as the XML components in the document are encountered by the parser. 
 The standard functions are 
\code{startElement()}, \code{endElement()}
\code{comment()}, \code{externalEntity()},
\code{entityDeclaration()}, \code{processingInstruction},
\code{text()}.
  }
 \item{ignoreBlanks}{ logical value indicating whether
text elements made up entirely of white space should be included
in the resulting `tree'. }
 \item{addContext}{ logical value indicating whether the callback functions 
in `handlers' should be invoked with contextual  information about
the parser and the position in the tree, such as node depth, 
path indices for the node relative the root, etc.
If this is True, each callback function  should support 
\dots.
}
 \item{useTagName}{ logical value indicating whether 
 the callback mechanism should look  for a function
 matching the tag name in the startElement and
 endElement events, before calling the default handler
 functions. This allows the caller to handle different
 element types for a particular DTD with their own functions directly, rather
 than performing a second dispatch in \code{startElement()}.
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
% \item{restartCounter}{}
 \item{useExpat}{
   a logical value indicating whether to use the expat SAX parser,
  or to default to the libxml.
   If this is TRUE, the library must have been compiled with support for expat.
   See \link{supportsExpat}.
 }
 \item{isURL}{
   indicates whether the \code{file}  argument refers to a URL
  (accessible via ftp or http) or a regular file on the system.
  If \code{asText} is TRUE, this should not be specified.
}
\item{state}{an optional S object that is passed to the
  callbacks and can be modified to communicate state between
  the callbacks. If this is given, the callbacks should accept
  an argument  named \code{.state} and it should return an object
  that will be used as the updated value of this state object.
  The new value can be any S object and will be passed to the next 
  callback where again it will be updated by that functions return
  value, and so on. 
  If this not specified in the call to \code{xmlEventParse},
  no \code{.state} argument is passed to the callbacks. This makes the
  interface compatible with previous releases.
  .}
  \item{replaceEntities}{
   logical value indicating whether to substitute entity references
    with their text directly. This should be left as False.
    The text still appears as the value of the node, but there
    is more information about its source, allowing the parse to be reversed
    with full reference information.
    }
}
\details{
 This is now implemented using the libxml parser.
 Originally, this was implemented via the Expat XML parser by
 Jim Clark (\url{http://www.jclark.com}).
}
\value{
  The return value is the `handlers'
argument. It is assumed that this is a closure and that
the callback functions have manipulated variables
local to it and that the caller knows how to extract this.
}
\Notes{
 The libxml parser can read URLs via http or ftp.
It does not require the support of \textbf{wget} as used
in other parts of \R, but uses its own facilities
to connect to remote servers.
}
\references{\url{http://www.w3.org/XML}, \url{http://www.jclark.com/xml}}
\author{Duncan Temple Lang}
\note{ This requires the Expat XML parser to be installed. }

\seealso{ \link{xmlTreeParse} }

\examples{
 fileName <- system.file("exampleData", "mtcars.xml", package="XML")

   # Print the name of each XML tag encountered at the beginning of each
   # tag.
   # Uses the libxml SAX parser.
 xmlEventParse(fileName,
                list(startElement=function(name, attrs){cat(name,"\n")}),
                useTagName=FALSE, addContext = FALSE)


\dontrun{
  # Parse the text rather than a file or URL by reading the URL's contents
  # and making it a single string. Then call xmlEventParse
xmlURL <- "http://www.omegahat.org/Scripts/Data/mtcars.xml"
xmlText <- paste(scan.url(xmlURL, what="",sep="\n"),"\n",collapse="\n")
xmlEventParse(xmlText, asText=TRUE)
}

 # Using a state object to share mutable data across callbacks
f <- system.file("exampleData", "gnumeric.xml", package = "XML")
zz <- xmlEventParse(f,
                    handlers = list(startElement=function(name, atts,.state) {
                                                     state = .state + 1
                                                     print(.state)
                                                     .state}), state = 0)
print(zz)
}
\keyword{file}
\keyword{IO}
