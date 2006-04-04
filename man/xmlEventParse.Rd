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
xmlEventParse(file, handlers=xmlEventHandler(), ignoreBlanks = FALSE, addContext=TRUE,
               useTagName=TRUE, asText = FALSE, trim=TRUE, useExpat=FALSE, isURL = FALSE,
                state = NULL, replaceEntities = TRUE, validate = FALSE,
                 saxVersion = 1)
}
\arguments{
  \item{file}{the source of the XML content.
    This can be a string givinging the name of a file or remote URL,
    the XML itself, a connection object, or a function.
    If this is a string, and \code{asText} is \code{TRUE},
    the value is the XML content.
    This allows one to read the content separately from parsing
    without having to write it to a file.
    If \code{asText} is \code{FALSE} and a string is passed
    for \code{file}, this is taken as the name of a
    file or remote URI. If one is using the libxml parser (i.e. not expat),
    this can be a URI accessed via HTTP or FTP or a compressed local file.
    If it is the name of a local file,
    it can include \code{~}, environment variables, etc. which will be expanded by R.
    (Note this is not the case in S-Plus, as far as I know.)

    If a connection is given, the parser incrementally reads one line at
    a time by calling the function \code{\link[base]{readLines}} with
    the connection as the first argument (and \code{1} as the number of
    lines to read).  The parser calls this function each time it needs
    more input.

    If invoking the \code{readLines} function to get each line is
    excessively slow or is inappropriate, one can provide a function as the value
    of \code{fileName}. Again, when the XML parser needs more content
    to process, it invokes this function to get a string.
    This function is called with a single argument, the maximum size
    of the string that can be returned.
    The function is responsible for accessing the correct connection(s),
    etc. which is typically done via lexical scoping/environments.
    This mechanism allows the user to control how the XML content
    is retrieved in very general ways. For example, one might
    read from a set of files, starting one when the contents
    of the previous file have been consumed. This allows for the
    use of hybrid connection objects.

    Support for connections and functions in this form is only
    provided if one is using libxml2 and not libxml version 1.
}
 \item{handlers}{ a closure object that contains  functions which will be invoked
as the XML components in the document are encountered by the parser. 
 The standard functions are 
\code{startElement()}, \code{endElement()}
\code{comment()}, \code{externalEntity()},
\code{entityDeclaration()}, \code{processingInstruction()},
\code{text()}, \code{cdata()},
\code{startDocument()}, and \code{endDocument()}.
  }
 \item{ignoreBlanks}{a logical value indicating whether
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
  }
  \item{replaceEntities}{
   logical value indicating whether to substitute entity references
    with their text directly. This should be left as False.
    The text still appears as the value of the node, but there
    is more information about its source, allowing the parse to be reversed
    with full reference information.
  }
  \item{saxVersion}{an integer value which should be either 1 or 2.
    This specifies which SAX interface to use in the C code.
    The essential difference is the number of arguments passed to the
    \code{startElement} handler function(s).  In addition to the name of
    the element and the named-attributes vector, two additional arguments
    are provided.
    The first identifies the namespace of the element.
    This is a named character vector of length 1,
    with the value being the URI of the namespace and the name
    being the prefix that identifies that namespace within the document.
    For example, \code{xmlns:r="http://www.r-project.org"}
    would be passed as \code{c(r = "http://www.r-project.org")}.
    If there is no prefix because the namespace is being used as the
    default, the result of calling \code{\link[base]{names}} on
    the string is \code{""}.
    The fourth argument gives the collection of all the namespaces
    defined within this element.
    Again, this is a named character vector.
  }
  \item{validate}{
  Currently, this has no effect as the libxml2 parser uses a
  document structure to do validation.
  a logical indicating whether to use a validating parser or not, or in other words
  check the contents against the DTD specification. If this is true, warning
  messages will be displayed about errors in the DTD and/or document, but the parsing 
  will proceed except for the presence of terminal errors.
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

\references{\url{http://www.w3.org/XML}, \url{http://www.jclark.com/xml}}
\author{Duncan Temple Lang}
\note{
 The libxml parser can read URLs via http or ftp.
It does not require the support of \textbf{wget} as used
in other parts of \R, but uses its own facilities
to connect to remote servers.
}

\seealso{ \link{xmlTreeParse} }

\examples{
 fileName <- system.file("exampleData", "mtcars.xml", package="XML")

   # Print the name of each XML tag encountered at the beginning of each
   # tag.
   # Uses the libxml SAX parser.
 xmlEventParse(fileName,
                list(startElement=function(name, attrs){
                                    cat(name,"\n")
                                  }),
                useTagName=FALSE, addContext = FALSE)


\dontrun{
  # Parse the text rather than a file or URL by reading the URL's contents
  # and making it a single string. Then call xmlEventParse
xmlURL <- "http://www.omegahat.org/Scripts/Data/mtcars.xml"
xmlText <- paste(scan(xmlURL, what="",sep="\n"),"\n",collapse="\n")
xmlEventParse(xmlText, asText=TRUE)
}

    # Using a state object to share mutable data across callbacks
f <- system.file("exampleData", "gnumeric.xml", package = "XML")
zz <- xmlEventParse(f,
                    handlers = list(startElement=function(name, atts, .state) {
                                                     .state = .state + 1
                                                     print(.state)
                                                     .state
                                                 }), state = 0)
print(zz)




    # Illustrate the startDocument and endDocument handlers.
xmlEventParse(fileName,
               handlers = list(startDocument = function() {
                                                 cat("Starting document\n")
                                               },
                               endDocument = function() {
                                                 cat("ending document\n")
                                             }),
               saxVersion = 2)




if(libxmlVersion()$major >= 2) {


 startElement = function(x, ...) cat(x, "\n")


 xmlEventParse(file(f), handlers = list(startElement = startElement))


 # Parse with a function providing the input as needed.
 xmlConnection = 
  function(con) {

   if(is.character(con))
     con = file(con, "r")
  
   if(isOpen(con, "r"))
     open(con, "r")

   function(len) {

     if(len < 0) {
        close(con)
        return(character(0))
     }

      x = character(0)
      tmp = ""
    while(length(tmp) > 0 && nchar(tmp) == 0) {
      tmp = readLines(con, 1)
      if(length(tmp) == 0)
        break
      if(nchar(tmp) == 0)
        x = append(x, "\n")
      else
        x = tmp
    }
    if(length(tmp) == 0)
      return(tmp)
  
    x = paste(x, collapse="")

    x
  }
 }

 ff = xmlConnection(f)
 xmlEventParse(ff, handlers = list(startElement = startElement))

  # Parse from a connection. Each time the parser needs more input, it
  # calls readLines(<con>, 1)
 xmlEventParse(file(f),  handlers = list(startElement = startElement))


  # using SAX 2
 h = list(startElement = function(name, attrs, namespace, allNamespaces){ 
                                 cat("Starting", name,"\n")
                                 if(length(attrs))
                                     print(attrs)
                                 print(namespace)
                                 print(allNamespaces)
                         },
          endElement = function(name, uri) {
                          cat("Finishing", name, "\n")
            }) 
 xmlEventParse(system.file("exampleData", "namespaces.xml", package="XML"), handlers = h, saxVersion = 2)

}
}
\keyword{file}
\keyword{IO}
