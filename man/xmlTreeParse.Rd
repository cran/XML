\name{xmlTreeParse}
\alias{xmlTreeParse}
\alias{htmlTreeParse}
\title{XML Parser}
\description{
 Parses an XML or HTML file or string, and generates an R 
 structure representing the XML/HTML tree.
 Use \code{htmlTreeParse} when the content is known
 to be (potentially malformed) HTML.
}
\usage{
xmlTreeParse(file, ignoreBlanks=TRUE, handlers=NULL, replaceEntities=FALSE,
              asText=FALSE, trim=TRUE, validate=FALSE, getDTD=TRUE,
               isURL=FALSE, asTree = FALSE, addAttributeNamespaces = FALSE,
               useInternalNodes = FALSE, isSchema = FALSE)
htmlTreeParse(file, ignoreBlanks = TRUE, handlers = NULL,
               replaceEntities = FALSE, asText = FALSE, trim = TRUE,
               isURL = FALSE, asTree = FALSE, 
               useInternalNodes = FALSE) 
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
  objects. Typically, this is a named list of functions,
   and a closure can be used to provide local data.
  This provides a way of filtering the tree as it is being
  created, adding or removing nodes, and generally processing
  them as they are constructed in the C code.

   In a recent addition to the package (version 0.99-8),
   if this is specified as a single function object,
   we call that function for each node in the underlying DOM tree.
   It is invoked with the new node and its parent node.
  }
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
  not the R facilities (e.g. \code{\link{scan}}).
 }
 \item{asTree}{this only applies when on passes a value for
   the  \code{handlers} argument and is used then to determine
   whether the DOM tree should be returned or the \code{handlers}
   object.
 }
 \item{addAttributeNamespaces}{a logical value indicating whether to
   return the namespace in the names of the attributes within a node
   or to omit them. If this is \code{TRUE}, an attribute such as
   \code{xsi:type="xsd:string"} is reported with the name
     \code{xsi:type}.
    If it is \code{FALSE}, the name of the attribute is \code{type}.}
 \item{useInternalNodes}{a logical value indicating whether 
  to call the converter functions with objects of class
   \code{XMLInternalNode} rather than \code{XMLNode}.
   This should make things faster as we do not convert  the 
  contents of the internal nodes to R explicit objects.
  Also, it allows one to access the parent and ancestor nodes.
  However, since the objects refer to volatile C-level objects,
  one cannot store these nodes for use in further computations within R.
  They \dQuote{disappear} after the processing the XML document is completed.

  If this argument is \code{TRUE} and no handlers are provided, the
  return value is a reference to the internal C-level document pointer.
  This can be used to do post-processing via XPath expressions using
  \code{\link{getNodeSet}}.
}
\item{isSchema}{a logical value indicating whether the document
  is an XML schema (\code{TRUE}) and should be parsed as such using
  the built-in schema parser in libxml.}
}
\details{
 The \code{handlers} argument is used similarly
to those specified in \link{xmlEventParse}.
 When an XML tag (element) is processed,
  we look for a function in this collection 
  with the same name as the tag's name. 
  If this is not found, we look for one named
  \code{startElement}. If this is not found, we use the default
  built in converter.
  The same works for comments, entity references, cdata, processing instructions,
  etc.
 The default entries should be named
\code{comment}, \code{startElement},
\code{externalEntity},
\code{processingInstruction},
\code{text}, \code{cdata} and \code{namespace}.
All but the last should take the XMLnode as their first argument.
In the future, other information may be passed via \dots,
for example, the depth in the tree, etc.
Specifically, the second argument will be the parent node into which they
are being added, but this is not currently implemented,
so should have a default value (\code{NULL}).

The \code{namespace} function is called with a single argument which
is an object of class \code{XMLNameSpace}.  This contains
\begin{description} 
\item[id] the namespace identifier as used to
qualify tag names; 
\item[uri] the value of the namespace identifier,
i.e. the URI
 identifying the namespace.
\item[local] a logical value indicating whether the definition
is local to the document being parsed.
\end{description}

One should note that the \code{namespace} handler is called before the
node in which the namespace definition occurs and its children are
processed.  This is different than the other handlers which are called
after the child nodes have been processed.

Each of these functions can return arbitrary values that are then
entered into the tree in place of the default node passed to the
function as the first argument.  This allows the caller to generate
the nodes of the resulting document tree exactly as they wish.  If the
function returns \code{NULL}, the node is dropped from the resulting
tree. This is a convenient way to discard nodes having processed their
contents.

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
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}
\note{Make sure that the necessary 3rd party libraries are available.}

\seealso{ \link{xmlEventParse} }

\examples{
 fileName <- system.file("exampleData", "test.xml", package="XML")
   # parse the document and return it in its standard format.
 xmlTreeParse(fileName)

   # parse the document, discarding comments.
  
 xmlTreeParse(fileName, handlers=list("comment"=function(x,...){NULL}), asTree = TRUE)

   # print the entities
 invisible(xmlTreeParse(fileName,
            handlers=list(entity=function(x) {
                                    cat("In entity",x$name, x$value,"\n")
                                    x}
                                  ), asTree = TRUE
                          )
          )

 # Parse some XML text.
 # Read the text from the file
 xmlText <- paste(readLines(fileName), "\n", collapse="")

 xmlTreeParse(xmlText, asText=TRUE)


 # Read a MathML document and convert each node
 # so that the primary class is 
 #   <name of tag>MathML
 # so that we can use method  dispatching when processing
 # it rather than conditional statements on the tag name.
 # See plotMathML() in examples/.
 fileName <- system.file("exampleData", "mathml.xml",package="XML")
m <- xmlTreeParse(fileName, 
                  handlers=list(
                   startElement = function(node){
                   cname <- paste(xmlName(node),"MathML", sep="",collapse="")
                   class(node) <- c(cname, class(node)); 
                   node
                }))



  # In this example, we extract _just_ the names of the
  # variables in the mtcars.xml file. 
  # The names are the contents of the <variable>
  # tags. We discard all other tags by returning NULL
  # from the startElement handler.
  #
  # We cumulate the names of variables in a character
  # vector named `vars'.
  # We define this within a closure and define the 
  # variable function within that closure so that it
  # will be invoked when the parser encounters a <variable>
  # tag.
  # This is called with 2 arguments: the XMLNode object (containing
  # its children) and the list of attributes.
  # We get the variable name via call to xmlValue().

  # Note that we define the closure function in the call and then 
  # create an instance of it by calling it directly as
  #   (function() {...})()

  # Note that we can get the names by parsing
  # in the usual manner and the entire document and then executing
  # xmlSApply(xmlRoot(doc)[[1]], function(x) xmlValue(x[[1]]))
  # which is simpler but is more costly in terms of memory.
 fileName <- system.file("exampleData", "mtcars.xml", package="XML")
 doc <- xmlTreeParse(fileName,  handlers = (function() { 
                                 vars <- character(0) ;
                                list(variable=function(x, attrs) { 
                                                vars <<- c(vars, xmlValue(x[[1]])); 
                                                NULL}, 
                                     startElement=function(x,attr){
                                                   NULL
                                                  }, 
                                     names = function() {
                                                 vars
                                             }
                                    )
                               })()
                     )

  # Here we just print the variable names to the console
  # with a special handler.
 doc <- xmlTreeParse(fileName, handlers = list(
                                  variable=function(x, attrs) {
                                             print(xmlValue(x[[1]])); TRUE
                                           }), asTree=TRUE)


  # This should raise an error.
  try(xmlTreeParse(
            system.file("exampleData", "TestInvalid.xml", package="XML"),
            validate=TRUE))

\dontrun{
 # Parse an XML document directly from a URL.
 # Requires Internet access.
 xmlTreeParse("http://www.omegahat.org/Scripts/Data/mtcars.xml", asText=TRUE)
}

  counter = function() {
              counts = integer(0)
              list(startElement = function(node) {
                                     name = xmlName(node)
                                     if(name \%in\% names(counts))
                                          counts[name] <<- counts[name] + 1
                                     else
                                          counts[name] <<- 1
                                  },
                    counts = function() counts)
            }

   h = counter()
   xmlTreeParse(system.file("exampleData", "mtcars.xml", package="XML"),  handlers = h, useInternalNodes = TRUE)
   h$counts()




 getLinks = function() { 
       links = character() 
       list(a = function(node, ...) { 
                   links <<- c(links, xmlGetAttr(node, "href"))
                   node 
                }, 
            links = function()links)
     }

 h1 = getLinks()
 htmlTreeParse(system.file("examples", "index.html", package = "XML"), handlers = h1)
 h1$links()

 h2 = getLinks()
 htmlTreeParse(system.file("examples", "index.html", package = "XML"), handlers = h2, useInternalNodes = TRUE)
 all(h1$links() == h2$links())

  # Using flat trees
 tt = xmlHashTree()
 f = system.file("exampleData", "mtcars.xml", package="XML")
 xmlTreeParse(f, handlers = tt[[".addNode"]])
 xmlRoot(tt)
}

\keyword{file}
\keyword{IO}

