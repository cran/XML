\name{readHTMLTable}
\alias{readHTMLTable}
\alias{readHTMLTable,character-method}
\alias{readHTMLTable,HTMLInternalDocument-method}
\alias{readHTMLTable,XMLInternalElementNode-method}
\title{Read data from one or more HTML tables}
\description{
 This function and its methods provide somewhat robust methods for
 extracting data from HTML tables in an HTML document.
 One can read all the tables in a document given by filename or URL,
 or having already parsed the document via \code{\link{htmlParse}}.
 Alternatively, one can specify an individual \code{<table>}
 node in the document.

 The methods attempt to do some heuristic computations to determine
 the header labels for the columns, the name of the table, etc.
}
% xmlName(node) == "table" && ("thead" \%in\% names(node) || length(getNodeSet(node, "./tr[1]/th")) > 0)
\usage{
readHTMLTable(doc, header = NA,
              colClasses = NULL, skip.rows = integer(), trim = TRUE, elFun = xmlValue, as.data.frame = TRUE, ...)
}
\arguments{
  \item{doc}{the HTML document which can be a file name or a URL
    or an already parsed \code{HTMLInternalDocument}, or
    an HTML node of class \code{XMLInternalElementNode}.}
  \item{header}{either a logical value indicating whether the table has
    column labels, e.g. the first row or a \code{thead}, or alternatively
   a character vector giving the names to use for the resulting columns.}
  \item{colClasses}{}
  \item{skip.rows}{}
  \item{trim}{a logical value indicating whether to remove leading and
 trailing white space from the content cells.}
  \item{elFun}{a function which, if specified, is called when converting
     each cell. Currently, only the node is specified. In the future, we might 
     additionally pass the index of the column so that the function has
    some context, e.g. whether the value is a row label or a regular
    value, or if the caller knows the type of columns.
  }
  \item{as.data.frame}{a logical value indicating whether to turn the
    resluting table(s) into data frames or leave them as matrices.
   }
   \item{\dots}{currently additional parameters that are passed on to
     \code{\link{as.data.frame}} if \code{as.data.frame} is \code{TRUE}.
    We may change this to use these as additional arguments for calls to
 \code{elFun}.}
}
\value{
  If the document (either by name or parsed tree) is specified,
  the return vale is a list of data frames or matrices.
  If a single HTML node is provided
}
\references{
HTML4.0 specification  
}
\author{Duncan Temple Lang}
\seealso{
  \code{\link{htmlParse}}
  \code{\link{getNodeSet}}
  \code{\link{xpathSApply}}    
}
\examples{
 u = "http://en.wikipedia.org/wiki/World_population"
 
 tables = readHTMLTable(u)
 names(tables)

 tables[[2]]
  # Print the table. Note that the values are all characters
  # not numbers. Also the column names have a preceding X since
  # R doesn't allow the variable names to start with digits.
 tmp = tables[[2]]

  # We can transform this to get the rows to be years and the columns
  # to be population counts. We'll create a matrix.
 vals = cbind(year = as.integer(gsub("X", "", names(tmp)[-1])),
              matrix(as.integer(gsub(",", "", as.character(unlist(tmp[-1])))),
                      ncol(tmp)-1, byrow = TRUE, dimnames = list(NULL, as.character(tmp[[1]]))))

   # Let's just read the second table directly by itself.
 doc = htmlParse(u)
 tableNodes = getNodeSet(doc, "//table")
 tb = readHTMLTable(tableNodes[[2]])

  # Let's try to adapt the values on the fly.
  # We'll create a function that turns a th/td node into a val
 tryAsInteger = function(node) {
                  val = xmlValue(node)
                  ans = as.integer(gsub(",", "", val))
                  if(is.na(ans))
                      val
                  else
                      ans
                }

 tb = readHTMLTable(tableNodes[[2]], elFun = tryAsInteger)

 tb = readHTMLTable(tableNodes[[2]], elFun = tryAsInteger,
                       colClasses = c("character", rep("integer", 9)))
}
\keyword{IO}
\keyword{data}

