RSinteger <-
function(x, attrs)
{
 as.integer(xmlValue(x[[1]]))
}

RSdouble  <-
function(x, attrs)
{
 as.numeric(xmlValue(x[[1]]))
}


RSlogical  <-
function(x, attrs)
{
 as.numeric(xmlValue(x[[1]]))
}

RSstring  <-
function(x, attrs)
{
 as.character(xmlValue(x[[1]]))
}


RSArray <- 
function(x, attrs) {
  v <- unlist(xmlChildren(x))
  names(v) <- NULL
 v
}

RSlist <- 
function(x, attrs) {
 v <- xmlChildren(x)
 names(v) <- NULL
 v
}


RSname <-
function(x)
{
  xmlValue(x)
}

RSlist <-
function(x)
{
 v <- xmlChildren(x)
 names(v) <- NULL
 v
}

RSrownames <-
function(x)
{
 x[[1]]
}

RSnamedList <-
function(x)
{
  n <- xmlSize(x)
  which <- seq(1, by=2, length= n/2)
  elNames <- sapply(x[which], function(x) xmlValue(x[[1]]))
  v <- lapply(x[which +1], function(x) x[[1]])
  names(v) <- elNames

 return(v)
}

RSlongitudinal <- 
function(x, attrs)
{
  v <- list(times=x[["times"]], cov=x[["cov"]])
  class(v) <- "lgtdl"
  v
}


RSobject <- 
function(x, attrs)
{
  v <- lapply(xmlElementsByTagName(x, "slot"), function(x) x[[1]])
  names(v) <- sapply(xmlElementsByTagName(x, "slot"), function(x) xmlAttrs(x)["name"])
  class(v) <- xmlAttrs(x)["type"]
  v
}


RSstartElement <-
function(x)
{
}


readXML<-
function(x)
{
  UseMethod("readXML")
}

readXML.XMLNode <- 
#
# This is a generic-style function
# which looks for a method/function for the particular
# XML tag name. This is not keyed off the class of the XMLNode
# argument at this point. That happens in the generic readXML.
function(x)
{
  name <- paste("readXML", xmlName(x), sep=".")
  if(exists(name, mode="function")) {
    f <- get(name)
    return(f(x))
  } else
    x
}

getRSXMLHandlers <-
#
# This merges the default 
#
#
function(...)
{
 handlers <- list(integer=RSinteger, double=RSdouble, logical = RSlogical, string = RSstring, array =RSArray, namedlist = RSnamedList, object = RSobject, list=RSlist, factor = RSfactor, dataframe = RSdataframe, startElement = readXML)

 args <- list(...)
 which <- (1:length(handlers))[is.na(match(names(handlers), names(args)))]
 if(length(which))
   args <- c(args, handlers[which])

 args
}

RSxmlScan <-
function(file, handlers = getRSXMLHandlers(...), ...) 
{
  xmlRoot(xmlTreeParse(file, handler=handlers, asTree=T))
}


RSfactor <-
function(x)
{
  vals <- x[["values"]]
  levs <- x[["levels"]]

  ordered <- as.logical(xmlAttrs(x)["ordered"])
  if(length(ordered) == 0)
    ordered <- F

  if(is.null(levs))
    f <- factor(unlist(vals), ordered = ordered)
  else
    f <- factor(vals, levels = unlist(levs), ordered = ordered)
     

 f
}

RSdataframe <-
function(x)
{
print(mode(x[[1]]))
 df <- as.data.frame(x[[1]])
 if(!is.null(x[["array"]]))
   rownames(df) <-  x[["array"]]

 df
}

