trim =
function(x)
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)


textNodesOnly =
  # Only process the top-level text nodes, not recursively.
  # Could be done as simply as
  #     xmlValue(x, recursive = FALSE)
function(x)
    paste(xmlSApply(x, function(n) if(is(n, "XMLInternalTextNode")) xmlValue(n) else ""), collapse = "")


toNumber =
function(x)
{
  as.numeric(gsub("[%,]", "", x))
}


if(FALSE) {
  doc = htmlParse("http://elections.nytimes.com/2008/results/states/president/california.html")
  tbls = getNodeSet(doc, "//table[not(./tbody)]|//table/tbody")
  o = readHTMLTable(tbls[[1]], skip.rows = c(1, Inf), header = FALSE, colClasses = c("character", replicate(5, toNumber)), elFun = textOnly)


  o = readHTMLTable("http://elections.nytimes.com/2008/results/states/president/california.html")  

  x = readHTMLTable("http://www.usatoday.com/news/politicselections/vote2004/CA.htm", as.data.frame = FALSE)  
}

setGeneric("readHTMLTable",
          function(doc, header = xmlName(node) == "table" && ("thead" %in% names(node) || length(getNodeSet(node, "./tr[1]/th")) > 0),
                    colClasses = NULL, skip.rows = integer(), trim = TRUE, elFun = xmlValue,
                     as.data.frame = TRUE, ...)           
             standardGeneric("readHTMLTable"))

setMethod("readHTMLTable", "character",
          function(doc, header = NA,
                    colClasses = NULL, skip.rows = integer(), trim = TRUE, elFun = xmlValue,
                     as.data.frame = TRUE, ...) {
              pdoc = htmlParse(doc)
              readHTMLTable(pdoc, header, colClasses, skip.rows, trim, elFun, as.data.frame, ...)
          })

setMethod("readHTMLTable", "HTMLInternalDocument",
          function(doc, header = NA,
                    colClasses = NULL, skip.rows = integer(), trim = TRUE, elFun = xmlValue,
                     as.data.frame = TRUE, ...)           
{
     #  tbls = getNodeSet(doc, "//table[not(./tbody)]|//table/tbody")
   tbls = getNodeSet(doc, "//table")
      # if header is missing, compute it each time.
   ans = lapply(tbls, readHTMLTable,  header, colClasses, skip.rows, trim, elFun, as.data.frame, ...)
   names(ans) = sapply(tbls, getHTMLTableName)
   ans
})

getHTMLTableName =
function(node)
{
  id = xmlGetAttr(node, "id")
  if(!is.null(id))
    return(id)

  cap = getNodeSet(node, "./caption")
  if(length(cap))
    return(xmlValue(cap[[1]]))
}


setMethod("readHTMLTable", "XMLInternalElementNode",
#readHTMLTable.XMLInternalElementNode  =
  #
  #
  # header is computed based on whether we have a table node and it has a thead.
  #  (We don't currently bother with the col spans.)
  #
  #  colClasses can be a character vector giving the name of the type for a column,
  #  an NULL to drop the corresponding column, or a function in which case it will
  #  be passed the contents of the column and can transform it as it wants.
  #  This allows us to clean text before converting it.
  #
  # skip.rows - an integer vector indicating which rows to ignore.
  #
  #  trim - a logical indicating whether to trim white space from the start and end of text.
  #
  #  elFun - a function which is called to process each th or td node to extract the content.
  #      This is typically xmlValue, but one can supply others (e.g. textNodesOnly)

  #  as.data.frame
  #

function(doc, header = xmlName(node) == "table" && ("thead" %in% names(node) || length(getNodeSet(node, "./tr[1]/th")) > 0) ,
          colClasses = NULL, skip.rows = integer(), trim = TRUE, elFun = xmlValue,
            as.data.frame = TRUE, ...)
{
  node = doc
  headerFromTable = FALSE
  dropFirstRow = FALSE

  if(is.logical(header) && (is.na(header) || header) &&  xmlName(node) == "table") {
    if("thead" %in% names(node))
       header = node[["thead"]]
    else if(all(names(node[["tr"]]) %in% c('text', 'th'))) {
       header = xpathSApply(node[["tr"]], "./th", xmlValue)
       dropFirstRow = TRUE
    }
  }

  if(is(header, "XMLInternalElementNode"))   {
     header = as.character(xpathSApply(header, ".//th|.//td", elFun))
     headerFromTable = TRUE

     if(xmlName(node) == "table" && "tbody" %in% names(node))
        node = node[["tbody"]]
   }
  
     # Process each row, by getting the content of each "cell" (th/td)
  rows = getNodeSet(node, "./tr")
  if(dropFirstRow)
     rows = rows[-1]
  els =  lapply(rows, function(row) {
                           tmp = xpathSApply(row, "./th|./td", elFun)
                           if(trim)
                              trim(tmp)
                           else
                              tmp
                        })
  
  if(length(skip.rows)) {
    infs = (skip.rows == Inf)
    if(any(infs))
          # want Inf - 2, Inf - 1, Inf,  to indicate drop last 3, but that won't work
          # take sequence of Inf to identify Inf - 2, Inf - 1, Inf
       skip.rows[skip.rows == Inf] = length(els)  - seq(0, length = sum(infs))  
    els = els[ - skip.rows ]
  }

  if(length(els) == 0)
    return(NULL)
  
   numEls = sapply(els, length)
   if(is.logical(header) && !is.na(header) && header) {
     header = els[[1]]
     els = els[-1]
     numEls = numEls[ - 1]
   } 

   ans = lapply(seq(length = max(numEls)),
                  function(col) {
                    sapply(els, `[`, col)
                  })

   if(is.character(header))
      names(ans) = header

   if(length(colClasses)) {

      colClasses = rep(colClasses, length = length(ans))
     
      n = sapply(colClasses, is.null)
      if(any(n)) {
         ans = ans[ ! n ]
         colClasses = colClasses[ ! n ]
      }

      ans = lapply(seq(along = ans) ,
                      function(i) 
                         if(is.function(colClasses[[i]]))
                            colClasses[[i]](ans[[i]])
                         else
                            as(ans[[i]], colClasses[[i]])
                   )
   }

   if(as.data.frame)  {
     ans = as.data.frame(ans, ...)
     if(is.character(header))
        names(ans) = header
   }
    
  ans
})
