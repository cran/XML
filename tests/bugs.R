library(XML)

top <- xmlRoot(xmlParse("<top><a/><b/><c/></top>"))
gc()
b <- replaceNodes(top[["b"]], newXMLTextNode("Hello"))
gc()
rm(b) # used to consider itself the last reference to the document and free it
gc()
top

d <- (top2 <- xmlRoot(xmlParse("<top2><d/></top2>", options = NODICT)))[["d"]]
gc() # now d is the last reference to the second document
a <- replaceNodes(top[["a"]], d)
rm(d, top2)
gc() # the second document is no longer referenced
replaceNodes(top[["d"]], a)
rm(a)
gc() # now d should be cleaned up
top

rm(top)
gc()
