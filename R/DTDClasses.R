#
# Some methods for the DTD classes, similar in spirit
# to those in XMLClasses
#
#    print()
#
#
#
# XMLSystemEntity
# XMLEntity
# XMLElementDef
# XMLSequenceContent
# XMLOrContent
# XMLElementContent
# XMLAttributeDef
#


print.XMLElementDef <-
function(def)
{
 cat("<!ELEMENT",def$name," ")
 print(def$contents)
 cat(">\n")
 if(length(def$attributes)) {

 cat("<!ATTLIST ",def$name,"\n")
  for(i in def$attributes) {
    cat("\t")
    print(i)
    cat("\n")
  }
  cat(">\n")
 }
 
}


print.XMLElementContent <-
function(el)
{
 if(names(el$type)[1] == "PCData") {
   cat(" ( #PCDATA ) ")
   return()
 }
 cat("(")
 cat(el$elements)
 cat(")",switch(names(el$ocur)[1],Once="", "One or More"="+","Zero or One"="?","Mult"="*")) 
}


print.XMLOrContent <-
function(el)
{
 n <- length(el$elements)
 cat("( ")
 for(i in 1:n) {
   print(el$elements[[i]])
   if(i < n)
    cat(" | ")
 }
 cat(" )")
}

print.XMLSequenceContent <-
function(el)
{
 cat("( ")
 n <- length(el$elements)
 for(i in 1:n) {
    print(el$elements[[i]])
    if(i < n)
        cat(", ")
 }
 cat(" )")
}


print.XMLAttributeDef <-
function(def)
{
 if(names(def$defaultType)[1] != "Implied")
   dflt <- paste("\"",def$defaultValue,"\"",collapse="",sep="")
 else
  dflt <- ""

 cat(def$name, xmlAttributeType(def), xmlAttributeType(def,T), dflt)
}

xmlAttributeType <-
function(def, defaultType = F)
{

 if(defaultType == F & names(def$type)[1] == "Enumeration") {
   return( paste("(",paste(def$defaultValue,collapse=" | "),")", sep=" ", collapse="") )
 }

 switch(ifelse(defaultType, names(def$defaultType)[1], names(def$type)[1]),
         "Fixed" = "#FIXED",
         "CDATA" = "CDATA",
         "Implied" = "#IMPLIED",
         "Required" = "#REQUIRED",
         "Id" = "#ID",
         "IDRef" = "#IDREF",
         "IDRefs" = "#IDREFS",
         "Entity" = "#ENTITY",
         "Entities" = "ENTITIES",
         "NMToken" = "#NMTOKEN",
         "NMTokens" = "#NMTOKENS",
         "Enumeration" = "",
         "Notation" = "",
         "<BROKEN>"
       )
}


print.XMLEntity <-
function(ent)
{
 cat("<!ENTITY %", ent$name,paste("\"",ent$value,"\"",sep="",collapse=""), ">\n")
}


xmlAttrs.XMLElementDef <-
function(def)
{
 def$attributes
}

