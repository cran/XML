.First.lib <-
function(libname, pkgname)
{
 library.dynam("XML", pkgname, libname)

 if(.useNamespacesInXMLPackage && exists("setMethod")) {
   .InitSAXMethods()
 }

   # Set the error handlers to our local ones.
 .C("RSXML_setErrorHandlers")
}

.onLoad =
function(libname, pkgname)
{
  # Added by Uwe Ligges.
 if(.Platform$OS.type == "windows"){
     temp <- Sys.getenv("PATH")
     Sys.setenv("PATH" = paste(utils::normalizePath(file.path(libname, pkgname, "libs")), 
                               file.path(Sys.getenv("R_HOME"), "modules", fsep="\\"), temp, sep=";"))
     on.exit(Sys.setenv(PATH = temp))
 }     
 library.dynam("XML", pkgname, libname)


 if(exists("setMethod")) {
#   .InitSAXMethods()
 }
   # Set the error handlers to our local ones.
 .C("RSXML_setErrorHandlers")
}

.onUnload <- function (libpath)
{
   library.dynam.unload("XML", libpath)
}



.Call =
function(name, ...)
{
  base::.Call(name, ..., PACKAGE = "XML")
}

.C =
function(name, ...)
{
  base::.C(name, ..., PACKAGE = "XML")
}


#
#  Copyright (c) 1998, 1999 The Omega Project for Statistical Computing.
#       All rights reserved.#
