.First.lib <-
function(libname, pkgname)
{
 library.dynam("XML", pkgname, libname)

 if(.useNamespacesInXMLPackage && exists("setMethod")) {
   .InitSAXMethods()
 }

   # Set the error handlers to our local ones.
 .C("RSXML_setErrorHandlers", PACAKAGE = "XML")
}

.onLoad =
function(...)
{
 if(exists("setMethod")) {
   .InitSAXMethods()
 }
}

#
#  Copyright (c) 1998, 1999 The Omega Project for Statistical Computing.
#       All rights reserved.#
