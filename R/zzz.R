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
function(...)
{
 if(exists("setMethod")) {
#   .InitSAXMethods()
 }
   # Set the error handlers to our local ones.
 .C("RSXML_setErrorHandlers")
}

#
#  Copyright (c) 1998, 1999 The Omega Project for Statistical Computing.
#       All rights reserved.#
