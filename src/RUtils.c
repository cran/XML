#include "Utils.h"

/*
  Utilities used in the R XML parsing facilities for invoking user-level functions from C.

 * See Copyright for the license status of this software.

 */

#include "Rinternals.h" /* Macros, etc. */

USER_OBJECT_ R_InternalRecursiveApply(USER_OBJECT_ top, USER_OBJECT_ func, USER_OBJECT_ klasses);

USER_OBJECT_
RS_XML(invokeFunction)(USER_OBJECT_ fun, USER_OBJECT_ opArgs, USER_OBJECT_ data)
{
  int i;
  long n;
  USER_OBJECT_ c, call;
  USER_OBJECT_ ans;

  n = GET_LENGTH(opArgs);
  if(data)
    n++;
  if(n  > 0) {
    PROTECT(c = call = allocList(n));
    for (i = 0; i < GET_LENGTH(opArgs); i++) {
      SETCAR(c, VECTOR_ELT(opArgs, i));
      c = CDR(c);
    }
    if(data) {
       SETCAR(c, data);
       SET_TAG(c, Rf_install(".state"));
    }

    call = LCONS(fun, call);
    UNPROTECT(1);
  } else  {
     call = allocVector(LANGSXP,1);
     SETCAR(call, fun);
  }  

  PROTECT(call);
   ans = eval(call, R_GlobalEnv);
  UNPROTECT(1);

  return(ans);
}

USER_OBJECT_
RS_XML(RecursiveApply)(USER_OBJECT_ top, USER_OBJECT_ func, USER_OBJECT_ klasses)
{
  USER_OBJECT_ ans;
  PROTECT(top = duplicate(top));
  ans = R_InternalRecursiveApply(top, func, klasses);
  UNPROTECT(1);
  return(ans);
}

USER_OBJECT_
R_InternalRecursiveApply(USER_OBJECT_ top, USER_OBJECT_ func, USER_OBJECT_ klasses)
{
  int CHILD_NODE = 2, i;
  USER_OBJECT_ kids;
  int numChildren;
  USER_OBJECT_ args, tmp;


  if(GET_LENGTH(top) > CHILD_NODE) {
    kids = VECTOR_ELT(top, CHILD_NODE);
    numChildren = GET_LENGTH(kids);
        /* Do the children first. */
    PROTECT(args = NEW_LIST(1));
    PROTECT(tmp = NEW_LIST(numChildren));  
    for(i = 0; i < numChildren; i++) {
      SET_VECTOR_ELT(tmp, i, R_InternalRecursiveApply(VECTOR_ELT(kids, i), func, klasses));
    }
    SET_VECTOR_ELT(top, CHILD_NODE, tmp);
    UNPROTECT(2);
  }

  PROTECT(args = NEW_LIST(1));
  SET_VECTOR_ELT(args, 0, top);
  tmp =  RS_XML(invokeFunction)(func, args, NULL);
  UNPROTECT(1);

  return(tmp);
}

USER_OBJECT_
RS_XML_SubstituteEntitiesDefault(USER_OBJECT_ replaceEntities)
{
    int value;
    USER_OBJECT_ ans;
    value = xmlSubstituteEntitiesDefault(LOGICAL_DATA(replaceEntities)[0]);   
    ans = NEW_LOGICAL(1);
    LOGICAL_DATA(ans)[0] = value;
    return(ans);
}

#include <R_ext/Rdynload.h>

/* Simple macro for expanding ENTRY(x, n) to {"<x>", (DL_FUNC) &<x>, <n>} */

#define ENTRY(name, n)  { #name, (DL_FUNC) &name, n }

static R_CallMethodDef callMethods[] = {
	ENTRY(RS_XML_RecursiveApply, 3),
	ENTRY(RS_XML_HtmlParseTree, 7),
	ENTRY(RS_XML_getDTD, 4),
	ENTRY(RS_XML_libxmlVersion, 0),
	ENTRY(RS_XML_Parse, 14),
	ENTRY(RS_XML_ParseTree, 16),
	ENTRY(R_newXMLDtd, 4),
	ENTRY(R_newXMLDoc, 2),
	ENTRY(R_newXMLNode, 4),
	ENTRY(R_newXMLTextNode, 2),
	ENTRY(R_xmlNewComment, 2),
	ENTRY(R_newXMLCDataNode, 2),
	ENTRY(R_newXMLPINode, 3),
	ENTRY(R_xmlNewNs, 3),
	ENTRY(R_xmlSetNs, 3),
	ENTRY(R_insertXMLNode, 2),
	ENTRY(R_saveXMLDOM, 6),
	ENTRY(R_xmlCatalogResolve, 3),
	ENTRY(RS_XML_xmlNodeNumChildren, 1),
        ENTRY(RS_XML_unsetDoc, 1),
        ENTRY(RS_XML_setDoc, 2),
        ENTRY(RS_XML_printXMLNode, 5),
        ENTRY(RS_XML_removeChildren, 3),
        ENTRY(RS_XML_clone, 2),
        ENTRY(RS_XML_addNodeAttributes, 2),
        ENTRY(RS_XML_removeNodeAttributes, 3),
        ENTRY(RS_XML_getNsList, 2),
        ENTRY(RS_XML_setNodeName, 2),
        ENTRY(R_xmlNsAsCharacter, 1),
        ENTRY(RS_XML_SubstituteEntitiesDefault, 1),
	{NULL, NULL, 0}
};

static R_CMethodDef cmethods[] = {
    ENTRY(RSXML_setErrorHandlers, 0),
    {NULL, NULL, 0}
};

void
R_init_XML(DllInfo *dll)
{
   R_useDynamicSymbols(dll, FALSE);
   R_registerRoutines(dll, cmethods, callMethods, NULL, NULL);
}





Rboolean
R_isInstanceOf(USER_OBJECT_ obj, const char *klass)
{

    USER_OBJECT_ klasses;
    int n, i;

    klasses = GET_CLASS(obj);
    n = GET_LENGTH(klasses);
    for(i = 0; i < n ; i++) {
	if(strcmp(CHAR_DEREF(STRING_ELT(klasses, i)), klass) == 0)
	    return(TRUE);
    }


    return(FALSE);
}
