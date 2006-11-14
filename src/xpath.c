#include "RS_XML.h"
#include <libxml/xpath.h>
#include "Utils.h"



SEXP
convertNodeSetToR(xmlNodeSetPtr obj, SEXP fun)
{
  SEXP ans, expr = NULL, arg = NULL, ref;
  int i;

  if(!obj)
     return(NULL_USER_OBJECT);

  PROTECT(ans = NEW_LIST(obj->nodeNr));

  if(GET_LENGTH(fun) && TYPEOF(fun) == CLOSXP) {
    PROTECT(expr = allocVector(LANGSXP, 2));
    SETCAR(expr, fun);
    arg = CDR(expr);
  } else if(TYPEOF(fun) == LANGSXP) {
    expr = fun;
    arg = CDR(expr);
  }

  for(i = 0; i < obj->nodeNr; i++) {
    ref = R_createXMLNodeRef(obj->nodeTab[i]);
    if(expr) {
      PROTECT(ref);
      SETCAR(arg, ref);
      PROTECT(ref = Rf_eval(expr, R_GlobalEnv)); /*XXX do we want to catch errors here? Maybe to release the namespaces. */
      SET_VECTOR_ELT(ans, i, ref);
      UNPROTECT(2);
    } else
      SET_VECTOR_ELT(ans, i, ref);
  }

  if(expr) {
    if(TYPEOF(fun) == CLOSXP) 
      UNPROTECT(1);
  } else
    SET_CLASS(ans, mkString("XMLNodeSet"));

  UNPROTECT(1);

  return(ans);
}

SEXP
convertXPathObjectToR(xmlXPathObjectPtr obj, SEXP fun)
{
  SEXP ans = NULL_USER_OBJECT;

  switch(obj->type) {

    case XPATH_NODESET:
        ans = convertNodeSetToR(obj->nodesetval, fun);
	break;
    case XPATH_BOOLEAN:
	ans = ScalarLogical(obj->boolval);
	break;
    case XPATH_NUMBER:
	ans = ScalarReal(obj->floatval);
	if(xmlXPathIsInf(obj->floatval))
	    REAL(ans)[0] = xmlXPathIsInf(obj->floatval) < 0 ? R_NegInf : R_PosInf;
        else if(xmlXPathIsNaN(obj->floatval))
	    REAL(ans)[0] = NA_REAL;
	break;
    case XPATH_STRING:
	ans = mkString(XMLCHAR_TO_CHAR(obj->stringval));
	break;
    case XPATH_POINT:
    case XPATH_RANGE:
    case XPATH_LOCATIONSET:
    case XPATH_USERS:
	PROBLEM "currently unsupported xmlXPathObject type %d in convertXPathObjectToR. Please send mail to maintainer.", obj->type
        WARN
    default:
	ans = R_NilValue;
  }

  return(ans);
}


#include <libxml/xpathInternals.h> /* For xmlXPathRegisterNs() */
xmlNsPtr *
R_namespaceArray(SEXP namespaces, xmlXPathContextPtr ctxt)
{
 int i, n;
 SEXP names = GET_NAMES(namespaces);
 xmlNsPtr *els;

 n = GET_LENGTH(namespaces);
 els = xmlMallocAtomic(sizeof(xmlNsPtr) * n);
 if(!els) {
   PROBLEM  "Failed to allocated space for namespaces"
   ERROR;
 }

 for(i = 0; i < n; i++) {
/*XXX who owns these strings. */
   const xmlChar *prefix, *href;
   href = CHAR_TO_XMLCHAR(strdup(CHAR_DEREF(STRING_ELT(namespaces, i))));
   prefix = names == NULL_USER_OBJECT ?  CHAR_TO_XMLCHAR("") /* NULL */ 
                                      :  CHAR_TO_XMLCHAR(strdup(CHAR_DEREF(STRING_ELT(names, i))));
   els[i] = xmlNewNs(NULL, href, prefix);
   if(ctxt) 
       xmlXPathRegisterNs(ctxt, prefix, href);
 }

 return(els);
}


SEXP
RS_XML_xpathEval(SEXP sdoc, SEXP path, SEXP namespaces, SEXP fun)
{
 xmlXPathContextPtr ctxt = NULL;
 xmlXPathObjectPtr result;
 SEXP ans = NULL_USER_OBJECT;

 xmlDocPtr doc;

 if(TYPEOF(sdoc) != EXTPTRSXP || R_ExternalPtrTag(sdoc) != Rf_install("XMLInternalDocument")) {
   PROBLEM "xpathEval must be given an internal XML document object, 'XMLInternalDocument'"
   ERROR;
 }

 doc = (xmlDocPtr) R_ExternalPtrAddr(sdoc);
 ctxt = xmlXPathNewContext(doc);

 if(GET_LENGTH(namespaces)) {
     ctxt->namespaces =  R_namespaceArray(namespaces, ctxt); /* xmlCopyNamespaceList(doc); */
     ctxt->nsNr = GET_LENGTH(namespaces);
 }


 result = xmlXPathEvalExpression(CHAR_TO_XMLCHAR(CHAR_DEREF(STRING_ELT(path, 0))), ctxt);

 if(result)
     ans = convertXPathObjectToR(result, fun);
 
 xmlXPathFreeObject(result);
 xmlXPathFreeContext(ctxt);

 if(!result) {
   PROBLEM  "error evaluating xpath expression %s", CHAR_DEREF(STRING_ELT(path, 0))
   ERROR;
 }

 return(ans);
}
