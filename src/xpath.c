#include "RS_XML.h"

#include <libxml/xpath.h>

#include "Utils.h"



SEXP
convertNodeSetToR(xmlNodeSetPtr obj)
{
  SEXP ans;
  int i;

  if(!obj)
     return(NULL_USER_OBJECT);

  PROTECT(ans = NEW_LIST(obj->nodeNr));

  for(i = 0; i < obj->nodeNr; i++) {
     SET_VECTOR_ELT(ans, i, R_createXMLNodeRef(obj->nodeTab[i]));
  }

  SET_CLASS(ans, mkString("XMLNodeSet"));
  UNPROTECT(1);

  return(ans);
}

SEXP
convertXPathObjectToR(xmlXPathObjectPtr obj)
{
  SEXP ans = NULL_USER_OBJECT;

  switch(obj->type) {

    case XPATH_NODESET:
	ans = convertNodeSetToR(obj->nodesetval);
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
	PROBLEM "unsupported xmlXPathObject type %d in convertXPathObjectToR", obj->type
        WARN
    default:
	ans = R_NilValue;
  }

  return(ans);
}


xmlNsPtr *
R_namespaceArray(SEXP namespaces)
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
   els[i] = xmlNewNs(NULL, strdup(CHAR_DEREF(STRING_ELT(namespaces, i))),
                      strdup(CHAR_DEREF(STRING_ELT(names, i))));
 }

 return(els);
}


SEXP
RS_XML_xpathEval(SEXP sdoc, SEXP path, SEXP namespaces)
{
 xmlXPathContextPtr ctxt = NULL;
 xmlXPathObjectPtr result;
 SEXP ans = NULL_USER_OBJECT;

/*XXX */
 xmlDocPtr doc = (xmlDocPtr) R_ExternalPtrAddr(sdoc);
 ctxt = xmlXPathNewContext(doc);

 if(GET_LENGTH(namespaces)) {
    ctxt->namespaces =  R_namespaceArray(namespaces); /* xmlCopyNamespaceList(doc); */
    ctxt->nsNr = GET_LENGTH(namespaces);
 }


 result = xmlXPathEvalExpression(CHAR_DEREF(STRING_ELT(path, 0)), ctxt);

 if(result)
     ans = convertXPathObjectToR(result);
 
 xmlXPathFreeObject(result);
 xmlXPathFreeContext(ctxt);

 if(!result) {
   PROBLEM  "error evaluating xpath expression %s", CHAR_DEREF(STRING_ELT(path, 0))
   ERROR;
 }

 return(ans);
}
