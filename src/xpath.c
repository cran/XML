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
      xmlNodePtr el;
      el = obj->nodeTab[i];
      if(el->type == XML_ATTRIBUTE_NODE) {
	  PROTECT(ref = mkString((el->children && el->children->content) ? XMLCHAR_TO_CHAR(el->children->content) : ""));
	  SET_NAMES(ref, mkString(el->name));
	  SET_CLASS(ref, mkString("XMLAttributeValue"));
	  UNPROTECT(1);
      } else if(el->type == XML_NAMESPACE_DECL)
	  ref = R_createXMLNsRef((xmlNsPtr) el);
      else
	ref = R_createXMLNodeRef(el);

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
        ans = mkString(XMLCHAR_TO_CHAR(obj->stringval)); //XXX encoding 
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
R_addXMLInternalDocument_finalizer(SEXP sdoc, SEXP fun)
{
    R_CFinalizer_t action;

    if(TYPEOF(fun) == CLOSXP) {
	R_RegisterFinalizer(sdoc, fun);	
	return(sdoc);
    }

    if(fun == R_NilValue)    {
        action = R_xmlFreeDoc;
    } else if(TYPEOF(fun) == EXTPTRSXP)
	action = (R_CFinalizer_t) R_ExternalPtrAddr(fun);

    R_RegisterCFinalizer(sdoc, action);
    return(sdoc);
}


SEXP
R_XMLInternalDocument_free(SEXP sdoc)
{
  if(TYPEOF(sdoc) != EXTPTRSXP || R_ExternalPtrTag(sdoc) != Rf_install("XMLInternalDocument")) {
     PROBLEM "R_free must be given an internal XML document object, 'XMLInternalDocument'"
     ERROR;
  }

  R_xmlFreeDoc(sdoc);
  
  return(sdoc);
}


SEXP
RS_XML_xpathEval(SEXP sdoc, SEXP r_node, SEXP path, SEXP namespaces, SEXP fun)
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

 if(GET_LENGTH(r_node)) {
     ctxt->node = ctxt->origin = R_ExternalPtrAddr(r_node);
 }

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

USER_OBJECT_
RS_XML_createDocFromNode(USER_OBJECT_ s_node)
{
 xmlDocPtr doc;
 xmlNodePtr node, ptr;
 SEXP ans;

 node = (xmlNodePtr) R_ExternalPtrAddr(s_node);
 doc = xmlNewDoc(CHAR_TO_XMLCHAR("1.0"));
 R_numXMLDocs++;

 ptr = xmlDocCopyNode(node, doc, 1);
 node = (xmlNodePtr) doc;
 xmlAddChild(node, ptr);

 ans = R_createXMLDocRef(doc);
 return(ans);
}

USER_OBJECT_
RS_XML_copyNodesToDoc(USER_OBJECT_ s_node, USER_OBJECT_ s_doc)
{
 xmlDocPtr doc;
 xmlNodePtr node, ptr;
 int len, i;
 SEXP ans;

 doc = (xmlDocPtr) R_ExternalPtrAddr(s_doc);

 if(TYPEOF(s_node) == EXTPTRSXP) {
     node = (xmlNodePtr) R_ExternalPtrAddr(s_node);
     ptr = xmlDocCopyNode(node, doc, 1);
     return(R_createXMLNodeRef(ptr));
 }

 len = Rf_length(s_node);
 PROTECT(ans = NEW_LIST(ans));
 for(i = 0; i < len; i++) {
     node = (xmlNodePtr) R_ExternalPtrAddr(VECTOR_ELT(s_node, i));
     ptr = xmlDocCopyNode(node, doc, 1);
     SET_VECTOR_ELT(ans, i, R_createXMLNodeRef(ptr)); 
 }
 UNPROTECT(1);
 return(ans);
}

/*
 Thoughts that we could set the kids to NULL and then free the doc
  after we createDocFromNode but the return of xpathApply will return
  these nodes and we need to be able to get to a document
 */
SEXP
RS_XML_killNodesFreeDoc(SEXP sdoc)
{
   xmlDocPtr doc = (xmlDocPtr) R_ExternalPtrAddr(sdoc);
   if(!doc) {
       PROBLEM "null xmlDocPtr passed as externalptr to RS_XML_killNodesFreeDoc"
	   WARN;
       return(ScalarLogical(FALSE));
   }
   doc->children = NULL;
   xmlFree(doc);
   return(ScalarLogical(TRUE));
}


#if 0
SEXP
RS_XML_xpathNodeEval(SEXP s_node, SEXP path, SEXP namespaces, SEXP fun)
{
 xmlXPathContextPtr ctxt = NULL;
 xmlXPathObjectPtr result;
 SEXP ans = NULL_USER_OBJECT;

 xmlDocPtr doc;

 if(TYPEOF(s_node) != EXTPTRSXP || R_ExternalPtrTag(s_node) != Rf_install("XMLInternalNode")) {
   PROBLEM "xpathEval must be given an internal XML document object, 'XMLInternalNode'"
   ERROR;
 }

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
#endif


SEXP
R_matchNodesInList(SEXP r_nodes, SEXP r_target, SEXP r_nomatch)
{
    xmlNodePtr el;
    int i, j, n, n2;
    SEXP ans;
      
    n = GET_LENGTH(r_nodes);
    n2 = GET_LENGTH(r_target);
    ans = NEW_INTEGER( n );
    for(i = 0; i < n ; i++) {
	el = R_ExternalPtrAddr(VECTOR_ELT(r_nodes, i));
	INTEGER(ans)[i] = INTEGER(r_nomatch)[0];
	for(j = 0; j < n2; j++) {
	    if(el == R_ExternalPtrAddr(VECTOR_ELT(r_target, j))) {
		INTEGER(ans)[i] = j;
		break; 
	    }
	}
    }

    return(ans);
}
