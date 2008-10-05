/**

 The purpose of this file is to provide the C-level facilities
 to create, modify and manage internal XML DOM nodes at the S
 language level. We want to be able to use the interface defined
 by xmlOutputDOM() and xmlOutputBuffer() but with an implementation
 that returns a tree that is built to be used with the libxml 
 data structures. So the intent is to incrementally add nodes
 to the tree in memory and then pass this to libxml to add it to 
 another tree or write it to a file, etc.

  The essential public/high-level functionality provided by the the S-leve interface 
  for building trees consists of:
 
   1) addTag
   2) closeTag
   3) addComment
   4) value

 addNode

   a) getOpenTag
   b) reset
 */

#include "RSCommon.h"
#include "RS_XML.h"


#ifdef FROM_GNOME_XML_DIR
#include <gnome-xml/parserInternals.h>
#include <gnome-xml/xmlmemory.h>
#else
#include <libxml/parserInternals.h>
#include <libxml/xmlmemory.h>
#endif


#define R_USE_XML_ENCODING 1
#include "Utils.h"  /* R_createXMLNodeRef(), Encoding macros. */


#ifdef USE_OLD_ROOT_CHILD_NAMES
# define XML_ROOT(n) (n)->childs
#else
# define XML_ROOT(n) (n)->xmlRootNode
#endif

/**
 Create a libxml comment node and return it as an S object
 referencing this value.
*/

USER_OBJECT_
R_xmlNewComment(USER_OBJECT_ str, USER_OBJECT_ sdoc)
{
    xmlNodePtr node;
    xmlDocPtr doc = NULL;
    xmlChar *txt;

    if(GET_LENGTH(sdoc))
	doc = (xmlDocPtr) R_ExternalPtrAddr(sdoc);

    txt = CHAR_TO_XMLCHAR(CHAR_DEREF(STRING_ELT(str, 0)));
    node =  doc ? xmlNewDocComment(doc, txt) : xmlNewComment(txt);

    return(R_createXMLNodeRef(node));
}

USER_OBJECT_
R_newXMLTextNode(USER_OBJECT_ value, USER_OBJECT_ sdoc)
{
   xmlNodePtr node;
    xmlDocPtr doc = NULL;
    xmlChar *txt;
    
    if(GET_LENGTH(sdoc))
	doc = (xmlDocPtr) R_ExternalPtrAddr(sdoc);

    txt = CHAR_TO_XMLCHAR(CHAR_DEREF(STRING_ELT(value, 0)));
    if(doc)
	node = xmlNewDocTextLen(doc, txt, strlen(XMLCHAR_TO_CHAR(txt)));
    else
	node = xmlNewText(txt);
	
    return(R_createXMLNodeRef(node));
}

USER_OBJECT_
R_newXMLCDataNode(USER_OBJECT_ sdoc, USER_OBJECT_ value)
{
  xmlDocPtr  doc = NULL;
  xmlNodePtr node;
  const char *tmp;

  if(GET_LENGTH(sdoc))
    doc = (xmlDocPtr) R_ExternalPtrAddr(sdoc);

  tmp = CHAR_DEREF(STRING_ELT(value,0));

  node = xmlNewCDataBlock(doc, CHAR_TO_XMLCHAR(tmp), strlen(tmp));

  return(R_createXMLNodeRef(node));
}


USER_OBJECT_
R_newXMLPINode(USER_OBJECT_ sdoc, USER_OBJECT_ name, USER_OBJECT_ content)
{
  xmlDocPtr  doc = NULL;
  xmlNodePtr node;

  if(GET_LENGTH(sdoc))
    doc = (xmlDocPtr) R_ExternalPtrAddr(sdoc);

  node = xmlNewPI(CHAR_TO_XMLCHAR(CHAR_DEREF(STRING_ELT(name, 0))), CHAR_TO_XMLCHAR(CHAR_DEREF(STRING_ELT(content, 0))));
  return(R_createXMLNodeRef(node));
}


USER_OBJECT_
R_newXMLNode(USER_OBJECT_ name, USER_OBJECT_ attrs, USER_OBJECT_ nameSpace, USER_OBJECT_ sdoc,
              USER_OBJECT_ nameSpaceDefinitions)
{
   xmlDocPtr doc = NULL;
   xmlNsPtr ns = NULL;
   xmlNodePtr node;

   if(GET_LENGTH(sdoc))
       doc = (xmlDocPtr) R_ExternalPtrAddr(sdoc);

    
   if(GET_LENGTH(nameSpace) > 0) {
       /* Need the default namespace and then also any other */
      CHAR_DEREF(STRING_ELT(nameSpace, 0));
   }

/*   ns = xmlNewNs(xmlGetRootElement(doc), ); */
   node = xmlNewDocNode(doc, ns, CHAR_TO_XMLCHAR(CHAR_DEREF(STRING_ELT(name, 0))), NULL);


#if 0
/* trying to do this in R now. */
   if((n = GET_LENGTH(nameSpaceDefinitions)) > 0) {
       /* Need the default namespace and then also any other */
     USER_OBJECT_ prefixes = GET_NAMES(nameSpaceDefinitions);
     for(i = 0; i < n ; i++) {
         xmlNewNs(node, CHAR_DEREF(STRING_ELT(nameSpaceDefinitions, i)), CHAR_DEREF(STRING_ELT(prefixes, i)));
     }
   }
#endif

#if 0
/* trying to do this in R now. */
   n = GET_LENGTH(attrs);
   if(n > 0) {
       USER_OBJECT_ attrNames = GET_NAMES(attrs);
       if(GET_LENGTH(attrNames) != n) {
	   PROBLEM "names of attributes is not the same length of attributes"
           ERROR;
       }
       for(i = 0; i < n ; i++) {
            xmlSetProp(node, CHAR_TO_XMLCHAR(CHAR_DEREF(STRING_ELT(attrNames, i))),
			  CHAR_TO_XMLCHAR(CHAR_DEREF(STRING_ELT(attrs, i))));
       }
   }
#endif


   if(doc && XML_ROOT(doc) == NULL) {
       XML_ROOT(doc) = node;
   }

   return(R_createXMLNodeRef(node));
}

USER_OBJECT_
RS_XML_getNextSibling(USER_OBJECT_ s_node, USER_OBJECT_ s_prev)
{
    xmlNodePtr node = (xmlNodePtr) R_ExternalPtrAddr(s_node), ptr;

    ptr = LOGICAL(s_prev)[0] ? node->next : node->prev;

    return(ptr ? R_createXMLNodeRef(ptr) : NULL_USER_OBJECT);
}


/*
  Add attributes to an existing node.
  At present, doesn't check for duplicates.
  Can do this in C or in R, but need to remove existing values, 
  and ensure that namespace considerations are handled properly.
 */
USER_OBJECT_
RS_XML_addNodeAttributes(USER_OBJECT_ s_node, USER_OBJECT_ attrs)
{
    int i, n;
    USER_OBJECT_ attr_names;
    xmlNodePtr node = (xmlNodePtr) R_ExternalPtrAddr(s_node);

    n = GET_LENGTH(attrs);
    attr_names = GET_NAMES(attrs);
    for(i = 0; i < n; i++) {
	xmlSetProp(node, CHAR_TO_XMLCHAR(CHAR_DEREF(STRING_ELT(attr_names, i))), CHAR_TO_XMLCHAR(CHAR_DEREF(STRING_ELT(attrs, i))));
    }
    
    return(ScalarInteger(n));
}

USER_OBJECT_
RS_XML_setNodeName(USER_OBJECT_ s_node, USER_OBJECT_ s_name)
{
    xmlNodePtr node = (xmlNodePtr) R_ExternalPtrAddr(s_node);
    xmlChar *name = CHAR_TO_XMLCHAR(CHAR_DEREF(STRING_ELT(s_name, 0)));
    xmlNodeSetName(node, name);

    return(NULL_USER_OBJECT);
}



USER_OBJECT_
RS_XML_removeNodeAttributes(USER_OBJECT_ s_node, USER_OBJECT_ attrs, USER_OBJECT_ asNamespace)
{
    int i, n;
    USER_OBJECT_ attr_names, ans;
    xmlNodePtr node = (xmlNodePtr) R_ExternalPtrAddr(s_node);

    n = GET_LENGTH(attrs);
    attr_names = GET_NAMES(attrs);
    PROTECT(ans = NEW_LOGICAL(n));
    for(i = 0; i < n; i++) {
	if(LOGICAL(asNamespace)[0]) {
	    xmlNsPtr ns = NULL;
	    xmlChar *id;
	    id = CHAR_TO_XMLCHAR(CHAR_DEREF(STRING_ELT(attr_names, i)));
	    ns = (xmlNsPtr) R_ExternalPtrAddr(VECTOR_ELT(attrs, i));
	    if(id[0])
		INTEGER(ans)[i] = xmlUnsetNsProp(node, ns, id);
	} else
	    INTEGER(ans)[i] = xmlUnsetProp(node, CHAR_TO_XMLCHAR(CHAR_DEREF(STRING_ELT(attrs, i))));
                             
    }
    UNPROTECT(1);
    
    return(ans);
}

#define GET_R_XML_NODE_PTR(x)   (xmlNodePtr) R_ExternalPtrAddr(s_node);


USER_OBJECT_
RS_XML_getNsList(USER_OBJECT_ s_node, USER_OBJECT_ asRef)
{
    xmlNodePtr node = GET_R_XML_NODE_PTR(s_node);
    xmlNsPtr *els, el;
    int n = 0, i;
    USER_OBJECT_ ans, names;
    DECL_ENCODING_FROM_NODE(node)

    els = xmlGetNsList(node->doc, node);
    if(!els) 
	return(NULL_USER_OBJECT);

    el = *els;
    while(el) {
	n++;
	el = el->next;
    }
    el = *els;

    if(LOGICAL(asRef)[0]) {
	PROTECT(ans = NEW_LIST(n));	
	PROTECT(names = NEW_CHARACTER(n));    
	for(i = 0; i < n ; i++, el = el->next) {
	    if(el->prefix)
		SET_STRING_ELT(names, i, COPY_TO_USER_STRING(XMLCHAR_TO_CHAR(el->prefix)));
	    SET_VECTOR_ELT(ans, i, R_createXMLNsRef(el));
	}
    } else {
	
	PROTECT(ans = NEW_CHARACTER(n));
	PROTECT(names = NEW_CHARACTER(n));    
	for(i = 0; i < n ; i++, el = el->next) {
	    if(el->prefix)
		SET_STRING_ELT(names, i, COPY_TO_USER_STRING(XMLCHAR_TO_CHAR(el->prefix)));
	    if(el->href)
   	        SET_STRING_ELT(ans, i, COPY_TO_USER_STRING(XMLCHAR_TO_CHAR(el->href)));
	}
    }

    SET_NAMES(ans, names);
    UNPROTECT(2);
    return(ans);
}

SEXP
R_removeInternalNode(SEXP r_node, SEXP r_free)
{
    xmlNodePtr node;
    int n = GET_LENGTH(r_node), i;

    for(i = 0; i < n; i++) {
	SEXP el = VECTOR_ELT(r_node, i);
	if(TYPEOF(el) != EXTPTRSXP) {
	    PROBLEM "removeInternalNode needs ans external pointer object"
	    ERROR;
	}

	node = (xmlNodePtr) R_ExternalPtrAddr(el);
	if(!node) {
	    PROBLEM "removeInternalNode ignoring a NULL external pointer object"
		WARN;	
	}
	xmlUnlinkNode(node);
    
	if(LOGICAL(r_free)[i])
	    xmlFreeNode(node);
    }

    return(NULL_USER_OBJECT);
}

SEXP
RS_XML_setRootNode(USER_OBJECT_ r_doc, USER_OBJECT_ r_node)
{
    xmlDocPtr doc;
    xmlNodePtr node;

    doc = (xmlDocPtr) R_ExternalPtrAddr(r_doc);
    node = (xmlNodePtr) R_ExternalPtrAddr(r_node);

    if(!node->doc)
	node->doc = doc;
    xmlDocSetRootElement(doc, node);

    return(ScalarLogical(TRUE));
}

/**
   Add the internal XML node represented by the S object @node
   as a child of the XML node represented by the S object @parent.
 */
USER_OBJECT_
R_insertXMLNode(USER_OBJECT_ node, USER_OBJECT_ parent, USER_OBJECT_ at, USER_OBJECT_ shallow)
{
    xmlNodePtr n, p;

    if(TYPEOF(parent) != EXTPTRSXP) {
       PROBLEM "R_insertXMLNode expects XMLInternalNode objects for the parent node"
       ERROR;
    }

    if(IS_LIST(node))  {
      int i;
      for(i = 0; i < GET_LENGTH(node); i++)
	  R_insertXMLNode(VECTOR_ELT(node, i), parent, R_NilValue/*XXX*/, shallow);

      return(NULL_USER_OBJECT);
    }


    if(TYPEOF(node) == STRSXP) {
        int i;
	p = (xmlNodePtr) R_ExternalPtrAddr(parent);	
	for(i = 0; i < GET_LENGTH(node); i++) {
  	    n = xmlNewText(CHAR(STRING_ELT(node, i)));
   	    xmlAddChild(p, n);
	}
    }

    if(TYPEOF(node) != EXTPTRSXP) {
       PROBLEM "R_insertXMLNode expects XMLInternalNode objects"
       ERROR;
    }

    p = (xmlNodePtr) R_ExternalPtrAddr(parent);
    n = (xmlNodePtr) R_ExternalPtrAddr(node);

    if(!p || !n) {
	PROBLEM "either the parent or child node is NULL"
        ERROR;
    }

    if(n->parent == p) {
	xmlUnlinkNode(n);
    } 

      /* Make certain the nodes belong to this document if they already belong to another by copying. */
    if(n->doc && n->doc != p->doc) {
	n = xmlDocCopyNode(n, p->doc, 1);
    } else if(!n->doc && LOGICAL(shallow)[0]) {
	/* XXX This is intended to avoid setting all the nodes to this document and then having to undo that
                later on.*/
      n->doc = p->doc;
    }


    switch(p->type) {
    case XML_ELEMENT_NODE:
    case XML_DOCUMENT_NODE:	
	xmlAddChild(p, n);
	break;
    case XML_PI_NODE:
	xmlAddSibling(p, n);
	break;
    default:
       {
	   PROBLEM "ignoring request to add child (types parent: %d, child %d)",
               p->type, n->type
	    WARN
       }
	break;
    }
    return(NULL_USER_OBJECT);     
}

USER_OBJECT_
RS_XML_xmlAddSiblingAt(USER_OBJECT_ r_to, USER_OBJECT_ r_node, USER_OBJECT_ r_after)
{
    xmlNodePtr p, n, ans;
    
    xmlNodePtr (*f)(xmlNodePtr, xmlNodePtr);

    p = (xmlNodePtr) R_ExternalPtrAddr(r_to);
    n = (xmlNodePtr) R_ExternalPtrAddr(r_node);

    if(!p || !n) {
	PROBLEM "either the parent or child node is NULL"
        ERROR;
    }
	
    f = LOGICAL(r_after)[0] ?  xmlAddNextSibling : xmlAddPrevSibling ;
    ans = f(p, n);

    return(R_createXMLNodeRef(ans));
}

USER_OBJECT_
RS_XML_replaceXMLNode(USER_OBJECT_ r_old, USER_OBJECT_ r_new)
{
    xmlNodePtr Old, New, ans;

    if(TYPEOF(r_old) != EXTPTRSXP && TYPEOF(r_new) != EXTPTRSXP) {
       PROBLEM "R_replaceXMLNode expects XMLInternalNode objects"
       ERROR;
    }
    Old = (xmlNodePtr) R_ExternalPtrAddr(r_old);
    New = (xmlNodePtr) R_ExternalPtrAddr(r_new);

    if(!Old) {
	PROBLEM "NULL value for XML node to replace"
	    ERROR;
    }

    ans = xmlReplaceNode(Old, New);
    return(R_createXMLNodeRef(ans));  
}


/*
  a = newXMLNode("a", newXMLNode("b", newXMLNode("c", 3)), newXMLNode("d", "text"))
  removeChildren(a, 2)
 */
USER_OBJECT_
RS_XML_removeChildren(USER_OBJECT_ s_node, USER_OBJECT_ kids, USER_OBJECT_ freeNode)
{
    int i, n;
    USER_OBJECT_ ans;
    xmlNodePtr node = NULL, tmp;
    if(GET_LENGTH(s_node)) {
	node = (xmlNodePtr) R_ExternalPtrAddr(s_node);
    
	if(!node) {
	    PROBLEM "Empty XMLInternalNode"
		ERROR;
	}
    }
    
    n = GET_LENGTH(kids);
    PROTECT(ans = NEW_LOGICAL(n));
    for(i = 0; i < n; i++) {
	tmp = (xmlNodePtr)  R_ExternalPtrAddr(VECTOR_ELT(kids, i));
	if(!tmp)
	    continue;
	if(node && tmp->parent != node) {
	    PROBLEM "trying to remove a child node from a different parent node"
   	     ERROR;
	}

	xmlUnlinkNode(tmp);
	if(LOGICAL(freeNode)[0])
	    xmlFreeNode(tmp);
	LOGICAL(ans)[i]  = TRUE;
    }
    UNPROTECT(1);

    return(ans);
}

USER_OBJECT_
R_xmlRootNode(USER_OBJECT_ sdoc, USER_OBJECT_ skipDtd)
{
  xmlDocPtr doc = (xmlDocPtr) R_ExternalPtrAddr(sdoc);
  xmlNodePtr node = NULL;

  if(doc)
      node = doc->children;
  
  if(!node) {
      PROBLEM "empty XML document"
	  WARN;
      return(NULL_USER_OBJECT);
  }

  if(LOGICAL(skipDtd)[0]) {
      while(node && (node->type == XML_DTD_NODE || node->type == XML_COMMENT_NODE)) {
	  node = node->next;
      }
  }

  if(node == NULL) 
      return(NULL_USER_OBJECT);


  return(R_createXMLNodeRef(node));  
}


/**
 Create an S object representing a newly created internal 
 XML document object.

 */

int R_numXMLDocs = 0;
int R_numXMLDocsFreed = 0;

USER_OBJECT_
R_newXMLDoc(USER_OBJECT_ dtd, USER_OBJECT_ namespaces)
{
  xmlDocPtr doc;
  doc = xmlNewDoc(CHAR_TO_XMLCHAR("1.0"));
  R_numXMLDocs++;

  return(R_createXMLDocRef(doc));
}




USER_OBJECT_
R_newXMLDtd(USER_OBJECT_ sdoc, USER_OBJECT_ sdtdName, USER_OBJECT_ sexternalID, USER_OBJECT_ ssysID)
{

    xmlDocPtr doc = NULL;
    xmlChar *dtdName = NULL;
    xmlChar *externalID = NULL;
    xmlChar *sysID = NULL;
    xmlDtdPtr node;

#define  GET_STR_VAL(x)  \
    if(GET_LENGTH(s##x) > 0) { \
   	   x =  CHAR_TO_XMLCHAR(CHAR_DEREF(STRING_ELT(s##x, 0)));	\
           if(!x[0]) \
               x = NULL; \
    }

    GET_STR_VAL(dtdName)
    GET_STR_VAL(externalID)
    GET_STR_VAL(sysID)

    if(sdoc != NULL_USER_OBJECT && TYPEOF(sdoc) == EXTPTRSXP)
      doc = (xmlDocPtr) R_ExternalPtrAddr(sdoc);

    node = xmlNewDtd(doc, dtdName, externalID, sysID);


/* should we do this???
      xmlAddChild((xmlNodePtr) doc, (xmlNodePtr) DTD); 
*/
    return(R_createXMLNodeRef((xmlNodePtr) node));
}


/*
  The append section here is irrelevant!
 */
USER_OBJECT_
R_xmlSetNs(USER_OBJECT_ s_node, USER_OBJECT_ s_ns, USER_OBJECT_ append)
{
  xmlNodePtr node = (xmlNodePtr) R_ExternalPtrAddr(s_node);
  xmlNsPtr ns = NULL;
  if(s_ns != NULL_USER_OBJECT)
      ns = (xmlNsPtr) R_ExternalPtrAddr(s_ns);

  if(LOGICAL(append)[0]) {
      xmlNsPtr el;
      if(!node->ns)  
	  xmlSetNs(node, xmlNewNs(node, NULL, NULL));
      el = node->ns;
      while(el->next) 
	  el = el->next;
      el->next = ns;
  } else  
      xmlSetNs(node, ns);

  return(s_ns);
}


USER_OBJECT_
R_xmlNewNs(USER_OBJECT_ sdoc, USER_OBJECT_ shref, USER_OBJECT_ sprefix)
{
  xmlNodePtr doc = (xmlNodePtr) R_ExternalPtrAddr(sdoc);
  const char *href = CHAR_DEREF(STRING_ELT(shref, 0));
  const char *prefix = NULL;
  xmlNsPtr ns;

  if(Rf_length(sprefix)) {
      prefix = CHAR_DEREF(STRING_ELT(sprefix, 0));
      if(!prefix[0])
	  prefix = NULL;
  }

  if(!href[0])
      href = NULL;

  ns = xmlNewNs(doc, CHAR_TO_XMLCHAR(href), CHAR_TO_XMLCHAR(prefix));

  return(R_createXMLNsRef(ns)); /*XXX */
}


USER_OBJECT_
RS_XML_clone(USER_OBJECT_ obj, USER_OBJECT_ recursive)
{
    if(TYPEOF(obj) != EXTPTRSXP) {
	PROBLEM  "clone can only be applied to an internal, C-level libxml2 object"
        ERROR;
    }

    if(!R_ExternalPtrAddr(obj)) {
	PROBLEM  "NULL value passed to clone, possibly from a previous session"
        ERROR;
    }

    if(R_isInstanceOf(obj, "XMLInternalElementNode")) {
	xmlNodePtr node, node_ans;
	node = (xmlNodePtr) R_ExternalPtrAddr(obj);
	node_ans = xmlCopyNode(node, INTEGER(recursive)[0]);
	return(R_createXMLNodeRef(node_ans));
    } else if(R_isInstanceOf(obj, "XMLInternalDOM")) {
	xmlDocPtr doc;
	doc = (xmlDocPtr) R_ExternalPtrAddr(obj);
	return(R_createXMLDocRef(xmlCopyDoc(doc, INTEGER(recursive)[0])));
    }
    
    PROBLEM "clone doesn't (yet) understand this internal data type"
    ERROR;

    return(NULL_USER_OBJECT); /* never reached */
}

#ifdef R_XML_DEBUG
xmlDocPtr currentDoc;
#endif

USER_OBJECT_
R_createXMLDocRef(xmlDocPtr doc)
{
  SEXP ref, tmp;

#ifdef R_XML_DEBUG
  currentDoc = doc;
#endif


  PROTECT(ref = R_MakeExternalPtr(doc, Rf_install("XMLInternalDocument"), R_NilValue));
  PROTECT(tmp = NEW_CHARACTER(1));
  SET_STRING_ELT(tmp, 0, mkChar("XMLInternalDocument"));
  SET_CLASS(ref, tmp);
  UNPROTECT(2);
  return(ref);
}


USER_OBJECT_
R_createXMLNsRef(xmlNsPtr ns)
{
  SEXP ref, tmp;

  PROTECT(ref = R_MakeExternalPtr(ns, Rf_install("XMLNamespaceRef"), R_NilValue));
  PROTECT(tmp = NEW_CHARACTER(1));
  SET_STRING_ELT(tmp, 0, mkChar("XMLNamespaceRef"));
  SET_CLASS(ref, tmp);
  UNPROTECT(2);
  return(ref);
}


const char * 
R_getInternalNodeClass(xmlElementType type)
{
    const char * p = "";
    switch(type) {
        case XML_ELEMENT_NODE:
              p = "XMLInternalElementNode";
              break;
        case XML_TEXT_NODE:
              p = "XMLInternalTextNode";
              break;
        case XML_CDATA_SECTION_NODE:
              p = "XMLInternalCDataNode";
              break;
        case XML_ENTITY_NODE:
              p = "XMLInternalEntityNode";
              break;
        case XML_ENTITY_REF_NODE:
              p = "XMLInternalEntityRefNode";
              break;
        case XML_PI_NODE:
              p = "XMLInternalPINode";
              break;
        case XML_COMMENT_NODE:
              p = "XMLInternalCommentNode";
              break;
        case XML_NOTATION_NODE:
              p = "XMLInternalNotationNode";
              break;
        case XML_DTD_NODE:
              p = "XMLDTDNode";
              break;
        case XML_NAMESPACE_DECL:
              p = "XMLNamespaceDeclaration";
              break;
        case XML_XINCLUDE_START:
              p = "XMLXIncludeStartNode";
              break;
        case XML_XINCLUDE_END:
              p = "XMLXIncludeEndNode";
              break;
        case XML_ENTITY_DECL:
              p = "XMLInternalEntityRefNode";
              break;
        case XML_ATTRIBUTE_DECL:
              p = "XMLAttributeDeclNode";
              break;
        case XML_DOCUMENT_NODE:
              p = "XMLDocumentNode";
              break;
        case XML_DOCUMENT_TYPE_NODE:
              p = "XMLDocumentTypeNode";
              break;
        case XML_DOCUMENT_FRAG_NODE:
              p = "XMLDocumentFragNode";
              break;

        default:
              p = "XMLUnknownInternalNode";
    }

    return(p);
}

/**

 */
USER_OBJECT_
R_createXMLNodeRef(xmlNodePtr node)
{
  SEXP ref, tmp;

  if(!node)
      return(NULL_USER_OBJECT);


  PROTECT(ref = R_MakeExternalPtr(node, Rf_install("XMLInternalNode"), R_NilValue));
  PROTECT(tmp = NEW_CHARACTER(3));
  SET_STRING_ELT(tmp, 0, mkChar(R_getInternalNodeClass(node->type)));
  SET_STRING_ELT(tmp, 1, mkChar("XMLInternalNode"));
  SET_STRING_ELT(tmp, 2, mkChar("XMLAbstractNode"));
  SET_CLASS(ref, tmp);
  UNPROTECT(2);
  return(ref);
}




#define ValOrNULL(x) CHAR_TO_XMLCHAR ((x && x[0] ? x : NULL))


/**
 Write the XML tree/DOM to a file or into a buffer (depending on the value
 of sfileName)

 It would be nice to use connections, but this is not yet possible
 in full generality.  Later

 @sdoc: the S object that is a reference to the top-level XML DOM.
 @sfileName: the S object that gives the name of the file to which the
  DOM should be written or alternatively, the S value `NULL' indicating
  that the DOM should be dumped to a buffer and returned as an S string.
 @compression: if @sfileName is the name of a file and we are not
  returning the DOM as a string, then we set the compression level
  to the value of this integer, unless it is omitted and specified as 
  the S value `NULL'.
 */
USER_OBJECT_
R_saveXMLDOM(USER_OBJECT_ sdoc, USER_OBJECT_ sfileName, USER_OBJECT_ compression, USER_OBJECT_ sindent,
	     USER_OBJECT_ prefix, USER_OBJECT_ r_encoding)
{
    xmlDocPtr doc = (xmlDocPtr) R_ExternalPtrAddr(sdoc);
    const char *fileName = NULL;
    USER_OBJECT_ ans = NULL_USER_OBJECT;
    xmlDtdPtr dtd = NULL;

    int oldIndent = xmlIndentTreeOutput;
    const char *encoding = CHAR_DEREF(STRING_ELT(r_encoding, 0));

    xmlIndentTreeOutput = LOGICAL_DATA(sindent)[0];

    if(GET_LENGTH(prefix) == 3) {
	dtd = xmlNewDtd(doc, ValOrNULL(CHAR_DEREF(STRING_ELT(prefix, 0))), 
                             ValOrNULL(CHAR_DEREF(STRING_ELT(prefix, 1))), 
     	                     ValOrNULL(CHAR_DEREF(STRING_ELT(prefix, 2))));
	dtd->parent = doc;
	dtd->doc = doc;

	dtd->prev = doc->children->prev;
	dtd->next = doc->children;
	doc->children->prev = (xmlNodePtr) dtd;

	doc->children = (xmlNodePtr) dtd;
    }


    /* Figure out what the name of the file is, or if it is NULL. */
    if(GET_LENGTH(sfileName))
      fileName = CHAR_DEREF(STRING_ELT(sfileName, 0));

    /* If the user specified a file name, write to it and honor 
       the compression setting they supplied. 
     */
    if(fileName && fileName[0]) {
        int compressionLevel = -1;
        if(GET_LENGTH(compression)) {
	    compressionLevel = xmlGetDocCompressMode(doc);
	    xmlSetDocCompressMode(doc, INTEGER_DATA(compression)[0]);
	}
	if(encoding && encoding[0])
	    xmlSaveFileEnc(CHAR_DEREF(STRING_ELT(sfileName, 0)),  doc, encoding);
#if 0
	else
	    xmlSaveFile(CHAR_DEREF(STRING_ELT(sfileName, 0)),  doc);
#else
	else {
	  FILE *f;
	  f = fopen(CHAR_DEREF(STRING_ELT(sfileName, 0)), "w");
	  xmlDocFormatDump(f, doc, 1);
	  fclose(f);
	}
#endif
        if(compressionLevel != -1) {
	    xmlSetDocCompressMode(doc, compressionLevel);
	}
    } else {
	/* So we are writing to a buffer and returning the DOM as an S string. */
        xmlChar *mem;
        int size;
/*??? Do we need to allocate this memory? */
        PROTECT(ans = NEW_CHARACTER(1));
	if(encoding && encoding[0])
	    xmlDocDumpFormatMemoryEnc(doc, &mem, &size, encoding, LOGICAL_DATA(sindent)[0]);
	else {
	    xmlDocDumpFormatMemory(doc, &mem, &size, 1); 
	    /* xmlDocDumpMemory(doc, &mem, &size);  original */
	}


	if(dtd) {
	    xmlNodePtr tmp;
	    doc->extSubset  = NULL;
            tmp = doc->children->next;
	    tmp->prev = NULL;
            doc->children = tmp;
	    xmlFreeDtd(dtd);
	}

	if(mem) {
	    DECL_ENCODING_FROM_DOC(doc)
	    SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(XMLCHAR_TO_CHAR(mem))); 
	    xmlFree(mem);
	} else { 
               /*XXX get the error message from libxml2 */
	    PROBLEM "failed to write XML document contents"
		ERROR;
	}
        UNPROTECT(1);

       return(ans);
    }

    xmlIndentTreeOutput = oldIndent;
    return(ans);
}


USER_OBJECT_
RS_XML_setDoc(USER_OBJECT_ snode, USER_OBJECT_ sdoc)
{
/*Might use xmlCopyNode or xmlCopyNodeList if we have to make a copy*/
    xmlDocPtr doc;
    xmlNodePtr node = (xmlNodePtr) R_ExternalPtrAddr(snode);

    if(sdoc != NULL_USER_OBJECT) {
       doc = (xmlDocPtr) R_ExternalPtrAddr(sdoc);
    } else {
	doc = xmlNewDoc(CHAR_TO_XMLCHAR("1.0"));
	R_numXMLDocs++;
    }
 
    xmlDocSetRootElement(doc, node);
    return(R_createXMLDocRef(doc));
}

USER_OBJECT_
RS_XML_unsetDoc(USER_OBJECT_ snode, USER_OBJECT_ unlink, USER_OBJECT_ r_parent)
{
    xmlNodePtr node = (xmlNodePtr) R_ExternalPtrAddr(snode);
    if(!node) {
	return(NULL_USER_OBJECT);
    }

    if(node->doc && node->doc->children == node) {
	xmlDocSetRootElement(node->doc, NULL);
    }

    if(LOGICAL(unlink)[0]) 
	xmlUnlinkNode(node);


    node->doc = NULL;
    node->parent = NULL;

    if(r_parent != R_NilValue) {
        node->parent = (xmlNodePtr) R_ExternalPtrAddr(snode);
    }

    return(ScalarLogical(TRUE));
}

SEXP
RS_XML_setDocEl(SEXP r_node, SEXP r_doc)
{
    xmlNodePtr node = (xmlNodePtr) R_ExternalPtrAddr(r_node);
    xmlDocPtr doc = (xmlDocPtr) R_ExternalPtrAddr(r_doc);
    xmlSetTreeDoc(node, doc);
/*    node->doc = doc; */
    return(R_NilValue);
}



#ifdef ADD_XML_OUTPUT_BUFFER_CODE

/* These two taken from libxml2-2.6.27 
   They are needed if xmlOutputBufferCreateBuffer()
   is not in the installed libxml2.
   It appeared in libxml2-2.6.23, released on Jan 5 2006
*/
static int
xmlBufferWrite (void * context, const char * buffer, int len) {
    int ret;

    ret = xmlBufferAdd((xmlBufferPtr) context, (const xmlChar *) buffer, len);
    if (ret != 0)
        return(-1);
    return(len);
}

xmlOutputBufferPtr
xmlOutputBufferCreateBuffer(xmlBufferPtr buffer,
                            xmlCharEncodingHandlerPtr encoder) {
    xmlOutputBufferPtr ret;

    if (buffer == NULL) return(NULL);

    ret = xmlOutputBufferCreateIO((xmlOutputWriteCallback)
                                  xmlBufferWrite,
                                  (xmlOutputCloseCallback)
                                  NULL, (void *) buffer, encoder);

    return(ret);
}

#endif


/* Not completed.
   This could put the node into a new document and then call R_saveXMLDOM()
   but we are doing it in separate steps with separate C routines and 
   calling these from R.

    xmlNodeDumpOutput

Test:
  a = newXMLNode("a", "first bit", newXMLNode("b", "contents of b", newXMLNode("c", 3)), "more text")
  a = newXMLNode("a", newXMLNode("b", newXMLNode("c", 3))) 
  .Call("RS_XML_printXMLNode", a, as.integer(1), as.integer(1), character())
*/
USER_OBJECT_
RS_XML_printXMLNode(USER_OBJECT_ r_node, USER_OBJECT_ level, USER_OBJECT_ format, USER_OBJECT_ indent, USER_OBJECT_ r_encoding)
{
    USER_OBJECT_ ans;
    xmlNodePtr node;
    const char *encoding = NULL;
    xmlOutputBufferPtr buf;
    xmlBufferPtr xbuf;

    int oldIndent;

    oldIndent = xmlIndentTreeOutput;

    node = (xmlNodePtr) R_ExternalPtrAddr(r_node);

    xmlIndentTreeOutput =  LOGICAL(indent)[0];

    xbuf = xmlBufferCreate();
   
    if(GET_LENGTH(r_encoding))
	encoding = CHAR_DEREF(STRING_ELT(r_encoding, 0));

    buf = xmlOutputBufferCreateBuffer(xbuf, NULL);
//    xmlKeepBlanksDefault(0);

    xmlNodeDumpOutput(buf,  node->doc, node, INTEGER(level)[0], INTEGER(format)[0], encoding);
    xmlOutputBufferFlush(buf);
    xmlIndentTreeOutput = oldIndent;

    if(xbuf->use > 0) {
        /*XXX this const char * in CHARSXP means we have to make multiple copies. */
#if 0
	char *rbuf = R_alloc(sizeof(char) * (xbuf->use + 1));
	memcpy(rbuf, xbuf->content, xbuf->use + 1);
	PROTECT(tmp = mkChar(rbuf));
#endif
	ans = ScalarString(mkChar(xbuf->content));
    } else
      ans = NEW_CHARACTER(1);

    xmlOutputBufferClose(buf);

    return(ans);
}


SEXP
R_xmlNodeValue(SEXP node, SEXP raw)
{
   xmlNodePtr n = (xmlNodePtr) R_ExternalPtrAddr(node);
   xmlChar *tmp;
   SEXP ans;
   DECL_ENCODING_FROM_NODE(n)

   if(!n) {
      PROBLEM  "null value for xml node reference"
      ERROR;
   }

   tmp  = xmlNodeGetContent(n);
/*
  xmlGetNodeRawString
  xmlGetNodeString

   if(GET_LENGTH(raw) == 0)
   else if(LOGICAL(raw)[0]) {
   } else {

   }
*/
   if(tmp) {
     ans = ScalarString(CreateCharSexpWithEncoding(encoding, tmp));
//     ans = mkString(XMLCHAR_TO_CHAR(tmp));
     // Just playing:  ans = ScalarString(mkCharCE(tmp, CE_UTF8));
   } else 
       ans = NEW_CHARACTER(0);

   return(ans);
}

USER_OBJECT_
R_xmlNsAsCharacter(USER_OBJECT_ s_ns)
{
  xmlNsPtr ns = NULL;
  USER_OBJECT_ ans, names;
  const xmlChar *encoding = NULL;
  ns = (xmlNsPtr) R_ExternalPtrAddr(s_ns);  
#ifdef LIBXML_NAMESPACE_HAS_CONTEXT
  encoding = ns->context ? ns->context->encoding : NULL;
#endif

  PROTECT(ans = NEW_CHARACTER(2));
  PROTECT(names = NEW_CHARACTER(2));

  SET_STRING_ELT(names, 0, mkChar("prefix"));
  SET_STRING_ELT(names, 1, mkChar("href"));

  if(ns->prefix)
      SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(XMLCHAR_TO_CHAR(ns->prefix)));
  if(ns->href)
      SET_STRING_ELT(ans, 1, COPY_TO_USER_STRING(XMLCHAR_TO_CHAR(ns->href)));

  SET_NAMES(ans, names);
  UNPROTECT(2);
  return(ans);
}

USER_OBJECT_
R_getXMLNodeDocument(USER_OBJECT_ s_node)
{
    xmlNodePtr n = (xmlNodePtr) R_ExternalPtrAddr(s_node);
    if(!n->doc)
	return(NULL_USER_OBJECT);

       /*??? Does this arrange to free it? */
    return(R_createXMLDocRef(n->doc));
}


USER_OBJECT_
RS_XML_isDescendantOf(USER_OBJECT_ r_node, USER_OBJECT_ r_top, USER_OBJECT_ strict)
{
    xmlNodePtr node, ptr, top;
    node = (xmlNodePtr) R_ExternalPtrAddr(r_node);    
    top = (xmlNodePtr) R_ExternalPtrAddr(r_top);    

    if(!node || !top) {
	PROBLEM  "null value passed to RS_XML_isDescendantOf"
	    ERROR;
    }

    ptr = node;

    while(ptr && ptr->type != XML_DOCUMENT_NODE) {
	if(ptr == top)
	    return(ScalarLogical(ptr == node && LOGICAL(strict)[0] ? FALSE : TRUE));
	ptr = ptr->parent;
    }
    
    return(ScalarLogical(FALSE));
}
