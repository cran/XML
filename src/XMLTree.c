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


#include "Utils.h"  /* R_createXMLNodeRef() */


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
R_xmlNewComment(USER_OBJECT_ str)
{
    xmlNodePtr node;
    node = xmlNewComment(CHAR_TO_XMLCHAR(CHAR_DEREF(STRING_ELT(str, 0))));

    return(R_createXMLNodeRef(node));
}

USER_OBJECT_
R_newXMLTextNode(USER_OBJECT_ value)
{
   xmlNodePtr node;
   node = xmlNewText(CHAR_TO_XMLCHAR(CHAR_DEREF(STRING_ELT(value, 0))));
   return(R_createXMLNodeRef(node));
}

USER_OBJECT_
R_newXMLCDataNode(USER_OBJECT_ sdoc, USER_OBJECT_ value)
{
  xmlDocPtr  doc = NULL;
  xmlNodePtr node;
  char *tmp;

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
R_newXMLNode(USER_OBJECT_ name, USER_OBJECT_ attrs, USER_OBJECT_ nameSpace, USER_OBJECT_ sdoc)
{
   xmlDocPtr doc = NULL;
   xmlNsPtr ns = NULL;
   xmlNodePtr node;
   int n;

   if(GET_LENGTH(sdoc))
       doc = (xmlDocPtr) R_ExternalPtrAddr(sdoc);

   if(GET_LENGTH(nameSpace) > 0) {
      CHAR_DEREF(STRING_ELT(nameSpace, 0));
   }

/*   ns = xmlNewNs(xmlGetRootElement(doc), ); */
   node = xmlNewDocNode(doc, ns, CHAR_TO_XMLCHAR(CHAR_DEREF(STRING_ELT(name, 0))), NULL);

   n = GET_LENGTH(attrs);
   if(n > 0) {
       int i;
       USER_OBJECT_ attrNames = GET_NAMES(attrs);
       for(i = 0; i < n ; i++) {
	   xmlSetProp(node, CHAR_TO_XMLCHAR(CHAR_DEREF(STRING_ELT(attrNames, i))),
                            CHAR_TO_XMLCHAR(CHAR_DEREF(STRING_ELT(attrs, i))));
       }
   }

   if(doc && XML_ROOT(doc) == NULL) {
       XML_ROOT(doc) = node;
   }

   return(R_createXMLNodeRef(node));
}

/**
 Add the internal XML node represented by the S object @node
 as a child of the XML node represented by the S object @parent.


 */
USER_OBJECT_
R_insertXMLNode(USER_OBJECT_ node, USER_OBJECT_ parent)
{
    xmlNodePtr n, p;

    if(IS_LIST(node))  {
      int i;
      for(i = 0; i < GET_LENGTH(node); i++)
         R_insertXMLNode(VECTOR_ELT(node, i), parent);

      return(NULL_USER_OBJECT);
    }

    if(TYPEOF(node) != EXTPTRSXP || TYPEOF(parent) != EXTPTRSXP) {
       PROBLEM "R_insertXMLNode expects InternalXMLNode objects"
       ERROR;
    }

    p = (xmlNodePtr) R_ExternalPtrAddr(parent);
    n = (xmlNodePtr) R_ExternalPtrAddr(node);

    xmlAddChild(p, n);

    return(NULL_USER_OBJECT);     
}


USER_OBJECT_
R_xmlRootNode(USER_OBJECT_ sdoc)
{
  xmlDocPtr doc = (xmlDocPtr) R_ExternalPtrAddr(sdoc);
  
  return(R_createXMLNodeRef(doc->children));  
}


/**
 Create an S object representing a newly created internal 
 XML document object.

 */
USER_OBJECT_
R_newXMLDoc(USER_OBJECT_ dtd, USER_OBJECT_ namespaces)
{
  xmlDocPtr doc;
  doc = xmlNewDoc(CHAR_TO_XMLCHAR("1.0"));

  return(R_createXMLDocRef(doc));
}


USER_OBJECT_
R_newXMLDtd(USER_OBJECT_ sdoc, USER_OBJECT_ sname, USER_OBJECT_ sexternalID, USER_OBJECT_ ssysID)
{

  xmlDocPtr doc = (xmlDocPtr) R_ExternalPtrAddr(sdoc);
  char *dtdName = CHAR_DEREF(STRING_ELT(sname, 0));
  char *externalID = CHAR_DEREF(STRING_ELT(sexternalID, 0));
  char *sysID = CHAR_DEREF(STRING_ELT(ssysID, 0));
  xmlDtdPtr node;

  node = xmlNewDtd(doc, CHAR_TO_XMLCHAR( (externalID[0] ? externalID : NULL)), 
                        CHAR_TO_XMLCHAR((sysID[0] ? sysID : NULL)), 
                        CHAR_TO_XMLCHAR(dtdName));
/*     xmlAddChild((xmlNodePtr) doc, (xmlNodePtr) DTD); */
  return(R_createXMLNodeRef((xmlNodePtr) node));
}


USER_OBJECT_
R_xmlSetNs(USER_OBJECT_ s_node, USER_OBJECT_ s_ns)
{
  xmlNodePtr node = (xmlNodePtr) R_ExternalPtrAddr(s_node);
  xmlNsPtr ns = (xmlNsPtr) R_ExternalPtrAddr(s_ns);

   xmlSetNs(node, ns);

  return(s_ns);
}


USER_OBJECT_
R_xmlNewNs(USER_OBJECT_ sdoc, USER_OBJECT_ shref, USER_OBJECT_ sprefix)
{
  xmlNodePtr doc = (xmlNodePtr) R_ExternalPtrAddr(sdoc);
  char *href = CHAR_DEREF(STRING_ELT(shref, 0));
  char *prefix = CHAR_DEREF(STRING_ELT(sprefix, 0));
  xmlNsPtr ns;

  ns = xmlNewNs(doc, CHAR_TO_XMLCHAR(href), CHAR_TO_XMLCHAR(prefix));

  return(R_createXMLNodeRef((xmlNodePtr) ns));
}


USER_OBJECT_
R_createXMLDocRef(xmlDocPtr doc)
{
  SEXP ref, tmp;

  PROTECT(ref = R_MakeExternalPtr(doc, Rf_install("XMLInternalDocument"), R_NilValue));
  PROTECT(tmp = NEW_CHARACTER(1));
  SET_STRING_ELT(tmp, 0, COPY_TO_USER_STRING("XMLInternalDocument"));
  SET_CLASS(ref, tmp);
  UNPROTECT(2);
  return(ref);
}

/**

 */
USER_OBJECT_
R_createXMLNodeRef(xmlNodePtr node)
{
  SEXP ref, tmp;

  PROTECT(ref = R_MakeExternalPtr(node, Rf_install("XMLInternalNode"), R_NilValue));
  PROTECT(tmp = NEW_CHARACTER(1));
  SET_STRING_ELT(tmp, 0, COPY_TO_USER_STRING("XMLInternalNode"));
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
              USER_OBJECT_ prefix)
{
    xmlDocPtr doc = (xmlDocPtr) R_ExternalPtrAddr(sdoc);
    char *fileName = NULL;
    USER_OBJECT_ ans = NULL_USER_OBJECT;
    xmlDtdPtr dtd = NULL;

    int oldIndent = xmlIndentTreeOutput;

    xmlIndentTreeOutput = LOGICAL_DATA(sindent)[0];

    if(GET_LENGTH(prefix) == 3) {
	fprintf(stderr, "setting DTD\n");fflush(stderr);
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
	xmlSaveFile(CHAR_DEREF(STRING_ELT(sfileName, 0)),  doc);
        if(compressionLevel != -1) {
	    xmlSetDocCompressMode(doc, compressionLevel);
	}
    } else {
	/* So we are writing to a buffer and returning the DOM as an S string. */
        xmlChar *mem;
        int size;
        PROTECT(ans = NEW_CHARACTER(1));
#if DUMP_WITH_ENCODING
	xmlDocDumpFormatMemoryEnc(doc, &mem, &size, NULL, LOGICAL_DATA(sindent)[0]);
#else
	xmlDocDumpMemory(doc, &mem, &size); 
#endif

	if(dtd) {
	    xmlNodePtr tmp;
	    doc->extSubset  = NULL;
            tmp = doc->children->next;
	    tmp->prev = NULL;
            doc->children = tmp;
	    xmlFreeDtd(dtd);
	}

        SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(XMLCHAR_TO_CHAR(mem))); 
        xmlFree(mem);
        UNPROTECT(1);

       return(ans);
    }

    xmlIndentTreeOutput = oldIndent;
    return(ans);
}
