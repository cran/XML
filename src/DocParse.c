/**
  Routines for parsing and processing an XML document
  into an R/S data structure.


 * See Copyright for the license status of this software.

 */

#include "DocParse.h"
#include "Utils.h"  /* For isBlank() */


                    /* For the call to stat. */
#include <sys/stat.h>
#include <unistd.h>  

#include "RSDTD.h"

#include <stdarg.h>


#include <libxml/xmlschemas.h>

int RS_XML(setNodeClass)(xmlNodePtr node, USER_OBJECT_ ans);
USER_OBJECT_ RS_XML(notifyNamespaceDefinition)(USER_OBJECT_ ns, R_XMLSettings *parserSettings);


void RS_XML(ValidationWarning)(void *ctx, const char *msg, ...);
void RS_XML(ValidationError)(void *ctx, const char *msg, ...);


static USER_OBJECT_ convertNode(USER_OBJECT_ ans, xmlNodePtr node, R_XMLSettings *parserSettings);
static void NodeTraverse(xmlNodePtr doc, USER_OBJECT_ converterFunctions, R_XMLSettings *parserSettings);


static USER_OBJECT_ makeSchemaReference(xmlSchemaPtr ref);


USER_OBJECT_
RS_XML(getDefaultValiditySetting)(USER_OBJECT_ val)
{
#ifdef HAVE_VALIDITY

 extern int xmlDoValidityCheckingDefaultValue;
 USER_OBJECT_ ans;
 ans = NEW_INTEGER(1);
 INTEGER_DATA(ans)[0] = xmlDoValidityCheckingDefaultValue;

  if(GET_LENGTH(val))
     xmlDoValidityCheckingDefaultValue = INTEGER_DATA(val)[0];
  return(ans);

#else

  return(NEW_INTEGER(0));

#endif
}


/**
  Entry point for reading, parsing and converting an XML tree
  to an R object.

  fileName is the string identifying the file, and is
  expanded using the normal rules for an R file name.
  That is, it can contain environment variables, ~, etc. 
 
  converterFunctions is a collection of functions used to 
  map a node into an R object. This would normally
  be a closure. It is not currently used, but will be enabled in
  the future.

  skipBlankLines controls whether text elements consisting
  simply of white space are included in the resulting
  structure.


  The return value is a simple list with named elements
     file, version and children
  The children element is itself a list consisting of 
  objects of class `XMLNode'. Each of these has the characteristic
 
 */
USER_OBJECT_
RS_XML(ParseTree)(USER_OBJECT_ fileName, USER_OBJECT_ converterFunctions, 
                    USER_OBJECT_ skipBlankLines, USER_OBJECT_ replaceEntities,
                     USER_OBJECT_ asText, USER_OBJECT_ trim, USER_OBJECT_ validate,
                      USER_OBJECT_ getDTD, USER_OBJECT_ isURL,
                       USER_OBJECT_ addNamespaceAttributes,
                        USER_OBJECT_ internalNodeReferences, 
   	     	         USER_OBJECT_ s_useHTML, USER_OBJECT_ isSchema)
{

  char *name;
  xmlDocPtr doc;
  USER_OBJECT_ rdoc;
  USER_OBJECT_ className;
  R_XMLSettings parserSettings;
  int previousValiditySetting;

  int asTextBuffer = LOGICAL_DATA(asText)[0];
  int isURLDoc = LOGICAL_DATA(isURL)[0];
  int useHTML = LOGICAL_DATA(s_useHTML)[0];

  parserSettings.skipBlankLines = LOGICAL_DATA(skipBlankLines)[0];
  parserSettings.converters = converterFunctions;
  parserSettings.trim = LOGICAL_DATA(trim)[0];

  parserSettings.internalNodeReferences = LOGICAL_DATA(internalNodeReferences)[0];

  parserSettings.addAttributeNamespaces = LOGICAL_DATA(addNamespaceAttributes)[0];





  if(asTextBuffer == 0) {
    struct stat tmp_stat;  
#ifdef USE_R
    name = R_ExpandFileName(CHAR(STRING_ELT(fileName, 0)));
#else
   name = CHARACTER_DATA(fileName)[0];
#endif
    if(!isURLDoc && (name == NULL || stat(name, &tmp_stat) < 0)) {
      PROBLEM "Can't find file %s", CHAR_DEREF(STRING_ELT(fileName, 0))
      ERROR;
    }
  } else {
/* XXX take care of freeing this. */
    name = strdup(CHAR_DEREF(STRING_ELT(fileName, 0)));
  }

    /* If one wants entities expanded directly and to appear as text.  */
  if(LOGICAL_DATA(replaceEntities)[0]) 
      xmlSubstituteEntitiesDefault(1);   


  if(LOGICAL_DATA(isSchema)[0]) {
      xmlSchemaPtr schema = NULL;
      xmlSchemaParserCtxtPtr ctxt;

      ctxt = xmlSchemaNewParserCtxt(name);
      schema = xmlSchemaParse(ctxt);

/*XXX make certain to cleanup the settings. */
      return(makeSchemaReference(schema));

  }

  if(asTextBuffer) {
   doc = useHTML ? htmlParseDoc(CHAR_TO_XMLCHAR(name), NULL) : xmlParseMemory(name, strlen(name));
   if(doc != NULL) {
      doc->name = (char *) xmlStrdup(CHAR_TO_XMLCHAR("<buffer>"));
   }
  } else {
   doc = useHTML ? htmlParseFile(XMLCHAR_TO_CHAR(name), NULL) : xmlParseFile(name);
  }

  if(doc == NULL) {
    PROBLEM "error in creating parser for %s", name
    ERROR;
  }

  if(!useHTML && LOGICAL_DATA(validate)[0]) {
      xmlValidCtxt ctxt;
      ctxt.error = RS_XML(ValidationError);
      ctxt.warning = RS_XML(ValidationWarning);

      if(!xmlValidateDocument(&ctxt, doc)) {
	  PROBLEM "XML document is invalid"
          ERROR;
      }
  }

  if(parserSettings.internalNodeReferences) {
      /* Use a different approach - pass internal nodes to the converter functions*/
      xmlNodePtr root;
#ifdef USE_OLD_ROOT_CHILD_NAMES
      root = doc->root;
#else
      root = doc->xmlRootNode;
#ifdef ROOT_HAS_DTD_NODE
    if(root->next && root->children == NULL)
       root = root->next;
#endif
#endif   
      NodeTraverse(root, converterFunctions, &parserSettings);
      PROTECT(rdoc = NULL_USER_OBJECT);
  } else {
      PROTECT(rdoc = RS_XML(convertXMLDoc)(name, doc, converterFunctions, &parserSettings));
  }

  if(asTextBuffer && name)
    free(name);


  if(!useHTML && !parserSettings.internalNodeReferences && LOGICAL_DATA(getDTD)[0]) {
    USER_OBJECT_ ans, klass, tmp;
    const char *names[] = {"doc", "dtd"};
      PROTECT(ans = NEW_LIST(2));
        SET_VECTOR_ELT(ans, 0, rdoc);
        SET_VECTOR_ELT(ans, 1, tmp = RS_XML(ConstructDTDList)(doc, 1, NULL));

        PROTECT(klass = NEW_CHARACTER(1));
        SET_STRING_ELT( klass, 0, COPY_TO_USER_STRING("DTDList"));
        SET_CLASS(tmp, klass);

        RS_XML(SetNames)(sizeof(names)/sizeof(names[0]), names, ans);

      UNPROTECT(2); /* release the ans */
      rdoc = ans;
  }

  if(parserSettings.internalNodeReferences && GET_LENGTH(converterFunctions) < 1) {
     UNPROTECT(1);
     return(R_createXMLDocRef(doc));
  }

  xmlFreeDoc(doc);

  if(!parserSettings.internalNodeReferences) {
     /* Set the class for the document. */
    className = NEW_CHARACTER(1);
    PROTECT(className);
      SET_STRING_ELT(className, 0, COPY_TO_USER_STRING(useHTML ? "HTMLDocument" : "XMLDocument"));   
      SET_CLASS(rdoc, className);
    UNPROTECT(1);
  }


 UNPROTECT(1); 
 return(rdoc);
}

enum { FILE_ELEMENT_NAME, VERSION_ELEMENT_NAME, CHILDREN_ELEMENT_NAME, NUM_DOC_ELEMENTS};



void
NodeTraverse(xmlNodePtr root, USER_OBJECT_ converterFunctions, R_XMLSettings *parserSettings)
{
  xmlNodePtr c, tmp;
    c = root;

    while(c) {
	USER_OBJECT_ ref;
#ifndef USE_OLD_ROOT_CHILD_NAMES
         tmp = c->xmlChildrenNode;
#else
               c->childs;
#endif
        if(tmp)
	    NodeTraverse(tmp, converterFunctions, parserSettings);
	PROTECT(ref = R_createXMLNodeRef(c));
	convertNode(ref, c, parserSettings);
	UNPROTECT(1);
	c = c->next;
    }
}





/**
   Returns a named list whose elements are
    file:  the name of the file being processed.
    version: the XML version.
    root: the collection of children.
 */
USER_OBJECT_
RS_XML(convertXMLDoc)(char *fileName, xmlDocPtr doc, USER_OBJECT_ converterFunctions, 
                    R_XMLSettings *parserSettings)
{
  USER_OBJECT_ rdoc;
  USER_OBJECT_ rdoc_el_names, klass;
  int n = NUM_DOC_ELEMENTS;
  const char *version = "";

  PROTECT(rdoc = NEW_LIST(n));
  PROTECT(rdoc_el_names = NEW_CHARACTER(n));
 
    /* Insert the name of the file being processed */
  SET_VECTOR_ELT(rdoc, FILE_ELEMENT_NAME, NEW_CHARACTER(1));
    SET_STRING_ELT(VECTOR_ELT(rdoc, FILE_ELEMENT_NAME), 0, COPY_TO_USER_STRING(doc->name ? doc->name : fileName));
    SET_STRING_ELT(rdoc_el_names, FILE_ELEMENT_NAME, COPY_TO_USER_STRING("file"));

    /* Insert the XML version information */
  SET_VECTOR_ELT(rdoc, VERSION_ELEMENT_NAME, NEW_CHARACTER(1));
  if(doc->version)
	version = XMLCHAR_TO_CHAR(doc->version);

  SET_STRING_ELT(VECTOR_ELT(rdoc, VERSION_ELEMENT_NAME), 0, 
                                     COPY_TO_USER_STRING(version));
  SET_STRING_ELT(rdoc_el_names, VERSION_ELEMENT_NAME, COPY_TO_USER_STRING("version"));

    /* Compute the nodes for this tree, recursively. 
       Note the SIDEWAYS argument to get the sibling nodes
       at the root, rather than just the first and its children.
     */
{
  xmlNodePtr root;
#ifdef USE_OLD_ROOT_CHILD_NAMES
    root = doc->root;
#else
    root = doc->xmlRootNode;

#ifdef ROOT_HAS_DTD_NODE
    if(root->next && root->children == NULL)
       root = root->next;
#endif
#endif
  SET_VECTOR_ELT(rdoc, CHILDREN_ELEMENT_NAME, RS_XML(createNodeChildren)(root,  SIDEWAYS, parserSettings)); 
}
  SET_STRING_ELT(rdoc_el_names, CHILDREN_ELEMENT_NAME, COPY_TO_USER_STRING("children"));

  SET_NAMES(rdoc, rdoc_el_names);

  PROTECT(klass = NEW_CHARACTER(1));
  SET_STRING_ELT(klass, 0, COPY_TO_USER_STRING("XMLDocumentContent"));
  SET_CLASS(rdoc, klass);

  UNPROTECT(3);

  return(rdoc);
}

USER_OBJECT_
processNamespaceDefinitions(xmlNs *ns, xmlNodePtr node, R_XMLSettings *parserSettings)
{
  int n = 0;
  xmlNs *ptr = ns;
  USER_OBJECT_ ans, tmp, names;

  while(ptr) {
    ptr = ptr->next;
    n++;
  }
  PROTECT(ans = NEW_LIST(n));
  PROTECT(names = NEW_CHARACTER(n));

  for(n = 0, ptr = ns; ptr ; n++, ptr = ptr->next) {
    tmp = RS_XML(createNameSpaceIdentifier)(ptr,node);
    (void) RS_XML(notifyNamespaceDefinition)(tmp, parserSettings);
    SET_VECTOR_ELT(ans, n, tmp);
    if(ptr->prefix)
       SET_STRING_ELT(names, n, COPY_TO_USER_STRING(XMLCHAR_TO_CHAR(ptr->prefix)));
  }

  SET_NAMES(ans, names);
  UNPROTECT(2);
  return(ans);
}

/**
   Creates an R object representing the specified node, and its children
   if recursive is non-zero. Certain types of nodes have 

   direction controls whether we take the siblings of this node
   or alternatively its children.

   parentUserNode the previously created user-leve node for the parent of the
            target node. 

 */

enum { NODE_NAME, NODE_ATTRIBUTES, NODE_CHILDREN, NODE_NAMESPACE, NODE_NAMESPACE_DEFS, NUM_NODE_ELEMENTS};

USER_OBJECT_
RS_XML(createXMLNode)(xmlNodePtr node, int recursive, int direction, R_XMLSettings *parserSettings, USER_OBJECT_ parentUserNode)
{
  int n = NUM_NODE_ELEMENTS;
  USER_OBJECT_ ans;
  USER_OBJECT_ ans_el_names;
  USER_OBJECT_ nsDef = NULL_USER_OBJECT;
  int addValue;

  char *contentValue = XMLCHAR_TO_CHAR(node->content);

#ifdef ROOT_HAS_DTD_NODE
  if(node->type == XML_DTD_NODE)
    return(NULL);
#endif

  if(parserSettings->trim) {
    contentValue = trim(XMLCHAR_TO_CHAR(node->content));
  }

  addValue = (contentValue && strlen(contentValue) && isBlank(contentValue) == 0);

#ifdef LIBXML2
  if(node->type == XML_ENTITY_DECL)
    return(NULL);
#endif
 
        /* Drop text nodes that are blank, if that is what the user wanted. */
  if(parserSettings->skipBlankLines && addValue == 0 && node->type == XML_TEXT_NODE)
    return(NULL);

  
  if(addValue)
    n++;


     /* Create the default return value being a list of name, attributes, children 
        and possibly value. 


      */
 PROTECT(ans = NEW_LIST(n));
 PROTECT(ans_el_names = NEW_CHARACTER(n));

   /* If there are namespace definitions within this node, */
  if(node->nsDef)  {
    nsDef = processNamespaceDefinitions(node->nsDef, node, parserSettings);
    SET_VECTOR_ELT(ans, NODE_NAMESPACE_DEFS, nsDef);
  }


  SET_VECTOR_ELT(ans, NODE_NAME, NEW_CHARACTER(1));
  if(node->name)
    SET_STRING_ELT(VECTOR_ELT(ans, NODE_NAME), 0, COPY_TO_USER_STRING(XMLCHAR_TO_CHAR(node->name)));

  SET_VECTOR_ELT(ans, NODE_ATTRIBUTES, RS_XML(AttributeList)(node, parserSettings));

  if(recursive)
    SET_VECTOR_ELT(ans, NODE_CHILDREN, RS_XML(createNodeChildren)(node, direction, parserSettings));
  else
    SET_VECTOR_ELT(ans, NODE_CHILDREN, NULL_USER_OBJECT); 



  SET_STRING_ELT(ans_el_names, NODE_NAME, COPY_TO_USER_STRING("name"));
  SET_STRING_ELT(ans_el_names, NODE_ATTRIBUTES,  COPY_TO_USER_STRING("attributes"));
  SET_STRING_ELT(ans_el_names, NODE_CHILDREN, COPY_TO_USER_STRING("children"));
  SET_STRING_ELT(ans_el_names, NODE_NAMESPACE, COPY_TO_USER_STRING("namespace"));
  SET_STRING_ELT(ans_el_names, NODE_NAMESPACE_DEFS, COPY_TO_USER_STRING("namespaceDefinitions"));

  if(node->ns) {
    PROTECT(nsDef = NEW_CHARACTER(1));
    if(node->ns->prefix)
       SET_STRING_ELT(nsDef, 0, COPY_TO_USER_STRING(XMLCHAR_TO_CHAR(node->ns->prefix)));
    SET_VECTOR_ELT(ans, NODE_NAMESPACE, nsDef);
    UNPROTECT(1);
  }



  if(addValue) {
    SET_STRING_ELT(ans_el_names, NUM_NODE_ELEMENTS, COPY_TO_USER_STRING("value"));
    SET_VECTOR_ELT(ans, NUM_NODE_ELEMENTS, NEW_CHARACTER(1));
    SET_STRING_ELT(VECTOR_ELT(ans, NUM_NODE_ELEMENTS), 0, COPY_TO_USER_STRING(contentValue));
  }

  SET_NAMES(ans, ans_el_names);
  

    /* Compute the class of this object based on the type in the
       XML node.
     */

  RS_XML(setNodeClass)(node, ans);

     /* 
          Now invoke any user-level converters. 
      */
  ans = convertNode(ans, node, parserSettings);

  UNPROTECT(1);
  UNPROTECT(1);
  return(ans);
}

static USER_OBJECT_
convertNode(USER_OBJECT_ ans, xmlNodePtr node, R_XMLSettings *parserSettings)
{
    USER_OBJECT_ val = ans; /* R_NilValue; */

  if(parserSettings != NULL) {
    USER_OBJECT_  fun = NULL;
    const char *funName;
       if(node->name) {
          funName = XMLCHAR_TO_CHAR(node->name);
          fun = RS_XML(findFunction)(funName, parserSettings->converters);
       } 

      if(fun == NULL) {
	/* Didn't find the tag-specific function in the handlers. 
           So see if there is one for this type node.
         */
        fun = RS_XML(lookupGenericNodeConverter)(node, ans, parserSettings);
      }
      if(fun != NULL) {
        USER_OBJECT_ opArgs = NEW_LIST(1);
	 PROTECT(opArgs);
	 SET_VECTOR_ELT(opArgs, 0, ans);
	 val = RS_XML(invokeFunction)(fun, opArgs, NULL);
   	 UNPROTECT(1);
      }
  }
  return(val);
}


int
RS_XML(setNodeClass)(xmlNodePtr node, USER_OBJECT_ ans)
{
 char *className = NULL;
 int numEls = 1;
 int appendDefault = 1;

  switch(node->type) {
    case XML_ENTITY_REF_NODE:
      className = "XMLEntityRef";
      break;
    case XML_PI_NODE:
      className = "XMLProcessingInstruction";
      break;
    case XML_COMMENT_NODE:
      className = "XMLComment";
      break;
    case XML_TEXT_NODE:
      className = "XMLTextNode";
      break;
    case XML_CDATA_SECTION_NODE:
      className = "XMLCDataNode";
      break;
#ifdef LIBXML2
    case XML_ENTITY_DECL:
      className = "XMLEntityDeclaration";
      break;
#endif
   default:
     className = "XMLNode";
     appendDefault = 0;
     numEls = 1;
     break;
  }

  if(className) {
     USER_OBJECT_ Class;
     if(appendDefault)
       numEls++;
     PROTECT(Class = NEW_CHARACTER(numEls));
        SET_STRING_ELT(Class, 0, COPY_TO_USER_STRING(className));
	if(appendDefault)
          SET_STRING_ELT(Class, numEls-1, COPY_TO_USER_STRING("XMLNode"));
        SET_CLASS(ans, Class);
      UNPROTECT(1);
      className = NULL;
  }


  return(node->type);
}


const char *RS_XML(NameSpaceSlotNames)[] = {"id", "uri", "local"};
enum {NAMESPACE_PREFIX_SLOT, NAMESPACE_URI_SLOT, NAMESPACE_TYPE_SLOT, NAMESPACE_NUM_SLOTS};

/**
  Create a local object identifying the name space used by a particular node.
  This is not the name space definition which would have a URL/URI and a type.
 */
USER_OBJECT_
RS_XML(createNameSpaceIdentifier)(xmlNs *space, xmlNodePtr node)
{

 USER_OBJECT_ ans;

 if(node->nsDef) {
   PROTECT(ans = NEW_LIST(3));
     SET_VECTOR_ELT(ans, NAMESPACE_PREFIX_SLOT, NEW_CHARACTER(1));
     SET_STRING_ELT(VECTOR_ELT(ans, NAMESPACE_PREFIX_SLOT), 0, COPY_TO_USER_STRING(XMLCHAR_TO_CHAR( (space->prefix ? space->prefix : (xmlChar*)"")))); 

     SET_VECTOR_ELT(ans, NAMESPACE_URI_SLOT, NEW_CHARACTER(1));
     SET_STRING_ELT(VECTOR_ELT(ans, NAMESPACE_URI_SLOT), 0, space->href ? COPY_TO_USER_STRING(XMLCHAR_TO_CHAR(space->href)) : NA_STRING); 


     SET_VECTOR_ELT(ans, NAMESPACE_TYPE_SLOT, NEW_LOGICAL(1));
     LOGICAL_DATA(VECTOR_ELT(ans, NAMESPACE_TYPE_SLOT))[0] = (space->type == XML_LOCAL_NAMESPACE);

     RS_XML(SetNames)(NAMESPACE_NUM_SLOTS, RS_XML(NameSpaceSlotNames), ans);

   {
    USER_OBJECT_ klass;
    PROTECT(klass = NEW_CHARACTER(1));
     SET_STRING_ELT(klass, 0, COPY_TO_USER_STRING("XMLNameSpace"));
     SET_CLASS(ans, klass);
    UNPROTECT(1);
   }
   UNPROTECT(1);
 } else {
   PROTECT(ans =  NEW_CHARACTER(1));
   if(space->prefix)
      SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(XMLCHAR_TO_CHAR(space->prefix)));
   UNPROTECT(1);
 }


 return(ans);
}

/**
  Attempt to find a function in the handler methods corresponding to the
  type of the node, not its specific tag name.
 */
USER_OBJECT_
RS_XML(lookupGenericNodeConverter)(xmlNodePtr node, USER_OBJECT_ defaultNodeValue, 
                                     R_XMLSettings *parserSettings)
{
  char *name;
  USER_OBJECT_ fun = NULL;
  switch(node->type) {
    case XML_ENTITY_REF_NODE:
      name = "entity";
      break;
    case XML_ELEMENT_NODE:
      name = "startElement";
      break;
    case XML_PI_NODE:
      name = "proccesingInstruction";
      break;
    case XML_COMMENT_NODE:
      name = "comment";
      break;
    case XML_TEXT_NODE:
      name = "text";
      break;
    case XML_CDATA_SECTION_NODE:
      name = "cdata";
      break;
  default:
      name = NULL;     
  }

  if(name && name[0])
   fun = RS_XML(findFunction)(name, parserSettings->converters);   
 
 return(fun);
}

/**
  Creates the R objects representing the children or siblings of the specified
  node, handling simple text cases with no children, as well as recursively
  processing the children.

  node   the node whose children or siblings should be converted.
        
  direction DOWN or SIDEWAYS indicating the children or siblings should
    be processed, respectively. If SIDEWAYS is specified, the node itself
    is included in the result.

  parserSettings  "global" information about the parsing conversion for the duration of the parser.


  Return  list of XMLNode objects.
 */
USER_OBJECT_
RS_XML(createNodeChildren)(xmlNodePtr node, int direction, R_XMLSettings *parserSettings)
{
  int n = 0, i;
  USER_OBJECT_ ans = NULL_USER_OBJECT;
  USER_OBJECT_ elNames = NULL;
  int unProtect = 0;
  xmlNodePtr base, c = (direction == SIDEWAYS) ? node : 
#ifndef USE_OLD_ROOT_CHILD_NAMES
                              node->xmlChildrenNode;
#else
                              node->childs;
#endif

  base = c;

      /* Count the number of elements being converted. */
  while(c) {
    c = c->next;
    n++;
  }

  if(n > 0) {
    USER_OBJECT_ tmp;
    USER_OBJECT_ tmpNames;
    int count = 0;


    c = base;

    PROTECT(ans = NEW_LIST(n));
    PROTECT(elNames = NEW_CHARACTER(n));

    unProtect = 2;

    for(i = 0; i < n; i++, c = c->next) {
	tmp = RS_XML(createXMLNode)(c, 1, DOWN, parserSettings, ans);
	if(tmp && tmp != NULL_USER_OBJECT) {
	    SET_VECTOR_ELT(ans, count, tmp); 
	    if(c->name)
		SET_STRING_ELT(elNames, count, COPY_TO_USER_STRING(XMLCHAR_TO_CHAR(c->name)));
            count++;
	}
    }

     if(count < n) {
      /* Reset the length! */
#ifdef USE_S
#else
      PROTECT(tmp  = NEW_LIST(count));
      PROTECT(tmpNames = NEW_CHARACTER(count));
      for(i = 0 ;  i < count ; i++) {
        SET_VECTOR_ELT(tmp, i, VECTOR_ELT(ans, i));
        SET_STRING_ELT(tmpNames, i, STRING_ELT(elNames, i));
      }
      ans = tmp;
      SET_NAMES(ans, tmpNames);
      UNPROTECT(4);
      PROTECT(ans);
      unProtect = 1;
#endif
     } else {
         SET_NAMES(ans, elNames);
     }

    if(unProtect > 0)  
       UNPROTECT(unProtect);
  }

  return(ans);
}


/**
   Create an R named list containing the attributes of the specified node.
 */

/*
   We could use the CONS mechanism rather than doing a double pass.
   Not certain what is quicker in this situation. Also, doesn't
   work that way in S4, so keep it this way.
*/
USER_OBJECT_ 
RS_XML(AttributeList)(xmlNodePtr node, R_XMLSettings *parserSettings)
{
  USER_OBJECT_ ans = NULL_USER_OBJECT;
  USER_OBJECT_ ans_names;
  xmlAttr * atts;

  int n = 0, i;

      /* Count the number of attributes*/
    atts = node->properties;

    while(atts) {
      n++;
      atts = atts->next;
    }

  if(n > 0) {
    PROTECT(ans = NEW_CHARACTER(n));
    PROTECT(ans_names = NEW_CHARACTER(n));
         /* Loop over the attributes and create the string elements
            and the elements of the name vector.
          */


      atts = node->properties;

      for(i=0; i < n ; i++) {
	/* Have to be careful that atts->val and atts->val->context are non-null. Something like
           <a href=""> kills it otherwise.
         */
#ifdef LIBXML2
         SET_STRING_ELT(ans, i, 
                         COPY_TO_USER_STRING(XMLCHAR_TO_CHAR(
                                          ((atts->xmlChildrenNode != (xmlNode*)NULL && atts->xmlChildrenNode->content != (xmlChar*)NULL )?
                                                       atts->xmlChildrenNode->content : (xmlChar*)""))));
#else
         SET_STRING_ELT(ans, i, COPY_TO_USER_STRING(XMLCHAR_TO_CHAR(((atts->val != (xmlNode*)NULL && atts->val->content != (xmlChar*)NULL )? atts->val->content : (xmlChar*)""))));

#endif
         if(atts->name) {
           if(parserSettings->addAttributeNamespaces && atts->ns && atts->ns->prefix) {
             char buf[400];
             sprintf(buf, "%s:%s", atts->ns->prefix, atts->name);
             SET_STRING_ELT(ans_names, i, COPY_TO_USER_STRING(buf));
	   } else
             SET_STRING_ELT(ans_names, i, COPY_TO_USER_STRING(XMLCHAR_TO_CHAR(atts->name)));
	 }

         atts = atts->next;
      }

    SET_NAMES(ans, ans_names);

    UNPROTECT(1);
    UNPROTECT(1);
   }
  return(ans);
}

USER_OBJECT_
RS_XML(notifyNamespaceDefinition)(USER_OBJECT_ arg, R_XMLSettings *parserSettings)
{
 USER_OBJECT_ fun, ans = NULL_USER_OBJECT;

     fun = RS_XML(findFunction)("namespace", parserSettings->converters);
     if(fun != NULL) {
        USER_OBJECT_ opArgs = NEW_LIST(1);
        USER_OBJECT_ tmp;
	 PROTECT(opArgs);
	 SET_VECTOR_ELT(opArgs, 0, arg);
	 tmp = RS_XML(invokeFunction)(fun, opArgs, NULL);
         ans = tmp;
   	 UNPROTECT(1);
      }

 return(ans);
}

#ifdef USE_XML_VERSION_H
#ifndef LIBXML_TEST_VERSION
#include <libxml/xmlversion.h>
#endif
#endif

USER_OBJECT_
RS_XML(libxmlVersion)()
{
 USER_OBJECT_ ans;
 unsigned int val;

#ifdef LIBXML_VERSION_NUMBER
 val = LIBXML_VERSION_NUMBER;
#else 
#ifdef LIBXML_VERSION
 val = LIBXML_VERSION;
#else
 val = 0;
#endif
#endif

 ans = NEW_NUMERIC(1);
 NUMERIC_DATA(ans)[0] = val;
 return(ans);
}



static 
void
notifyError(const char *msg, va_list ap, Rboolean isError)
{
#if 0
    if(isError) {
	 PROBLEM "error in validating XML document"
	 ERROR;
    } else {
	 PROBLEM "warning when validating XML document"
	 ERROR;
    }

#else
#define BUFSIZE 2048
    char buf[BUFSIZE];

    memset(buf, '\0', BUFSIZE);
    vsnprintf(buf, BUFSIZE, msg, ap);

    PROBLEM buf
        WARN;

#endif
}



void
RS_XML(ValidationError)(void *ctx, const char *format, ...)
{
  char *msg = "Message unavailable";
  va_list(ap);
  va_start(ap, format);

  if(strcmp(format, "%s") == 0)
    msg = va_arg(ap, char *);

  va_end(ap);
  notifyError(msg, ap, TRUE);
}

void
RS_XML(ValidationWarning)(void *ctx, const char *format, ...)
{
  char *msg = "Message unavailable";
  va_list(ap);
  va_start(ap, format);

  if(strcmp(format, "%s") == 0)
    msg = va_arg(ap, char *);

  va_end(ap);
  notifyError(msg, ap, FALSE);
}


USER_OBJECT_
R_createXMLNode(USER_OBJECT_ snode, USER_OBJECT_ handlers)
{
    xmlNodePtr node = (xmlNodePtr) R_ExternalPtrAddr(snode);
    R_XMLSettings parserSettings;

    parserSettings.converters = handlers;

    return(RS_XML(createNodeChildren)(node, SIDEWAYS, &parserSettings));
}


USER_OBJECT_
RS_XML_xmlNodeName(USER_OBJECT_ snode)
{
    xmlNodePtr node = (xmlNodePtr) R_ExternalPtrAddr(snode);
    USER_OBJECT_ ans;

    PROTECT(ans = NEW_CHARACTER(1));
    SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(XMLCHAR_TO_CHAR(node->name)));
    UNPROTECT(1);    
    return(ans);
}


USER_OBJECT_
RS_XML_xmlNodeNamespace(USER_OBJECT_ snode)
{
    xmlNodePtr node = (xmlNodePtr) R_ExternalPtrAddr(snode);
    USER_OBJECT_ ans;
    xmlNs *ns;

    ns = node->ns;
    if(!ns)
	return(NEW_CHARACTER(0));

    PROTECT(ans = NEW_CHARACTER(2));
    SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(XMLCHAR_TO_CHAR(ns->prefix)));
    SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(XMLCHAR_TO_CHAR(ns->href)));
    UNPROTECT(1);    
    return(ans);
}

USER_OBJECT_
RS_XML_xmlNodeAttributes(USER_OBJECT_ snode, USER_OBJECT_ addNamespaces)
{
    xmlNodePtr node = (xmlNodePtr) R_ExternalPtrAddr(snode);
    R_XMLSettings parserSettings;
    parserSettings.addAttributeNamespaces = LOGICAL_DATA(addNamespaces)[0];

    return(RS_XML(AttributeList)(node, &parserSettings));
}

USER_OBJECT_
RS_XML_xmlNodeParent(USER_OBJECT_ snode)
{
    xmlNodePtr node = (xmlNodePtr) R_ExternalPtrAddr(snode);
    return(R_createXMLNodeRef(node->parent));
}



USER_OBJECT_
RS_XML_xmlNodeChildrenReferences(USER_OBJECT_ snode, USER_OBJECT_ addNamespaces)
{
    xmlNodePtr node = (xmlNodePtr) R_ExternalPtrAddr(snode);
    USER_OBJECT_ ans;
    int count = 0, i;
    xmlNodePtr ptr =  node->children;

    while(ptr) {
	count++;
	ptr = ptr->next;
    }

    ptr = node->children;

    PROTECT(ans = NEW_LIST(count));
    for(i = 0; i < count ; i++, ptr = ptr->next) {
	SET_VECTOR_ELT(ans, i, R_createXMLNodeRef(ptr));
    }
    UNPROTECT(1);
   
    return(ans);
}



static USER_OBJECT_
makeSchemaReference(xmlSchemaPtr schema)
{
    return(R_makeRefObject(schema, "xmlSchemaRef"));
/*
    USER_OBJECT_ ans;
    PROTECT(ans = R_MakeExternalPtr(schema, Rf_install("XMLSchema"), R_NilValue));
    SET_CLASS(ans, mkString("XMLSchema"));
    UNPROTECT(1);
    return(ans);
*/
}

