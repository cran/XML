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

int RS_XML(setNodeClass)(xmlNodePtr node, USER_OBJECT_ ans);
USER_OBJECT_ RS_XML(notifyNamespaceDefinition)(USER_OBJECT_ ns, R_XMLSettings *parserSettings);

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
                      USER_OBJECT_ getDTD, USER_OBJECT_ isURL)
{
extern int xmlDoValidityCheckingDefaultValue;

  char *name;
  xmlDocPtr doc;
  USER_OBJECT_ rdoc;
  USER_OBJECT_ className;
  R_XMLSettings parserSettings;
  int previousValiditySetting;
  int asTextBuffer = LOGICAL_DATA(asText)[0];
  int isURLDoc = LOGICAL_DATA(isURL)[0];

  parserSettings.skipBlankLines = LOGICAL_DATA(skipBlankLines)[0];
  parserSettings.converters = converterFunctions;
  parserSettings.trim = LOGICAL_DATA(trim)[0];


  previousValiditySetting = xmlDoValidityCheckingDefaultValue;

   /* Control whether we are validating or not. */
  xmlDoValidityCheckingDefaultValue = LOGICAL_DATA(validate)[0];

  if(asTextBuffer == 0) {
    struct stat tmp_stat;  
#ifdef USE_R
    name = R_ExpandFileName(CHAR(STRING_ELT(fileName, 0)));
#else
   name = CHARACTER_DATA(fileName)[0];
#endif
    if(!isURLDoc && (name == NULL || stat(name, &tmp_stat) < 0)) {
      xmlDoValidityCheckingDefaultValue = previousValiditySetting;
      PROBLEM "Can't find file %s", CHAR_DEREF(STRING_ELT(fileName, 0))
      ERROR;
    }
  } else {
    name = strdup(CHAR_DEREF(STRING_ELT(fileName, 0)));
  }

    /* If one wants entities expanded directly and to appear as text.  */
  if(LOGICAL_DATA(replaceEntities)[0])
    xmlSubstituteEntitiesDefault(1);   

  if(asTextBuffer) {
   doc = xmlParseMemory(name, strlen(name));
   if(doc != NULL) {
      doc->name = "<buffer>";
   }
  } else {
   doc = xmlParseFile(name);
  }

  if(doc == NULL) {
    xmlDoValidityCheckingDefaultValue = previousValiditySetting;
    PROBLEM "error in creating parser for %s", name
    ERROR;
  }

  PROTECT(rdoc = RS_XML(convertXMLDoc)(name, doc, converterFunctions, &parserSettings));

  if(asTextBuffer && name)
    free(name);

  xmlDoValidityCheckingDefaultValue = previousValiditySetting;


  if(LOGICAL_DATA(getDTD)[0]) {
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


     /* Set the class for the document. */
  className = NEW_CHARACTER(1);
  PROTECT(className);
    SET_STRING_ELT(className, 0, COPY_TO_USER_STRING("XMLDocument"));   
    SET_CLASS(rdoc, className);
  UNPROTECT(1);

 UNPROTECT(1); 
 return(rdoc);
}

enum { FILE_ELEMENT_NAME, VERSION_ELEMENT_NAME, CHILDREN_ELEMENT_NAME, NUM_DOC_ELEMENTS,};


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

  PROTECT(rdoc = NEW_LIST(n));
  PROTECT(rdoc_el_names = NEW_CHARACTER(n));
 
    /* Insert the name of the file being processed */
  SET_VECTOR_ELT(rdoc, FILE_ELEMENT_NAME, NEW_CHARACTER(1));
    SET_STRING_ELT(VECTOR_ELT(rdoc, FILE_ELEMENT_NAME), 0, COPY_TO_USER_STRING(doc->name ? doc->name : fileName));
    SET_STRING_ELT(rdoc_el_names, FILE_ELEMENT_NAME, COPY_TO_USER_STRING("file"));

    /* Insert the XML version information */
  SET_VECTOR_ELT(rdoc, VERSION_ELEMENT_NAME, NEW_CHARACTER(1));
    SET_STRING_ELT(VECTOR_ELT(rdoc, VERSION_ELEMENT_NAME), 0, COPY_TO_USER_STRING(doc->version));
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


/**
   Creates an R object representing the specified node, and its children
   if recursive is non-zero. Certain types of nodes have 

   direction controls whether we take the siblings of this node
   or alternatively its children.

   parentUserNode the previously created user-leve node for the parent of the
            target node. 
 */

enum { NODE_NAME, NODE_ATTRIBUTES, NODE_CHILDREN, NODE_NAMESPACE, NUM_NODE_ELEMENTS};

USER_OBJECT_
RS_XML(createXMLNode)(xmlNodePtr node, int recursive, int direction, R_XMLSettings *parserSettings, USER_OBJECT_ parentUserNode)
{
  int n = NUM_NODE_ELEMENTS;
  USER_OBJECT_ ans;
  USER_OBJECT_ ans_el_names;
  USER_OBJECT_ nsDef = NULL_USER_OBJECT;
  int addValue;

  char *contentValue = node->content;

#ifdef ROOT_HAS_DTD_NODE
  if(node->type == XML_DTD_NODE)
    return(NULL);
#endif

  if(parserSettings->trim) {
    contentValue = trim(node->content);
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


  if(node->nsDef)  {
     nsDef = RS_XML(createNameSpaceIdentifier)(node->nsDef, node);
    (void) RS_XML(notifyNamespaceDefinition)(nsDef, parserSettings);
  }

     /* Create the default return value being a list of name, attributes, children 
        and possibly value. 
      */
 PROTECT(ans = NEW_LIST(n));
 PROTECT(ans_el_names = NEW_CHARACTER(n));

  SET_VECTOR_ELT(ans, NODE_NAME, NEW_CHARACTER(1));
  if(node->name)
    SET_STRING_ELT(VECTOR_ELT(ans, NODE_NAME), 0, COPY_TO_USER_STRING(node->name));

  SET_VECTOR_ELT(ans, NODE_ATTRIBUTES, RS_XML(AttributeList)(node, parserSettings));

  if(recursive)
    SET_VECTOR_ELT(ans, NODE_CHILDREN, RS_XML(createNodeChildren)(node, direction, parserSettings));
  else
    SET_VECTOR_ELT(ans, NODE_CHILDREN, NULL_USER_OBJECT); 

  SET_STRING_ELT(ans_el_names, NODE_NAME, COPY_TO_USER_STRING("name"));
  SET_STRING_ELT(ans_el_names, NODE_ATTRIBUTES,  COPY_TO_USER_STRING("attributes"));
  SET_STRING_ELT(ans_el_names, NODE_CHILDREN, COPY_TO_USER_STRING("children"));
  SET_STRING_ELT(ans_el_names, NODE_NAMESPACE, COPY_TO_USER_STRING("namespace"));

  if(node->ns) {
    if(!node->nsDef)
       nsDef = RS_XML(createNameSpaceIdentifier)(node->ns, node);
    SET_VECTOR_ELT(ans, NODE_NAMESPACE, nsDef);
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

  if(parserSettings != NULL) {
    USER_OBJECT_  fun = NULL;
    const char *funName;
       if(node->name) {
          funName = node->name;
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
        USER_OBJECT_ tmp;
	 PROTECT(opArgs);
	 SET_VECTOR_ELT(opArgs, 0, ans);
	 tmp = RS_XML(invokeFunction)(fun, opArgs);
         ans = tmp;
   	 UNPROTECT(1);
      }
  }


  UNPROTECT(1);
  UNPROTECT(1);
  return(ans);
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
     SET_STRING_ELT(VECTOR_ELT(ans, NAMESPACE_PREFIX_SLOT), 0, COPY_TO_USER_STRING(space->prefix ? space->prefix : (xmlChar*)"")); 

     SET_VECTOR_ELT(ans, NAMESPACE_URI_SLOT, NEW_CHARACTER(1));
     SET_STRING_ELT(VECTOR_ELT(ans, NAMESPACE_URI_SLOT), 0, COPY_TO_USER_STRING(space->href)); 

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
      SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(space->prefix));
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
  xmlNodePtr c = (direction == SIDEWAYS) ? node : 
#ifndef USE_OLD_ROOT_CHILD_NAMES
                              node->xmlChildrenNode;
#else
                              node->childs;
#endif

  while(c) {
    c = c->next;
    n++;
  }

  if(n > 0) {
    USER_OBJECT_ tmp;
    USER_OBJECT_ tmpNames;

    int count = 0;
    PROTECT(ans = NEW_LIST(n));
    PROTECT(elNames = NEW_CHARACTER(n));
      unProtect = 2;
    c = (direction == SIDEWAYS) ? node :
#ifndef USE_OLD_ROOT_CHILD_NAMES
                              node->xmlChildrenNode;
#else
                              node->childs;
#endif
    for(i = 0; i < n; i++) {
      tmp = RS_XML(createXMLNode)(c, 1, DOWN, parserSettings, ans);
      if(tmp && GET_LENGTH(tmp)) {
	SET_VECTOR_ELT(ans, count, tmp); 
        if(c->name)
          SET_STRING_ELT(elNames, count++, COPY_TO_USER_STRING(c->name));
      }
      c = c->next;
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
#ifdef LIBXML2
    atts = node->properties;
    /*   atts = node->children; */
#else
    atts = node->properties;
#endif
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

#ifdef LIBXML2
    /*      atts = node->children; */
      atts = node->properties;
#else
      atts = node->properties;
#endif
      for(i=0; i < n ; i++) {
	/* Have to be careful that atts->val and atts->val->context are non-null. Something like
           <a href=""> kills it otherwise.
         */
#ifdef LIBXML2
         SET_STRING_ELT(ans, i, COPY_TO_USER_STRING((atts->xmlChildrenNode != (xmlNode*)NULL && atts->xmlChildrenNode->content != (xmlChar*)NULL )? atts->xmlChildrenNode->content : (xmlChar*)""));
#else
         SET_STRING_ELT(ans, i, COPY_TO_USER_STRING((atts->val != (xmlNode*)NULL && atts->val->content != (xmlChar*)NULL )? atts->val->content : (xmlChar*)""));

#endif
         if(atts->name)
           SET_STRING_ELT(ans_names, i, COPY_TO_USER_STRING(atts->name));
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
	 tmp = RS_XML(invokeFunction)(fun, opArgs);
         ans = tmp;
   	 UNPROTECT(1);
      }

 return(ans);
}
