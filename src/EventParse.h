/*
 * See Copyright for the license status of this software.
 */

#ifndef EVENT_PARSE_H
#define EVENT_PARSE_H

#include <ctype.h>
#include <stdlib.h> 

#include "RSCommon.h"
#include "RS_XML.h"

#ifdef LIBEXPAT
#include "xmlparse.h"
#else
typedef char XML_Char;
#endif



   /* Extensible Struct for carrying information about the parser and its 
      options as specified by the caller from R or S.
    */
typedef struct {
  /**
    The name of the source file which is being parsed.
   */
  char *fileName;

  /**
     Flag indicating whether blank (white-space only) text entries should be discarded
     and not reported.
   */
  int ignoreBlankLines;

  /**
     Flag indicating whether the methods in the user-level functions
     should be invoked with additional information about the current
     context of the parser, specifically the level or depth of the current node in the 
     tree, potentially the index sequence (i_child1, i_child2, i_child3,...) which 
     identifies the node relative to the root of the tree.

     Specify this in the call to xmlEventParse().
   */
  int addContextInfo;

  /* Flag indicating whether an attempt should be made when calling
     startElement to lookup a method matching the tag name rather than
     the vanilla startElement method.

     Set this in the call xmlEventParse().
   */
  int callByTagName;

   /* The R object in which to search for appropriate methods, usually a closure. */
  USER_OBJECT_ methods;

   /* 
      The current depth in the XML document tree. 
      Used when constructing
    */
  int depth;

  /*
     Flag indicating whether we should trim the 
   */
  int trim;


   /* S object used in event parsing to share state across calls. */
  USER_OBJECT_ stateObject;
} RS_XMLParserData;


USER_OBJECT_ 
RS_XML(Parse)(USER_OBJECT_ fileName, USER_OBJECT_ handlers, USER_OBJECT_ addContext, USER_OBJECT_ ignoreBlanks, USER_OBJECT_ useTagName, USER_OBJECT_ asText, USER_OBJECT_ trim, USER_OBJECT_ useExpat, USER_OBJECT_ state);

/* Allocate a data structure for use with the parser */
RS_XMLParserData *createRSXMLParserData(USER_OBJECT_ handlers) ;

USER_OBJECT_ RS_XML(callUserFunction)(char *opName, const char *preferredName, RS_XMLParserData *parser, USER_OBJECT_ opArgs) ;
USER_OBJECT_ RS_XML(createAttributesList)(const char **atts);


void RS_XML(entityDeclarationHandler)(void *userData, const XML_Char *entityName, 
                          const XML_Char *base, const XML_Char *systemId, 
                          const XML_Char *publicId, const XML_Char *notationName);


void RS_XML(entityDeclarationHandler)(void *userData, const XML_Char *entityName, 
                                      const XML_Char *base, const XML_Char *systemId, 
				     const XML_Char *publicId, const XML_Char *notationName);

void RS_XML(commentHandler)(void *userData, const XML_Char *data);

void RS_XML(endElement)(void *userData, const char *name);
void RS_XML(startElement)(void *userData, const char *name, const char **atts);
void RS_XML(processingInstructionHandler)(void *userData, const XML_Char *target, const XML_Char *data); 
void RS_XML(textHandler)(void *userData,  const XML_Char *s, int len);
void RS_XML(startCdataSectionHandler)(void *userData) ;
void RS_XML(endCdataSectionHandler)(void *userData) ;
RS_XMLParserData *RS_XML(createParserData)(USER_OBJECT_ handlers);


int RS_XML(parseBufferWithParserData)(char *buf, RS_XMLParserData *parserData);
int RS_XML(notStandAloneHandler)(void *userData);
#endif
