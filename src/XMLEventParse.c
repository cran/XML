#include "EventParse.h"
#include "DocParse.h"

#ifdef FROM_GNOME_XML_DIR
#include <gnome-xml/parserInternals.h>
#else
#include <libxml/parserInternals.h>
#endif

/*
  This is an event driven parsing implementation for R & S
  using the libxml (http://xmlsoft.org) rather than Jim Clark's expat.
  It works much the same way, but has some advantages
   a) only one library need be installed for both document
     and event parsing
   b) the libxml tools can read data via ftp and http.

  Both expat and libxml provide the SAX interface and allow us to share
  a great deal of code between the two event parser implementations
  within this package.
 */

void RS_XML(startElementHandler)(void *ctx, const xmlChar *name, const xmlChar **atts);
void RS_XML(commentElementHandler)(void *ctx, const xmlChar *val);
void RS_XML(charactersHandler)(void *user_data, const xmlChar *ch, int len);
void RS_XML(endElementHandler)(void *ctx, const xmlChar *name);
void RS_XML(startDocumentHandler)(void *ctx);
void RS_XML(endDocumentHandler)(void *ctx);
void RS_XML(cdataBlockHandler)(void *ctx, const xmlChar *value, int len);
void RS_XML(piHandler)(void *ctx, const xmlChar *target, const xmlChar *data);
void RS_XML(entityDeclaration)(void *ctx, const xmlChar *name, int type, const xmlChar *publicId, 
                                const xmlChar *systemId, xmlChar *content);

int RS_XML(isStandAloneHandler)(void *ctx);
void RS_XML(initXMLParserHandler)(xmlSAXHandlerPtr xmlParserHandler);
void RS_XML(warningHandler)(void *ctx, const char *msg, ...);
void RS_XML(errorHandler)(void *ctx, const char *msg, ...);
void RS_XML(fatalErrorHandler)(void *ctx, const char *msg, ...);


void
RS_XML(libXMLEventParse)(const char *fileName, RS_XMLParserData *parserData, int asText)
{
 extern int xmlDoValidityCheckingDefaultValue;
 xmlSAXHandlerPtr xmlParserHandler;
 xmlParserCtxtPtr ctx = (xmlParserCtxtPtr) calloc(1, sizeof(xmlParserCtxtPtr));

      xmlDoValidityCheckingDefaultValue = 1;

  xmlParserHandler = (xmlSAXHandlerPtr) Calloc(1,xmlSAXHandler);
  /* Make certain this is initialized so that we don't have any references
     to unwanted routines!
   */
  memset(xmlParserHandler, '\0', sizeof(xmlSAXHandler));

  RS_XML(initXMLParserHandler)(xmlParserHandler);


 if(asText)
  ctx = xmlCreateDocParserCtxt((char *)fileName);
 else
  ctx = xmlCreateFileParserCtxt(fileName);

  if(ctx == NULL) {
    PROBLEM "Can't parse %s", fileName
    ERROR;
  }

  ctx->userData = parserData;
  ctx->sax = xmlParserHandler;

  xmlParseDocument(ctx);

  ctx->sax = NULL;
  xmlFreeParserCtxt(ctx);

  Free(xmlParserHandler);
}


void
RS_XML(initXMLParserHandler)(xmlSAXHandlerPtr xmlParserHandler)
{
  xmlParserHandler->startElement = RS_XML(startElementHandler);
  xmlParserHandler->endElement = RS_XML(endElementHandler);
  xmlParserHandler->comment = RS_XML(commentElementHandler);
  xmlParserHandler->entityDecl = RS_XML(entityDeclaration);
  /* external entity 
   */
  xmlParserHandler->characters = RS_XML(charactersHandler);
  xmlParserHandler->processingInstruction = RS_XML(piHandler);
  xmlParserHandler->cdataBlock = RS_XML(cdataBlockHandler);

  xmlParserHandler->startDocument = RS_XML(startDocumentHandler);
  xmlParserHandler->endDocument = RS_XML(endDocumentHandler);

  xmlParserHandler->isStandalone = RS_XML(isStandAloneHandler);
  xmlParserHandler->fatalError = RS_XML(fatalErrorHandler);
  xmlParserHandler->warning = RS_XML(warningHandler);
  xmlParserHandler->error = RS_XML(errorHandler);
}

void
RS_XML(startElementHandler)(void *userData, const xmlChar *name, const xmlChar **atts)
{
  /*  fprintf(stderr, "In startElementHandler for libxml event parsing\n");fflush(stderr); */
  RS_XML(startElement)(userData, (const char *)name, (const char **)atts);
}

void
RS_XML(endElementHandler)(void *ctx, const xmlChar *name)
{
  RS_XML(endElement)(ctx, (const char *)name);
}

void
RS_XML(commentElementHandler)(void *ctx, const xmlChar *val)
{
  RS_XML(commentHandler)(ctx, (const XML_Char*)val);
}

void 
RS_XML(charactersHandler)(void *user_data, const xmlChar *ch, int len)
{
  RS_XML(textHandler)(user_data, (const XML_Char*)ch, len);
}

void
RS_XML(startDocumentHandler)(void *ctx)
{

}

void
RS_XML(endDocumentHandler)(void *ctx)
{

}

void
RS_XML(cdataBlockHandler)(void *ctx, const xmlChar *value, int len)
{
  //  fprintf(stderr, "CDATA: %s %d\n", value, len); fflush(stderr);
 USER_OBJECT_ opArgs;

 PROTECT(opArgs = NEW_LIST(1));
 SET_VECTOR_ELT(opArgs, 0, NEW_CHARACTER(1));
   SET_STRING_ELT(VECTOR_ELT(opArgs, 0), 0, COPY_TO_USER_STRING(value));
  RS_XML(callUserFunction)("cdata", (const char *)NULL, (RS_XMLParserData*)ctx, opArgs);
  UNPROTECT(1);
}

void
RS_XML(piHandler)(void *ctx, const xmlChar *target, const xmlChar *data)
{
  RS_XML(processingInstructionHandler)(ctx, (const XML_Char*)target, (const XML_Char*)data);
}

void
RS_XML(entityDeclaration)(void *ctx,
                            const xmlChar *name, int type, const xmlChar *publicId,
			    const xmlChar *systemId, xmlChar *content)
{

}


int
RS_XML(isStandAloneHandler)(void *ctx)
{
  return(1);
}

void
RS_XML(fatalErrorHandler)(void *ctx, const char *msg, ...)
{

  PROBLEM "Fatal error in the XML event driven parser for %s.",
                    ((RS_XMLParserData*) ctx)->fileName
 ERROR;

}

void
RS_XML(errorHandler)(void *ctx, const char *msg, ...)
{
  PROBLEM "Error in the XML event driven parser for %s: %s",
    ((RS_XMLParserData*) ctx)->fileName, msg
  ERROR;

}

void
RS_XML(warningHandler)(void *ctx, const char *msg, ...)
{

 PROBLEM "XML event driven parser warning from %s.",
   ((RS_XMLParserData*) ctx)->fileName
 WARN;

}
