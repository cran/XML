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


#ifdef  NEED_CLOSE_CALLBACK
/* Is this actually needed? We can ensure that all errors
   are caught by R and so ensure that we close things.
*/
int
RS_XML_closeConnectionInput(void *context)
{
   int status;
   status = RS_XML_readConnectionInput(context, NULL, -1);

   return(1);
}
#endif

int
RS_XML_readConnectionInput(void *context, char *buffer, int len)
{
  SEXP e, tmp, arg;
  int n;
  int errorOccurred;
  char *str;
  int left = len-1, count;

#ifdef R_XML_DEBUG
  char *orig = buffer;
#endif

  xmlParserCtxtPtr ctx = (xmlParserCtxtPtr) context;

  if(isFunction(ctx->_private)) {
     /* Invoke the user-provided function to get the 
        next line. */
    PROTECT(e = allocVector(LANGSXP, 2));
    SETCAR(e, ctx->_private);
    arg = NEW_INTEGER(1);
    INTEGER_DATA(arg)[0] = len;
    SETCAR(CDR(e), arg);
  } else {
       /* Call readLines on the connection to get the next line. */
    PROTECT(e = allocVector(LANGSXP, buffer ? 3 : 2));
    SETCAR(e, install(buffer ? "readLines" : "close"));
    SETCAR(CDR(e), ctx->_private);
    if(buffer) {
      tmp = NEW_INTEGER(1);
      INTEGER_DATA(tmp)[0] = 1;
      SETCAR(CDR(CDR(e)), tmp);
    }
  }

  n = count = 0;
  while(n == 0 && left > 0) {
   str = NULL;

     /* Update the argument to the user-defined function to say how much is left. */
   if(isFunction(ctx->_private))
     INTEGER_DATA(arg)[0] = left;

   tmp = R_tryEval(e, R_GlobalEnv, &errorOccurred);

   if(errorOccurred || !IS_CHARACTER(tmp)) {
     UNPROTECT(1);
     if ((ctx->sax != NULL) && (ctx->sax->error != NULL))
       ctx->sax->error(ctx->userData, "Failed to call read on XML connection");
    /* throw an XML error. */
     return(-1);
   }

   if(GET_LENGTH(tmp)) {
    str = CHAR_DEREF(STRING_ELT(tmp, 0));
    n = strlen(str);
    if(n == 0) {
       buffer[0] = '\n';
       buffer++;
       count++;
       left--;
    } else {
      if(n > left) {
        PROBLEM "string read from XML connection too long for buffer: truncating %s to %d characters", str, left
	WARN;
      }
      strncpy(buffer, str, left);
      left -= n;

      /* should add the \n that R took off via readLines. */
      if(left > 0) {
          buffer[n] = '\n';
	  left--;
      }
      count += (n + 1);

    }
   } else {
     /* Notice that we may have actually added something to the 
        buffer, specifically a sequence of empty lines \n, 
        and these will be discarded and not passed to the XML parser
        but these are extraneous anyway.
      */
    n = count = 0;
    break;
   }
  }

#ifdef R_XML_DEBUG
  fprintf(stderr, "size (n=%d, count=%d) %s '%s'\n", n, count, str, orig);fflush(stderr);
#endif

  UNPROTECT(1);

  return(count);
/*  return(count == 0 ? -1 : count); */
}

xmlParserCtxtPtr
RS_XML_xmlCreateConnectionParserCtxt(USER_OBJECT_ con)
{
      xmlParserInputBufferPtr buf;
      xmlParserCtxtPtr ctx = NULL;

#ifdef LIBXML2
      ctx = xmlNewParserCtxt();
      ctx->_private = (USER_OBJECT_) con;
      buf = (xmlParserInputBufferPtr) R_chk_calloc(1, sizeof(xmlParserInputBuffer));
      buf->readcallback = RS_XML_readConnectionInput;
      buf->context = (void*) ctx;
      buf->buffer = xmlBufferCreate();
      buf->raw = NULL; /* buf->buffer; */

      ctx->input = xmlNewIOInputStream(ctx, buf, XML_CHAR_ENCODING_NONE);

      inputPush(ctx, ctx->input);
#endif
      return(ctx);
}

void
RS_XML(libXMLEventParse)(const char *fileName, RS_XMLParserData *parserData, RS_XML_ContentSourceType asText)
{
 extern int xmlDoValidityCheckingDefaultValue;
 int oldValiditySetting;
 xmlSAXHandlerPtr xmlParserHandler;
 
 xmlParserCtxtPtr ctx; /* = (xmlParserCtxtPtr) calloc(1, sizeof(xmlParserCtxtPtr)); XXX */

#ifdef HAVE_VALIDITY
      oldValiditySetting = xmlDoValidityCheckingDefaultValue;
      xmlDoValidityCheckingDefaultValue = 1;
#endif

  xmlParserHandler = (xmlSAXHandlerPtr) Calloc(1,xmlSAXHandler);
  /* Make certain this is initialized so that we don't have any references
     to unwanted routines!
   */
  memset(xmlParserHandler, '\0', sizeof(xmlSAXHandler));

  RS_XML(initXMLParserHandler)(xmlParserHandler);


  switch(asText) {
    case RS_XML_TEXT:
      ctx = xmlCreateDocParserCtxt((char *)fileName);
      break;

    case RS_XML_FILENAME:
      ctx = xmlCreateFileParserCtxt(fileName);
      break;

    case RS_XML_CONNECTION:
      ctx = RS_XML_xmlCreateConnectionParserCtxt((USER_OBJECT_) fileName);

      break;
    default:
      ctx = NULL;
  }

#ifdef HAVE_VALIDITY
    xmlDoValidityCheckingDefaultValue = oldValiditySetting ;
#endif

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
/*XXX Need to be smarter here about the msg coming from libxml containing formatting instructions
  e.g. %s and then picking up the ... */
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
