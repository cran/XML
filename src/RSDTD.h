/*
 * See Copyright for the license status of this software.
 */

#ifndef RSDTD_H
#define RSDTD_H


#include "RSCommon.h"

#define RS_XML(a)  RS_XML_##a

#include <libxml/valid.h>
#include <libxml/parser.h> 
#include <libxml/parserInternals.h> 
#include <libxml/tree.h>
#include <libxml/entities.h>

USER_OBJECT_ RS_XML(getDTD)(USER_OBJECT_ dtdFileName, USER_OBJECT_ externalId,  USER_OBJECT_ asText, USER_OBJECT_ isURL);
USER_OBJECT_ RS_XML(createDTDElement)(xmlElementPtr el);
USER_OBJECT_ RS_XML(createDTDElementContents)(xmlElementContentPtr vals, xmlElementPtr el, int recursive);
USER_OBJECT_ RS_XML(createDTDElementAttributes)(xmlAttributePtr vals, xmlElementPtr el);
USER_OBJECT_ RS_XML(createDTDAttribute)(xmlAttributePtr val, xmlElementPtr el);

USER_OBJECT_ RS_XML(AttributeEnumerationList)(xmlEnumerationPtr list, xmlAttributePtr attr, xmlElementPtr element);


USER_OBJECT_ RS_XML(SequenceContent)(xmlElementContentPtr vals, xmlElementPtr el);

USER_OBJECT_ RS_XML(ProcessElements)(xmlElementTablePtr table, xmlParserCtxtPtr ctxt);
USER_OBJECT_ RS_XML(ProcessEntities)(xmlEntitiesTablePtr table, xmlParserCtxtPtr ctxt);
USER_OBJECT_ RS_XML(createDTDEntity)(xmlEntityPtr entity);

USER_OBJECT_ RS_XML(createDTDParts)(xmlDtdPtr dtd, xmlParserCtxtPtr ctxt);

USER_OBJECT_ RS_XML(ConstructDTDList)(xmlDocPtr myDoc, int processInternals, xmlParserCtxtPtr ctxt);

#endif
