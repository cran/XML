/*
  This file uses the HTML parser in libxml to provide an HTML
  parser in R that is basically identical to the XML parsing interface.
  It can handle files, URLs, compressed files, and raw HTML text.
  It drops the DTD and validation options since these are not very relevant
  for HTML. (We can add put them back if anyone wants!)
 */

#include "DocParse.h"
#include "Utils.h"

#include "libxml/HTMLparser.h"

#include <sys/stat.h>
#include <unistd.h>  

USER_OBJECT_
RS_XML(HtmlParseTree)(USER_OBJECT_ fileName, USER_OBJECT_ converterFunctions, 
                       USER_OBJECT_ skipBlankLines, USER_OBJECT_ replaceEntities,
                       USER_OBJECT_ asText, USER_OBJECT_ trim, USER_OBJECT_ isURL)
{
  const char *name;
  xmlDocPtr doc;
  USER_OBJECT_ rdoc;
  USER_OBJECT_ className;
  R_XMLSettings parserSettings;
  int freeName = 0;

  int asTextBuffer = LOGICAL_DATA(asText)[0];
  int isURLDoc = LOGICAL_DATA(isURL)[0];

  parserSettings.skipBlankLines = LOGICAL_DATA(skipBlankLines)[0];
  parserSettings.converters = converterFunctions;
  parserSettings.trim = LOGICAL_DATA(trim)[0];

  if(asTextBuffer == 0) {
    struct stat tmp_stat;  
#ifdef USE_R
    name = CHAR(STRING_ELT(fileName, 0));
#else
    name = CHARACTER_DATA(fileName)[0];
#endif
    if(!isURLDoc && (name == NULL || stat(name, &tmp_stat) < 0)) {
      PROBLEM "Can't find file %s", CHAR_DEREF(STRING_ELT(fileName, 0))
      ERROR;
    }
  } else {
     name = strdup(CHAR_DEREF(STRING_ELT(fileName, 0)));
     freeName = 1;
  }


#if 0
    /* If one wants entities expanded directly and to appear as text.  */
  if(LOGICAL_DATA(replaceEntities)[0])
    xmlSubstituteEntitiesDefault(1);   
#endif

  if(asTextBuffer) {
   doc = htmlParseDoc(CHAR_TO_XMLCHAR(name), NULL);
   if(doc != NULL) {
      doc->name = (char *) xmlStrdup(CHAR_TO_XMLCHAR("<buffer>"));
   }
  } else {
      doc = htmlParseFile(name, NULL);
  }

  if(doc == NULL) {
    if(freeName && name)
        free((char *) name);
    PROBLEM "error in creating parser for %s", name
    ERROR;
  }

  PROTECT(rdoc = RS_XML(convertXMLDoc)(name, doc, converterFunctions, &parserSettings));

  if(freeName && name)
      free((char *) name);


  xmlFreeDoc(doc);

     /* Set the class for the document. */
  className = NEW_CHARACTER(1);
  PROTECT(className);
    SET_STRING_ELT(className, 0, COPY_TO_USER_STRING("HTMLDocument"));   
    SET_CLASS(rdoc, className);
  UNPROTECT(1);


 UNPROTECT(1); 
 return(rdoc);
}
