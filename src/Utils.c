/**
   Routines that are shared across the two XML parsers and their callbacks
   to R.
    isBlank - determines if a string consists entirely of whitespace
    RS_XML(invokeFunction) - call a user-level function, previously located
                             by RS_XML(findFunction).
    RS_XML(findFunction)   - search a list or closure for a function object
                             with a given name in that list.


 * See Copyright for the license status of this software.

 */

#include "Utils.h"  

#include <ctype.h>  /* For isspace() */

#ifdef LIBXML
#ifdef FROM_GNOME_XML_DIR
#include <gnome-xml/parser.h>
#else
#include <libxml/parser.h>
#endif
#endif

#include "RSCommon.h" /* for SET_NAMES */

/**
   Tests whether the string contains only white space
   or not. Returns 1 if is only white space. 0 otherwise.
 */

int isBlank(const char *str)
{
  int blank=0; 
  const char *ptr = str;
  while(ptr && (blank = isspace(ptr[0]))) {
    ptr++;
  }
  return(blank);
}

/**
  Does an in place trimming of a string by returning a pointer
  to the first non-white space character and also inserting a
  string terminator after the last non-whitespace character.
 */
char *
trim(char *str)
{
  char *tmp;

    /* If a degenerate string, just return. */
  if(str == (char*)NULL || str[0] == '\0')
    return(str);

   /* Jumpt to the end */
  tmp = str + strlen(str) - 1;
  while(tmp >= str && isspace(*tmp)) {
      tmp[0] = '\0';
      tmp--;
  }
  if(tmp == str) {
#if 0
   if(strlen(tmp) > 1)
    tmp[0] = '\0';
#endif
   return(str);
  }
#if 0
 else
    tmp[1] = '\0';
#endif


  tmp = str;

  while(*tmp && isspace(*tmp)) {
    tmp++;
  }

    return(tmp);
}


USER_OBJECT_
RS_XML(treeApply)(USER_OBJECT_ rtree, USER_OBJECT_ function, USER_OBJECT_ args)
{

  return(rtree);
}


/**
  Error handling utilities for use with the libxml document parsing mechanism.
  Intercept the error handling by replacing it with a routine of the same name
  and have it print to a buffer. Then call the Warning handler. Then the warnings
  will end up in the local system, accessible via the warnings() function.
  This allows them to be programmatically processed rather than having to process 
  the output to the terminal (via catching it in a call sink()).
 */

#ifdef LIBXML

#include <stdarg.h>

void localXmlParserPrintFileInfo(xmlParserInputPtr input, char *buf);

void xmlParserError(void *ctx, const char *msg, ...)
{ 
  xmlParserCtxtPtr ctxt = (xmlParserCtxtPtr) ctx;
  char buf[3000], *tmp;
  va_list args;

    /* Empty the string buffer. */
  memset(buf , '\0', sizeof(buf)/sizeof(buf[0]));

    /* Insert the file and line number. */
  localXmlParserPrintFileInfo(ctxt->input, buf);
    /* Move to the end of the buffer's contents. */
  tmp = buf + strlen(buf);

  va_start(args, msg);
    /* Write in the actual message. */
  vsprintf(tmp, msg, args);
  va_end(args);
  PROBLEM "XML Parsing Error: %s", buf
  WARN;
}

/**
    Write the file name and the current line number into the specified 
    string.
 */
void localXmlParserPrintFileInfo(xmlParserInputPtr input, char *buf) {
    if (input != NULL) {
	if (input->filename)
	    sprintf(buf, "%s:%d: ", input->filename,
		    input->line);
	else
	    sprintf(buf, "Entity: line %d: ", input->line);
    }
}

#endif




/**
  Utility method for setting the names of a list/vector from an array of
  native strings rather than an R/S character vector structure.
 */
void
RS_XML(SetNames)(int n, const char *cnames[], USER_OBJECT_ ans)
{
  int i;
  USER_OBJECT_ names;
  PROTECT(names = NEW_CHARACTER(n));
  for(i = 0; i < n ; i++) {
    /* could install as a pre-defined string. */
    /*
         CHARACTER_DATA(names)[i] = COPY_TO_USER_STRING(cnames[i]);
    */
         SET_STRING_ELT(names, i, COPY_TO_USER_STRING(cnames[i]));
  }

  SET_NAMES(ans, names);
  UNPROTECT(1);

}



/*
  Set the class of the target object to be the character vector containing
  just the specified name.
 */
int
RS_XML(SetClassName)(const char *localClassName, USER_OBJECT_ target)
{
    USER_OBJECT_ className;
     PROTECT(className = NEW_CHARACTER(1)); 
     /*
       CHARACTER_DATA(className)[0] = COPY_TO_USER_STRING(localClassName);
     */
       SET_STRING_ELT(className, 0, COPY_TO_USER_STRING(localClassName));
       SET_CLASS(target, className);
     UNPROTECT(1);
 
  return(1);
}


#if LIBXML2
struct _xmlHashTable {     
  struct _xmlHashEntry **table;    
  int size; 
}; 
#endif

#if OWN_XML_HASH_SIZE
int xmlHashSize(xmlHashTablePtr table)
{
  /* For version 2.2.* */
  return(table->size);
  /*
  return(table->nb_entities);
  */
}
#endif

USER_OBJECT_
RS_XML(findFunction)(const char *opName, USER_OBJECT_ _userObject)
{
  int i;
  USER_OBJECT_ fun = NULL;
 
     /* Get the names of the list. */
  USER_OBJECT_ names = GET_NAMES(_userObject);
     /* lookup function in the names of the list */
  for (i = 0; i < GET_LENGTH(names); i++) {
      if(!strcmp(opName, CHAR_DEREF(STRING_ELT(names, i)))) {
          fun = VECTOR_ELT(_userObject, i);
          break;
      }
  }
  return(fun);
}          
