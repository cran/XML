#include "RUtils.h"

/*
  Utilities used in the R XML parsing facilities for invoking user-level functions from C.

 * See Copyright for the license status of this software.

 */

#include "Rinternals.h" /* Macros, etc. */

USER_OBJECT_
RS_XML(invokeFunction)(USER_OBJECT_ fun, USER_OBJECT_ opArgs)
{
  int i;
  long n;
  USER_OBJECT_ c, call;
  USER_OBJECT_ ans;

  n = GET_LENGTH(opArgs);
  if(n  > 0) {
    PROTECT(c = call = allocList(n));
    for (i = 0; i < n; i++) {
      SETCAR(c, VECTOR_ELT(opArgs, i));
      c = CDR(c);
    }

    call = LCONS(fun, call);
    UNPROTECT(1);
  } else  {
     call = allocVector(LANGSXP,1);
     SETCAR(call, fun);
  }  

  PROTECT(call);
   ans = eval(call, R_GlobalEnv);
  UNPROTECT(1);

  return(ans);
}

USER_OBJECT_
RS_XML(findFunction)(const char *opName, USER_OBJECT_ _userObject) 
{ 
  int i;
  USER_OBJECT_ fun = NULL;

     /* Get the names of the list. */
  USER_OBJECT_ names = getAttrib(_userObject, R_NamesSymbol);
     /* lookup function in the names of the list */
  for (i = 0; i < GET_LENGTH(names); i++) {
      if(!strcmp(opName, CHAR(STRING_ELT(names, i)))) {
          fun = VECTOR_ELT(_userObject, i);
          break;
      }
  }
  return(fun);
}


