#include "RUtils.h"

/*
  Utilities used in the R XML parsing facilities for invoking user-level functions from C.

 * See Copyright for the license status of this software.

 */

#include "Rinternals.h" /* Macros, etc. */

USER_OBJECT_ R_InternalRecursiveApply(USER_OBJECT_ top, USER_OBJECT_ func, USER_OBJECT_ klasses);

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
RS_XML(RecursiveApply)(USER_OBJECT_ top, USER_OBJECT_ func, USER_OBJECT_ klasses)
{
  USER_OBJECT_ ans;
  PROTECT(top = duplicate(top));
  ans = R_InternalRecursiveApply(top, func, klasses);
  UNPROTECT(1);
  return(ans);
}

USER_OBJECT_
R_InternalRecursiveApply(USER_OBJECT_ top, USER_OBJECT_ func, USER_OBJECT_ klasses)
{
  int CHILD_NODE = 2, i;
  USER_OBJECT_ kids;
  int numChildren;
  USER_OBJECT_ args, tmp;


  if(GET_LENGTH(top) > CHILD_NODE) {
    kids = VECTOR_ELT(top, CHILD_NODE);
    numChildren = GET_LENGTH(kids);
        /* Do the children first. */
    PROTECT(args = NEW_LIST(1));
    PROTECT(tmp = NEW_LIST(numChildren));  
    for(i = 0; i < numChildren; i++) {
      SET_VECTOR_ELT(tmp, i, R_InternalRecursiveApply(VECTOR_ELT(kids, i), func, klasses));
    }
    SET_VECTOR_ELT(top, CHILD_NODE, tmp);
    UNPROTECT(2);
  }

  PROTECT(args = NEW_LIST(1));
  SET_VECTOR_ELT(args, 0, top);
  tmp =  RS_XML(invokeFunction)(func, args);
  UNPROTECT(1);


  return(tmp);
}

