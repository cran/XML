
/*
 Copyright the Omegahat project 1999-2005.
 Distributed under the GPL license (version 2).
*/

/*
 Cut-dpwn version for XML as an R package
 */

#ifndef RSCOMMON_H
#define RSCOMMON_H

#ifdef __cplusplus 
extern "C" {
#endif

  #include <Rinternals.h>
  #include <Rdefines.h>
  #ifdef length
  #undef length
  #endif

  #ifdef GET_LENGTH
  #undef GET_LENGTH
  #define GET_LENGTH(x) Rf_length(x)
  #endif

  #ifdef append
  #undef append
  #endif

  typedef SEXP USER_OBJECT_;
  typedef int RSInt;
  #include "R_ext/Boolean.h"

  #define CHAR_DEREF(x)   CHAR((x))
  #define IS_FUNCTION(x)   isFunction((x))

#ifdef __cplusplus 
}
#endif

#endif /* end of RSCOMMON_H*/


