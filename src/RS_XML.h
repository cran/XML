/*
 * See Copyright for the license status of this software.
 */

#ifndef RS_XML_H
#define RS_XML_H

#define RS_XML(a) RS_XML_##a

#ifdef _R_

#include "R.h"
#include "Rinternals.h"

#if 0
#if R_VERSION < R_Version(1, 2, 0)
#define STRING_ELT(x,i)   STRING(x)[i]
#define VECTOR_ELT(x,i)   VECTOR(x)[i]
#define SET_STRING_ELT(x,i,v)     (STRING(x)[i]=(v))
#define SET_VECTOR_ELT(x,i,v)     (VECTOR(x)[i]=(v))
#endif
#endif /* end of ignoring version details */

#endif /* end of _R_ */


#endif
