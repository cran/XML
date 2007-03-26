#include "Utils.h"  /* For isBlank() */

#include <libxml/catalog.h>


USER_OBJECT_
R_xmlCatalogResolve(USER_OBJECT_ r_id, USER_OBJECT_ type, USER_OBJECT_ debug)
{
    char *id  = CHAR_DEREF(STRING_ELT(r_id, 0));
    SEXP r_ans = R_NilValue;
    xmlChar* ans;
    int debugLevel = -1;

    debugLevel = xmlCatalogSetDebug(LOGICAL(debug)[0]);

    switch(INTEGER(type)[0]) {
    case 1:
	ans = xmlCatalogResolveURI(id);
	break;
    case 2:
	ans = xmlCatalogResolvePublic(id);
	break;
    case 3:
	ans = xmlCatalogResolveSystem(id);
	break;
    default:
	break;
    }

    xmlCatalogSetDebug(debugLevel);

    if(ans) {
	r_ans = mkString(ans);
	xmlFree(ans);
    } else
	r_ans = NEW_CHARACTER(0);

    return(r_ans);
}


