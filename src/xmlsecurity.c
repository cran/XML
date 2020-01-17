#ifdef HAVE_LIBXMLSEC
#include <xmlsec/xmlsec.h>
#include <xmlsec/app.h>
#include <xmlsec/crypto.h>

#include "Rinternals.h"

void
R_xmlSecCryptoInit(int *els)
{
    int status;

    els[0] = status = xmlSecCryptoInit();
    if(status != 0)
	return;

    els[1] = status = xmlSecCryptoAppInit(NULL);
    if(status != 0)
	return;

    els[2] = status = xmlSecCryptoInit();
}

SEXP
R_xmlSecCryptoShutdown()
{
    int status;
    status = xmlSecCryptoShutdown();
    return(ScalarInteger(status));
}
#else
// avoid a warning about an empty translation unit.
// instead, this gives one about an unused variable!
// static int foo;
void R_xmlSecCryptoInit(int *els) {}
#endif
