/*
 * See Copyright for the license status of this software.
 */

#ifndef UTILS_H
#define UTILS_H

#include "RS_XML.h"
#include "RSCommon.h"


int isBlank(const char *str);
char *trim(char *str);

USER_OBJECT_ RS_XML(invokeFunction)(USER_OBJECT_ fun, USER_OBJECT_ opArgs, USER_OBJECT_ state);
USER_OBJECT_ RS_XML(findFunction)(const char *opName, USER_OBJECT_ functions);

void RS_XML(SetNames)(int n, const char *cnames[], USER_OBJECT_ ans);
int RS_XML(SetClassName)(const char *name, USER_OBJECT_ target);

#ifndef SET_CLASS_NAME
#define SET_CLASS_NAME(localClassName, target)  RS_XML(SetClassName)((localClassName), (target))
#endif

#ifdef LIBXML2
#ifdef FROM_GNOME_XML_DIR
#include <gnome-xml/hash.h>
#else
#include <libxml/hash.h>
#endif

int xmlHashSize(xmlHashTablePtr table);
#endif

#endif

