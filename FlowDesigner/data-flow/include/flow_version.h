// Copyright (C) 2001 Jean-Marc Valin


#ifndef FLOW_VERSION_H
#define FLOW_VERSION_H

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdlib.h>

#define OVERFLOW_ABI_VERSION "dev-2002-01-22"

extern "C" {
int version_check(const char *vers, const char *abi_vers, const char *unused_vers=NULL);
}

static int dummy_version_check = version_check(OVERFLOW_VERSION, OVERFLOW_ABI_VERSION);


#endif
