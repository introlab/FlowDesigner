// Copyright (C) 2001 Jean-Marc Valin


#ifndef FLOW_VERSION_H
#define FLOW_VERSION_H

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif


int version_check(const char *vers);

static int dummy_version_check = version_check(OVERFLOW_VERSION);


#endif
