// Copyright (C) 2001 Jean-Marc Valin

#ifndef PATH_H
#define PATH_H



#include <vector>
#include <string>

std::vector<std::string> envList(char *envName, bool include_home=true);

extern "C" {
void scanDL(bool debug=false);
}

#endif
