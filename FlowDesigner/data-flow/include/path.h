// Copyright (C) 2001 Jean-Marc Valin

#ifndef PATH_H
#define PATH_H

using namespace std;

#include <vector>
#include <string>

vector<string> envList(char *envName, bool include_home=true);

extern "C" {
void scanDL(bool debug=false);
}

#endif
