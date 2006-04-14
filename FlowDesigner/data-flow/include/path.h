// Copyright (C) 2001 Jean-Marc Valin

#ifndef PATH_H
#define PATH_H

#include <vector>
#include <string>

namespace FD {

std::vector<std::string> envList(const char *envName, bool include_home=true);

extern "C" {
void scanDL(bool debug=false);
}

}
#endif
