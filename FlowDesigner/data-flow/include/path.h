// Copyright (C) 2001 Jean-Marc Valin

#ifndef PATH_H
#define PATH_H

#include <vector>
#include <string>

namespace FD {

std::vector<std::string> envList(const char *envName, bool include_home=true);

/**
	RecursiveScanDL will scan the path and children directories for .tlb files and return the list of files in libList
*/
void recursiveScanDL(const std::string &path, std::vector<std::string> &libList, bool debug);

//extern "C" {
void scanDL(bool debug=false);
//}

}
#endif
