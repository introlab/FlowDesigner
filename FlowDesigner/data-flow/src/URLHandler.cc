
// Copyright (C) 2004 Dominic Letourneau

#include "URLHandler.h"
#include <string>
#include <map>
#include "Object.h"
#include "net_types.h"
#include <sstream>
#include <fstream>

//@implements core

using namespace std;

map<string,url_func>  &URLHandler::url_table()
{
  static map<string,url_func> table;
  return table;
}


ObjectRef file_url_handler(const string& url, int flags) {

  //FORMAT file:/home/..

  int _pos = url.find(":");
  
  string stripped_url = url.substr(_pos + 1, url.size());

  switch(flags) {

  case URLHandler::URL_READ:
    return ObjectRef(new IStream(new ifstream(stripped_url.c_str())));
    break;

  case URLHandler::URL_WRITE:
    return ObjectRef(new OStream(new ofstream(stripped_url.c_str())));
    break;

  case URLHandler::URL_READWRITE:
    return ObjectRef(new IOStream(new fstream(stripped_url.c_str())));
    break;

  default:
    ostringstream my_stream;
    my_stream<<"Unknown flag "<<flags<<" in file_url_handler";
    throw new GeneralException(my_stream.str(),__FILE__,__LINE__);
    break;
  }

  return nilObject;
}
REGISTER_URL_HANDLER(file,file_url_handler);
