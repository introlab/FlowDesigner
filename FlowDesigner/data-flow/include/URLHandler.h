#ifndef _URLHANDLER_H_
#define _URLHANDLER_H_

// Copyright (C) 2004 Dominic Letourneau

#include "Object.h"
#include <string>
#include <map>
#include "BaseException.h"
#include "net_types.h"

typedef ObjectRef (*url_func)(const string &url, int flags);

class URLHandler {
  
 private:
  static map<string,url_func> &url_table();
  
 public:
  enum {
    URL_READ,
    URL_WRITE,
    URL_READWRITE
  };

  static int RegisterURLHandler(const string& name, url_func func) 
  {
    url_table()[name] = func;
    return 0;
  }
  
  static ObjectRef openStream(const string &inputURL, int flags)
  {

    try 
    {      
      int pos = inputURL.find(":");

      if (pos == string::npos) 
      {
	//default URL type is FILE:// if nothing specified
	return url_table()["file"](inputURL, flags);
      }
      else
      {
	if (url_table().find(inputURL.substr(0,pos - 1)) != url_table().end())
	{
	  return url_table()[inputURL.substr(0,pos - 1)](inputURL, flags);
        }
	else
	{
	  throw new GeneralException(string("Unable to create URL of type :") + inputURL,__FILE__,__LINE__);
	}
      }
    }
    catch(BaseException *e) 
    {
      throw e->add(new GeneralException(string("Unable to create URL of type :") + inputURL,__FILE__,__LINE__));
    }
  }   
};


#define REGISTER_URL_HANDLER(name, func) \
  int dummy_url_handler_for ## _ ## name = \
    URLHandler::RegisterURLHandler(# name,func);

#endif
