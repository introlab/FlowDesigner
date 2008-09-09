#ifndef _URLHANDLER_H_
#define _URLHANDLER_H_

// Copyright (C) 2004 Dominic Letourneau

#include "Object.h"
#include <string>
#include <map>
#include "BaseException.h"
#include "net_types.h"

namespace FD {
	
	typedef ObjectRef (*url_func)(const std::string &url, int flags);
	
	class URLHandler {
		
	private:
		static std::map<std::string,url_func> &url_table();
		
	public:
		enum {
			URL_READ,
			URL_WRITE,
			URL_READWRITE
		};
		
		static int RegisterURLHandler(const std::string& name, url_func func) 
		{
			url_table()[name] = func;
			return 0;
		}
		
		static ObjectRef openStream(const std::string &inputURL, int flags)
		{
			
			try 
			{      
				size_t pos = inputURL.find(":");
				
				if (pos == std::string::npos) 
				{
					//default URL type is file: if nothing specified
					return url_table()["file"](inputURL, flags);
				}
				else
				{
					//cerr<<"tring to get url type : "<<inputURL.substr(0,pos)<<endl;
					
					if (url_table().find(inputURL.substr(0,pos)) != url_table().end())
					{
						return url_table()[inputURL.substr(0,pos)](inputURL, flags);
					}
					else
					{
						throw new GeneralException(std::string("Unable to create URL of type : ") + inputURL,__FILE__,__LINE__);
					}
				}
			}
			catch(BaseException *e) 
			{
				throw e->add(new GeneralException(std::string("Unable to create URL of type : ") + inputURL,__FILE__,__LINE__));
			}
		}   
	};
	
	
#define REGISTER_URL_HANDLER(name, func) \
int dummy_url_handler_for ## _ ## name = \
URLHandler::RegisterURLHandler(# name,func);
	
}//using namespace FD

#endif
