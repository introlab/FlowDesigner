// Copyright (C) 2001 Jean-Marc Valin & Dominic Letourneau

#ifndef FLOW_PREF_H
#define FLOW_PREF_H

#include <map>
#include <string>



class FlowPref {
   int modified;
   std::map<std::string, std::map<std::string,std::string> > params;

   static FlowPref pref;
  public:
   FlowPref();
   ~FlowPref();
   void load();
   void save();

   static bool getBool(const std::string &cat, const std::string &str);
   static void setBool(const std::string &cat, const std::string &str, bool val);

   static unsigned int getColor(const std::string &cat, const std::string &str);
   static void setColor(const std::string &cat, const std::string &str, unsigned int col);

   static void Save();
};


#endif
