// Copyright (C) 2001 Jean-Marc Valin & Dominic Letourneau

#ifndef FLOW_PREF_H
#define FLOW_PREF_H

#include <map>
#include <string>

class FlowPref {
   int modified;
   map<string, map<string,string> > params;

   static FlowPref pref;
  public:
   FlowPref();
   ~FlowPref();
   void load();
   void save();

   static bool getBool(const string &cat, const string &str);
   static void setBool(const string &cat, const string &str, bool val);

   static unsigned int getColor(const string &cat, const string &str);
   static void setColor(const string &cat, const string &str, unsigned int col);

   static void Save();
};


#endif
