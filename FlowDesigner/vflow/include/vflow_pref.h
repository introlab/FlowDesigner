// Copyright (C) 2001 Jean-Marc Valin & Dominic Letourneau

#ifndef VFLOW_PREF_H
#define VFLOW_PREF_H

#include <map>
#include <string>

class VFlowPref {
     /*   
	  //General
	  bool showAll;
	  bool showTool;
	  bool printOut;
	  bool runProcess;
	  //Documents
	  int mdiMode;
	  int tabPos;
	  //Colors
	  int regularCol;
	  int highliteCol;
	  int errorCol;*/
   int modified;
   map<string,string> params;

   static VFlowPref pref;
  public:
   VFlowPref();
   ~VFlowPref();
   void load();
   void save();

   static bool getBool(const string &str);
   //static bool isShowAll() {return pref.showAll;}
};


class VFlowPrefDialog {
   GtkWidget *propertybox1;
  public:
   VFlowPrefDialog();
   ~VFlowPrefDialog();
   void apply();
   void close();
};


#endif
