// Copyright (C) 2001 Jean-Marc Valin & Dominic Letourneau

#ifndef VFLOW_PREF_H
#define VFLOW_PREF_H


GtkWidget *create_propertybox1 ();


class VFlowPref {
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
   int errorCol;

   static VFlowPref pref;
  public:
   VFlowPref();
   ~VFlowPref();
   
   static bool isShowAll() {return pref.showAll;}
};



#endif
