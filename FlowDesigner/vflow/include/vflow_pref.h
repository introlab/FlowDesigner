// Copyright (C) 2001 Jean-Marc Valin & Dominic Letourneau

#ifndef VFLOW_PREF_H
#define VFLOW_PREF_H


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
   void save();

   static bool isShowAll() {return pref.showAll;}
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
