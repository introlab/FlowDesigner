// Copyright (C) 2001 Jean-Marc Valin & Dominic Letourneau

#ifndef VFLOW_PREF_H
#define VFLOW_PREF_H

#include <map>
#include <string>

class VFlowPref {
   int modified;
   map<string,string> params;

   static VFlowPref pref;
  public:
   VFlowPref();
   ~VFlowPref();
   void load();
   void save();

   static bool getBool(const string &str);
   static void setBool(const string &str, bool val);

   static guint32 getColor(const string &str);
   static void setColor(const string &str, guint32 col);

   static void Save();
};


class VFlowPrefDialog {
   GtkWidget *propertybox1;
   GtkWidget *printout;
   GtkWidget *runprocess;
   GtkWidget *showallio;
   GtkWidget *showtooltip;
   GtkWidget *colorpicker1;
   GtkWidget *colorpicker2;
   GtkWidget *colorpicker3;
  public:
   VFlowPrefDialog();
   ~VFlowPrefDialog();
   void apply();
   void close();
};


#endif
