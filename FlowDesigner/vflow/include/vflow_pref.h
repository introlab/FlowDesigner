// Copyright (C) 2001 Jean-Marc Valin & Dominic Letourneau

#ifndef VFLOW_PREF_H
#define VFLOW_PREF_H

#include <map>
#include <string>

#include "flow_pref.h"

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
