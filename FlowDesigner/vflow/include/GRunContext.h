// Copyright (C) 2001 Jean-Marc Valin

#ifndef GRUNCONTEXT_H
#define GRUNCONTEXT_H

#include "UIDocument.h"
#include <string>
#include "ParameterSet.h"
#include <gnome.h>
#include "Network.h"


class GRunContext {

  friend gboolean delete_window (GtkWidget *widget, GdkEvent *event, GRunContext *my_context);

  protected:
   UIDocument *doc;
   ParameterSet &params;
   GtkWidget *win;
   GtkWidget *less;
   string less_text;
   Network *net;

  public:
   GRunContext(UIDocument *_doc, ParameterSet &_params);
   
   void less_print(const string &message);
   
   void less_print(const char *message);
   
   void run();
   
};

#endif
