#ifndef GRUNCONTEXT_H
#define GRUNCONTEXT_H

#include "UIDocument.h"
#include <string>
#include "ParameterSet.h"
#include <gnome.h>
#include "Network.h"


class GRunContext {
  protected:
   UIDocument *doc;
   ParameterSet &params;
   GtkWidget *win;
   GtkWidget *less;
   string less_text;
  public:
   GRunContext(UIDocument *_doc, ParameterSet &_params);
   
   void less_print(const string &message);
   
   void less_print(const char *message);
   
   void run();
   
};

#endif
