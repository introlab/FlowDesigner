// Copyright (C) 2001 Jean-Marc Valin

#ifndef GRUNCONTEXT_H
#define GRUNCONTEXT_H

#include "UIDocument.h"
#include <string>
#include "ParameterSet.h"
#include <gnome.h>
#include "Network.h"
#include <pthread.h>

class GRunContext {

  friend gboolean delete_window (GtkWidget *widget, GdkEvent *event, GRunContext *my_context);

  protected:

   pthread_t *running_thread;
   pthread_mutex_t del_lock;
   UIDocument *doc;
   ParameterSet &params;
   GtkWidget *win;
   GtkWidget *less;
   std::string less_text;
   Network *net;

  public:
   GRunContext(UIDocument *_doc, ParameterSet &_params);
   
   void less_print(const std::string &message);
   
   void less_print(const char *message);
   
   void run();

   void set_thread(pthread_t *thread);
   
};

#endif
