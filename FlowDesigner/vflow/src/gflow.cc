// Copyright (C) 2001 Jean-Marc Valin

#include <locale.h>
#include "UIDocument.h"
#include <string>
#include "ParameterSet.h"
#include "ObjectRef.h"
#include <pthread.h>
#include <gnome.h>
#include "Network.h"
#include <sstream>
#include "GRunContext.h"
#include "path.h"

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif


void quit()
{
   gdk_threads_enter(); 
   gtk_main_quit();
   gdk_threads_leave();
}


void run2(GRunContext *ctx)
{
   ctx->run();
   //gtk_main_quit();
}


int main(int argc, char **argv)
{
   try {
      scanDL();
   } catch (BaseException *e)
   {
      e->print();
      delete e;
      exit(1);
   }
   UINodeRepository::Scan();
   //UIDocument::loadAllInfo();

   g_thread_init(NULL);
   gnome_init ("vflow", VERSION, argc, argv);
   setlocale (LC_NUMERIC, "C");

   ParameterSet &params = *new ParameterSet;
   for (int arg = 2; arg<argc; arg++)
   {
      char arg_name[100];
      sprintf (arg_name, "ARG%d", arg-1);
      params.add(arg_name, ObjectRef (new String (argv[arg])));
   }
   UIDocument *doc = new UIDocument(argv[1]);
   doc->load();

   GRunContext *ctx = new GRunContext(doc, params);
   
   
   pthread_t runThread;
   pthread_attr_t tattr;
   pthread_attr_init(&tattr);
   pthread_attr_setdetachstate(&tattr,PTHREAD_CREATE_DETACHED);
   pthread_create(&runThread, &tattr, (void * (*)(void *))run2, (void *) ctx);
   
   gdk_threads_enter(); 
   gtk_main ();
   gdk_threads_leave();
   return 0;

}
