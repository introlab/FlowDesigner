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
#include <string>
#include <fstream>
#include "iextensions.h"
#include <unistd.h>

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
   if (string(argv[1]) == "/dev/stdin")
   {
      if (fork())
	 _exit(0);
   }
   try {
      scanDL();
   } catch (BaseException *e)
   {
      e->print();
      delete e;
      exit(1);
   }
   UINodeRepository::Scan();
   IExtensions::detect();

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
   UIDocument *doc;
   /*if (string(argv[1]) == "/dev/stdin")
   {
      string docStr;
      doc = new UIDocument("/dev/stdin");
      while(1)
      {
	 char buff[1025];
	 cin.read(buff, 1024);
	 buff[1024]=0;
	 if (cin.fail())
	 {
	    docStr.append(buff, cin.gcount());
	    break; 
	 }
	 docStr.append(buff, 1024);
      }
      doc->loadFromMemory(docStr.c_str(), docStr.size());
      
      } else*/ {
      doc = new UIDocument(argv[1]);
      doc->load();
   }

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
