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
#include <signal.h>
#include <libxml/parser.h>
#include "object_param.h"

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

using namespace std;
using namespace FD;

void quit()
{
   gdk_threads_enter(); 
   gtk_main_quit();
   gdk_threads_leave();
}


void run2(GRunContext *ctx)
{
   signal(11,SIG_DFL);
   ctx->run();
   //cerr << "End of run2()" << endl;
   //gtk_main_quit();
}


int main(int argc, char **argv)
{
   //xmlKeepBlanksDefault(0);

   if (argc < 2) {
     cout<<"Usage : "<<argv[0]<<" <document> [arguments]"<<endl;
     return -1;
   }

   if (string(argv[1]) == "/dev/stdin")
   {
      if (fork())
	 _exit(0);
   }

   g_thread_init(NULL);
   gdk_threads_init();
   gnome_init ("GFlow", VERSION, argc, argv);
   setlocale (LC_NUMERIC, "C");
   signal(11,SIG_DFL);

   try {
      scanDL();
   } catch (BaseException *e)
   {
      e->print();
      delete e;
      //exit(1);
   }

   UINodeRepository::Scan();
   IExtensions::detect();

   
   ParameterSet &params = *new ParameterSet;
   for (int arg = 2; arg<argc; arg++)
   {
      char arg_name[100];
      sprintf (arg_name, "ARG%d", arg-1);
      params.add(arg_name, ObjectRef (new String (argv[arg])));
      sprintf (arg_name, "string:ARG%d", arg-1);
      params.add(arg_name, ObjectRef (new String (argv[arg])));
      sprintf (arg_name, "int:ARG%d", arg-1);
      params.add(arg_name, ObjectRef (Int::alloc (atoi(argv[arg]))));
      sprintf (arg_name, "float:ARG%d", arg-1);
      params.add(arg_name, ObjectRef (Float::alloc (atof(argv[arg]))));
      if (strlen(argv[arg]) > 2 && argv[arg][0]=='<' && argv[arg][strlen(argv[arg])-1]=='>') {
	sprintf (arg_name, "object:ARG%d", arg-1);
	try {
	  string val(argv[arg]);
	  ParameterSet p;
	  ObjectRef obj = ObjectParam::stringParam("object", val, p);
	  if (!obj.isNil())
	     params.add(arg_name, obj);
	} catch (...) {}
      }
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
   
   /*
   pthread_attr_t tattr;
   pthread_attr_init(&tattr);
   pthread_attr_setdetachstate(&tattr,PTHREAD_CREATE_DETACHED);
   */

   ctx->set_thread(&runThread);

   //pthread_create(&runThread, &tattr, (void * (*)(void *))run2, (void *) ctx);
   pthread_create(&runThread, NULL, (void * (*)(void *))run2, (void *) ctx);

   gdk_threads_enter(); 
   gtk_main ();
   gdk_threads_leave();
   return 0;

}
