#include "UIDocument.h"
#include <string>
#include "ParameterSet.h"
#include "ObjectRef.h"
#include <pthread.h>
#include <gnome.h>
#include <locale.h>
#include "Network.h"

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif


class Args {
   public:
      int argc;
      char **argv;
};

void run(Args *arg)
{
   int argc=arg->argc;
   char **argv=arg->argv;
   ParameterSet param;
   for (int arg = 2; arg<argc; arg++)
   {
      char arg_name[100];
      sprintf (arg_name, "ARG%d", arg-1);
      param.add(arg_name, ObjectRef (new String (argv[arg])));
   }
   UIDocument *doc = new UIDocument(argv[1]);
   doc->load();
   
   Network *net = doc->build("MAIN", param);
   if (net->getInputNode())
      throw new GeneralException ("main network has input node", __FILE__, __LINE__);
   //cerr << "initializing...\n";
   net->initialize();
   //cerr << "running (UIDocument)...\n";
   
   for (int i = 0; ;i++) {
      if (!net->hasOutput(i)) break;
      cout << *net->getOutput(i,0);
   }
   delete net;
   gdk_threads_enter(); 
   gtk_main_quit();
   gdk_threads_leave(); 	 
}

int main(int argc, char **argv)
{
   try {
   UIDocument::scanDL();
   } catch (BaseException *e)
   {
      e->print();
      delete e;
      exit(1);
   }
   UIDocument::loadAllInfo();

   g_thread_init(NULL);
   gnome_init ("vflow", VERSION, argc, argv);
   setlocale (LC_NUMERIC, "C");

   Args arg;
   arg.argc = argc;
   arg.argv = argv;

   pthread_t runThread;
   pthread_attr_t tattr;
   pthread_attr_init(&tattr);
   pthread_attr_setdetachstate(&tattr,PTHREAD_CREATE_DETACHED);
   pthread_create(&runThread, &tattr, (void * (*)(void *))run, (void *) &arg);
   
   //run(arg);
   gdk_threads_enter(); 
   gtk_main ();
   gdk_threads_leave(); 
   return 0;

}
