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
      UIDocument *doc;
      ParameterSet *params;
};

void run(Args *arg)
{
   /*UIDocument *doc = arg->doc;
   ParameterSet &params=*(arg->params);
   
   Network *net = doc->build("MAIN", params);
   if (net->getInputNode())
      throw new GeneralException ("main network has input node", __FILE__, __LINE__);
   //cerr << "initializing...\n";
   net->initialize();
   //cerr << "running (UIDocument)...\n";
   
   for (int i = 0; ;i++) {
      if (!net->hasOutput(i)) break;
      cout << *net->getOutput(i,0);
   }
   delete net;*/
   gdk_threads_enter(); 
   gtk_main_quit();
   gdk_threads_leave(); 	 
}


class Args2 {
   public:
      UIDocument *doc;
      ParameterSet *params;
};

void run2(Args *arg)
{
   UIDocument *doc = arg->doc;
   ParameterSet &params=*(arg->params);
   
   Network *net = doc->build("MAIN", params);
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

   _exit(0);
   //cerr << "tata1\n";
   gdk_threads_enter(); 
   //cerr << "tata2\n";
   gtk_main_quit();
   //cerr << "tata3\n";
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

   ParameterSet params;
   for (int arg = 2; arg<argc; arg++)
   {
      char arg_name[100];
      sprintf (arg_name, "ARG%d", arg-1);
      params.add(arg_name, ObjectRef (new String (argv[arg])));
   }
   UIDocument *doc = new UIDocument(argv[1]);
   doc->load();

   bool withGUI=true;

   if (withGUI)
   {
      Args2 arg;
      arg.doc = doc;
      arg.params = &params;
      
      //cerr << "opening...\n";
      GtkWidget *win = gtk_window_new (GTK_WINDOW_TOPLEVEL);
      gtk_window_set_title (GTK_WINDOW (win), _("GFlow"));

      GtkWidget *vbox = gtk_vbox_new (FALSE, 0);
      gtk_widget_ref (vbox);
      gtk_object_set_data_full (GTK_OBJECT (win), "vbox", vbox,
				(GtkDestroyNotify) gtk_widget_unref);
      gtk_container_add (GTK_CONTAINER (win), vbox);
      gtk_widget_show(vbox);

      GtkWidget *less = gnome_less_new ();
      gtk_widget_set_usize(less, -1, -1);

      gtk_widget_ref (less);
      gtk_object_set_data_full (GTK_OBJECT (win), "less", less,
				(GtkDestroyNotify) gtk_widget_unref);
      
      gtk_widget_show (less);
      gtk_box_pack_start (GTK_BOX (vbox), less, TRUE, TRUE, 0);
      //gtk_container_add (GTK_CONTAINER (vbox), less);


      gtk_widget_show(win);
      gnome_less_show_string(GNOME_LESS(less),"Running network...\n");

      pthread_t runThread;
      pthread_attr_t tattr;
      pthread_attr_init(&tattr);
      pthread_attr_setdetachstate(&tattr,PTHREAD_CREATE_DETACHED);
      pthread_create(&runThread, &tattr, (void * (*)(void *))run2, (void *) &arg);
      
   } else {
      Args arg;
      arg.doc = doc;
      arg.params = &params;
      
      pthread_t runThread;
      pthread_attr_t tattr;
      pthread_attr_init(&tattr);
      pthread_attr_setdetachstate(&tattr,PTHREAD_CREATE_DETACHED);
      pthread_create(&runThread, &tattr, (void * (*)(void *))run, (void *) &arg);
   }

   gdk_threads_enter(); 
   //cerr << "tataA\n";
   gtk_main ();
   //cerr << "tataB\n";
   gdk_threads_leave(); 
   return 0;

}
