// Copyright (C) 2001 Jean-Marc Valin



#include "GRunContext.h"
#include <sstream>
#include "vflow_pref.h"
#include <unistd.h>

gboolean delete_window (GtkWidget *widget, GdkEvent *event, GRunContext *my_context) {
  
  if (my_context->net) {
    
    alarm(5);

    //stopping processing thread
    if (my_context->running_thread) {
      cerr<<"Stopping processing thread"<<endl;
      pthread_cancel(*(my_context->running_thread));
    }

    cerr<<"Stopping GTK"<<endl;
    gtk_main_quit();
    gdk_threads_leave();

    cerr<<"Deleting network..."<<endl;
    if(my_context->net) {
      delete my_context->net;
    }

    cerr<<"Terminating program"<<endl;
    exit(0);
  } 
  else  {
    exit(0);
  }
  
  return TRUE;
}

void GRunContext::set_thread(pthread_t *thread) {
   running_thread = thread;
}

GRunContext::GRunContext(UIDocument *_doc, ParameterSet &_params)
  : doc(_doc) , params(_params) , net(NULL), running_thread(NULL)
{
   //cerr << "opening...\n";
   win = gtk_window_new (GTK_WINDOW_TOPLEVEL);
   gtk_signal_connect (GTK_OBJECT (win), "delete-event", GTK_SIGNAL_FUNC(delete_window), this);

   gtk_window_set_title (GTK_WINDOW (win), _("GFlow"));
	 
   GtkWidget *vbox = gtk_vbox_new (FALSE, 0);
   gtk_widget_ref (vbox);
   gtk_object_set_data_full (GTK_OBJECT (win), "vbox", vbox,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_container_add (GTK_CONTAINER (win), vbox);
   gtk_widget_show(vbox);
	 
   less = gnome_less_new ();
   gtk_widget_set_usize(less, -1, -1);
	 
   gtk_widget_ref (less);
   gtk_object_set_data_full (GTK_OBJECT (win), "less", less,
			     (GtkDestroyNotify) gtk_widget_unref);
	 
   gtk_widget_show (less);
   gtk_box_pack_start (GTK_BOX (vbox), less, TRUE, TRUE, 0);
   //gtk_container_add (GTK_CONTAINER (vbox), less);
	 
	 
   gtk_widget_show(win);
   //gnome_less_show_string(GNOME_LESS(less),"Running network...\n");
	 
}

void GRunContext::less_print(const string &message) 
{ 
   less_text += message + string("\n");
	 
   if (less) {
      gnome_less_show_string(GNOME_LESS(less),less_text.c_str());
   } 
}

void GRunContext::less_print(const char *message) 
{ 
   less_text += string(message) + string("\n");
	 
   if (less) {
      gnome_less_show_string(GNOME_LESS(less),less_text.c_str());
   } 
}

void GRunContext::run()
{
   try {
      gdk_threads_enter();
      less_print("Running network...");
      gdk_threads_leave();
      net = doc->build("MAIN", params);
      if (net->getInputNode())
	 throw new GeneralException ("main network has input node", __FILE__, __LINE__);
      cerr << "initializing...\n";
      net->initialize();
      cerr << "running (UIDocument)...\n";
	    
      for (int i = 0; ;i++) {
	 if (!net->hasOutput(i)) break;
	 if (FlowPref::getBool("VFLOW", "PrintOutput"))
	 {
	    stringstream execOut;
	    execOut << *net->getOutput(i,0);
	    gdk_threads_enter();
	    less_print(execOut.str());
	    gdk_threads_leave();
	 } else {
	    net->getOutput(i,0);
	 }
      }
      delete net;
      net = NULL;
      gdk_threads_enter();
      less_print("Network ended normally");
      gdk_threads_leave();
	    
   } catch (BaseException *e)
   {
      //cerr << "exception\n";
      stringstream excOut;
	    
      e->print (excOut);
      gdk_threads_enter();
      less_print(excOut.str());
      gdk_threads_leave();
	    
      delete e;
   } catch (...)
   {
      gdk_threads_enter();
      less_print("Unknown exception caught");
      gdk_threads_leave();
   }
}
