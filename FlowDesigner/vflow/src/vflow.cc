// Copyright (C) 2001 Jean-Marc Valin & Dominic Letourneau

#include "vflow.h"
#include "iextensions.h"
#include <list>
#include <sstream>
#include "vflow_pref.h"
#include "flow_version.h"
#include <libxml/parser.h>
#include "misc_gui.h"
#include "GUIDocument.h"
#include "GUINetwork.h"
#include "GUILink.h"
#include "GUINode.h"

void GUIDocument_codegen(GUIDocument *doc);

vflowGUI* vflowGUI::vflowApp = NULL;


const int vflowGUI::CLIPBOARD_COPY_MODE = 0;
const int vflowGUI::CLIPBOARD_CUT_MODE = 1;
const int vflowGUI::CLIPBOARD_NONE_MODE = 2;

#define GLADE_HOOKUP_OBJECT(component,widget,name) \
  g_object_set_data_full (G_OBJECT (component), name, \
    gtk_widget_ref (widget), (GDestroyNotify) gtk_widget_unref)

#define GLADE_HOOKUP_OBJECT_NO_REF(component,widget,name) \
  g_object_set_data (G_OBJECT (component), name, widget)



/**********************************************************************************************************

**********************************************************************************************************/
vflowGUI* vflowGUI::instance() {

  if (!vflowApp) {
    vflowApp = new vflowGUI();
  }

  return vflowApp;
}

/**********************************************************************************************************

**********************************************************************************************************/
vflowGUI::vflowGUI() {

  create_mdi();

}

/**********************************************************************************************************

**********************************************************************************************************/
vflowGUI::~vflowGUI() {


}

GUIDocument *vflowGUI::getCurrentDoc()
{
   int curr = gtk_notebook_get_current_page (GTK_NOTEBOOK(notebook1));
   if (curr==-1)
   {
      return NULL;
   } else {

     cerr<<"Current page of the notebook : "<<curr<<endl;
      GtkWidget *page = gtk_notebook_get_nth_page (GTK_NOTEBOOK(notebook1), curr);
      return (GUIDocument*)gtk_object_get_data(GTK_OBJECT(page), "doc");
   }
}

/**********************************************************************************************************

**********************************************************************************************************/
void vflowGUI::create_empty_document() {

  new_doc_event(NULL,this);

}
/**********************************************************************************************************

**********************************************************************************************************/
void vflowGUI::load_document (const string fname) {

  GUIDocument *doc = new GUIDocument(fname);

  doc->setFullPath(fname);
  try {
     doc->load();
  } catch (BaseException *e) {
     stringstream except;
     e->print(except);
     doc->less_print (except.str());
  } catch (...) {
     doc->less_print ("unknown exception caught while loading document");
  }
  doc->resetModified();
}

/**********************************************************************************************************

**********************************************************************************************************/
void vflowGUI::copy(GUIDocument *doc) {
  //cerr<<"vflowGUI::copy called"<<endl;

  GUINetwork *net = dynamic_cast<GUINetwork*>(doc->getCurrentNet());
  
  if (net) {

    clipboard = net->getSelectedNodes();
    clipboardMode = CLIPBOARD_COPY_MODE;
    //cerr<<"clipboard size : "<<clipboard.size()<<endl;
  }
  

}
/**********************************************************************************************************

**********************************************************************************************************/
void vflowGUI::paste (GUIDocument *doc) {

  //cerr<<"vflowGUI::paste called"<<endl;
  GUINetwork *net = dynamic_cast<GUINetwork*>(doc->getCurrentNet());
  

  if (!net) return;

  map<GUINode*, GUINode*> created_nodes;

  //emptying selected nodes
  net->emptySelectedNodes();

  double x1,y1,x2,y2;
  double delta_x = 10;
  double delta_y = 10;
  int c1,c2;


  net->get_scroll_region(x1,y1,x2,y2);                                 
  //printf("scroll region (%f,%f) : (%f,%f)\n",x1,y1,x2,y2);

  net->get_scroll_offsets(c1,c2);   
  //printf("scroll offsets (%i, %i)\n",c1,c2);

  //copying
  for (list<GUINode*>::iterator iter = clipboard.begin();
       iter != clipboard.end(); iter++) {
    
    double x, y;

    (*iter)->getPos(x,y);

    string type = (*iter)->getType();
    
    //creating new node
    GUINode *my_node = dynamic_cast<GUINode*>(net->addNode(type,x + delta_x, y + delta_y));

    //new node aliases
    created_nodes.insert(make_pair((*iter),my_node));    

    //copying parameters
    UINodeParameters *params_source = (*iter)->getParameters();
    
    UINodeParameters *params_destination = new UINodeParameters(my_node,(*iter)->getType());
    
    params_destination->copyParameterText(params_source);
    
    my_node->setNodeParameters(params_destination);

    //unselecting old node
    (*iter)->unselect();

    //selecting newly created node
    my_node->select();

    //adding the new node to the selected list
    net->addSelectedNode(my_node);

    //remove the copied node from the selected list
    GUINetwork *source_network = dynamic_cast<GUINetwork*>((*iter)->getNetwork());
    
    if (source_network != net && source_network != NULL) {
      source_network->removeSelectedNode(*iter);
    }
    
  }
  
  //relinking (only selected nodes)
  for (list<GUINode*>::iterator iter = clipboard.begin();
       iter != clipboard.end(); iter++) {
    
    //getting terminals
    vector<UITerminal*> inputTerminals = (*iter)->getInputs();

    //getting links
    for (int i = 0; i < inputTerminals.size(); i++) {

      vector<UILink*> links = inputTerminals[i]->getConnections();

      for (int j = 0; j < links.size(); j++) {

	UITerminal *from_terminal = links[j]->getFromTerminal();
	UITerminal *to_terminal = links[j]->getToTerminal();

	GUINode *from_node = dynamic_cast<GUINode*>(from_terminal->getNode());
	GUINode *to_node = dynamic_cast<GUINode*>(to_terminal->getNode());


	if (created_nodes.find(from_node) != created_nodes.end() &&
	    created_nodes.find(to_node) != created_nodes.end()) {
	  //link between selected nodes

	 UILink *my_link =  created_nodes[from_node]->newLink(
				       created_nodes[from_node]->getOutputNamed(from_terminal->getName()),
				       created_nodes[to_node]->getInputNamed(to_terminal->getName()));

	 
	 while (!my_link->get_link_points().empty()) {
	   GUILinkPoint *p1 = my_link->get_link_points().front();
	   my_link->get_link_points().pop_front();
	   delete p1;
	   
	 }

	 //todo : reshape link
	 list<GUILinkPoint*> &p_list = links[j]->get_link_points();

	 for (list<GUILinkPoint*>::iterator p_iter = p_list.begin();
	      p_iter != p_list.end(); p_iter++) {
	   //copying points
	   my_link->get_link_points().push_back(new GUILinkPoint((*p_iter)->x + delta_x, (*p_iter)->y + delta_y));
	 }

	 dynamic_cast<GUILink*>(my_link)->update();

	}
      }

    }
  }


  if (clipboardMode == CLIPBOARD_CUT_MODE) {   
    //should destroy selected nodes from source document
    while(!clipboard.empty()) {
      GUINode *my_node = clipboard.front();
      clipboard.pop_front();
      delete my_node;
    }
  }

  if (clipboardMode == CLIPBOARD_COPY_MODE) {

    clipboard.resize(0);
    //clipboard should contain newly copied nodes.
    for (map<GUINode*,GUINode*>::iterator iter = created_nodes.begin();
	 iter != created_nodes.end(); iter++) {
      clipboard.push_back((*iter).second);
    }
  }

}
/**********************************************************************************************************

**********************************************************************************************************/
void vflowGUI::cut(GUIDocument *doc) {
  //cerr<<"vflowGUI::cut called"<<endl;

  GUINetwork *net = dynamic_cast<GUINetwork*>(doc->getCurrentNet());
  
  if (net) {
    clipboard = net->getSelectedNodes();
    clipboardMode = CLIPBOARD_CUT_MODE;
    //cerr<<"clipboard size : "<<clipboard.size()<<endl;    
  }
}
/**********************************************************************************************************

**********************************************************************************************************/
void vflowGUI::clear(GUIDocument *doc) {
  //cerr<<"vflowGUI::clear called"<<endl;
  if (doc) {

    GUINetwork *net = dynamic_cast<GUINetwork*>(doc->getCurrentNet());

    if (net) {
      net->clearSelectedNodes();
    }

    clipboard.resize(0);
    
    clipboardMode = CLIPBOARD_NONE_MODE;
  }
}

/**********************************************************************************************************

**********************************************************************************************************/
static gboolean delete_app(GtkWidget *widget, GdkEvent *event, vflowGUI *appPtr)
{

  exit_event(NULL, appPtr);
  return TRUE;
}


/**********************************************************************************************************

**********************************************************************************************************/
void new_doc_event  (GtkMenuItem *menuitem, vflowGUI *vflow) {

   GUIDocument *doc = new GUIDocument("Untitled");

   doc->addNetwork("MAIN", UINetwork::subnet);
   doc->resetModified();
   
   
}


/**********************************************************************************************************

**********************************************************************************************************/
void file_open_ok_sel(GtkWidget *w, vflowGUI *vflow) {


  //until user_data (2nd arg works)
  vflow = vflowGUI::instance();

   GtkWidget *ssel = GTK_WIDGET(gtk_object_get_user_data(GTK_OBJECT(w)));
   gchar *fname = g_strdup(gtk_file_selection_get_filename (GTK_FILE_SELECTION(ssel)));
	
   if (fname) {
      GUIDocument *doc = new GUIDocument("Untitled");

      doc->setFullPath(fname);
      try {
	 doc->load();
      } catch (BaseException *e) {
	 stringstream except;
	 e->print(except);
	 doc->less_print (except.str());
      } catch (...) {
	 doc->less_print ("unknown exception caught while loading document");
      }
   }
   g_free (fname);
   gtk_widget_destroy (GTK_WIDGET (ssel));
   ssel = NULL;
} 


/**********************************************************************************************************

**********************************************************************************************************/
gint file_open_destroy(GtkWidget *button, GtkWidget *sel) {

  gtk_widget_destroy(sel);

  return TRUE; 
}

/**********************************************************************************************************

**********************************************************************************************************/
void open_doc_event (GtkMenuItem *menuitem, vflowGUI *vflow) {

  //until user_data (2nd arg works)
  vflow = vflowGUI::instance();

  GtkWidget *ssel = gtk_file_selection_new("Open file...");
  
  gtk_object_set_user_data(GTK_OBJECT(GTK_FILE_SELECTION(ssel)->ok_button),ssel);

   gtk_signal_connect(GTK_OBJECT(GTK_FILE_SELECTION(ssel)->ok_button),
                      "clicked", (GtkSignalFunc)file_open_ok_sel, 
                      vflow);
    
   gtk_signal_connect(GTK_OBJECT(GTK_FILE_SELECTION(ssel)->cancel_button),
                      "clicked", (GtkSignalFunc)file_open_destroy, 
                      ssel);
      
   gtk_widget_show(ssel);

}

/**********************************************************************************************************

**********************************************************************************************************/
gint close_doc_event (GtkMenuItem *menuitem, vflowGUI *vflow) {
   
   GUIDocument *doc = vflowGUI::instance()->getCurrentDoc();
   
   if (!doc)
      return -1;
   if (!doc->isModified())
   {
      delete doc;
      return 1;
   }

   string question("Do you want to save changes to ");
   question += doc->getName();
   question += "?";
   
   switch (close_save_dialog(question.c_str()))
   {
   case GTK_RESPONSE_CANCEL:
      return 0;
      break;
   case GTK_RESPONSE_NO:
      delete doc;
      return 1;      
      break;
   case GTK_RESPONSE_ACCEPT:
      //doc->save();
      save_doc_event(NULL, vflow);
      delete doc;
      return 1;
      break;
   }
   //doc->closeRequest();
}


/**********************************************************************************************************

**********************************************************************************************************/
void file_saveas_ok_sel(GtkWidget *w, vflowGUI *vflow) {


   GUIDocument *doc = vflowGUI::instance()->getCurrentDoc();
   
   if (!doc)
      return;

   GtkWidget *ssel = GTK_WIDGET(gtk_object_get_user_data(GTK_OBJECT(w)));

   gchar *fname = g_strdup(gtk_file_selection_get_filename (GTK_FILE_SELECTION(ssel)));
   
   if (fname) {

      doc->setFullPath(fname);
      doc->save();
      doc->resetModified();
      
   }
   
   g_free (fname);
   gtk_widget_destroy (GTK_WIDGET (ssel));
   ssel = NULL;
   
} 

/**********************************************************************************************************

**********************************************************************************************************/
gint file_saveas_destroy(GtkWidget *w, GtkWidget *sel) {

  gtk_widget_destroy(sel);
  
   
  return TRUE;
   
}

/**********************************************************************************************************

**********************************************************************************************************/
void saveas_doc_event(GtkWidget *widget, vflowGUI *vflow) {

   gchar *title = NULL;


   //until user_data (2nd arg works)
   GUIDocument *doc = vflowGUI::instance()->getCurrentDoc();
   if (!doc) 
   {
      return;
   }

   title = g_strdup_printf (_("Save %s As..."), (gchar*) doc->getName().c_str());
   
   GtkWidget *ssel = gtk_file_selection_new(title);
   
   gtk_object_set_user_data(GTK_OBJECT(GTK_FILE_SELECTION(ssel)->ok_button),ssel);

   gtk_signal_connect(GTK_OBJECT(GTK_FILE_SELECTION(ssel)->ok_button),
                      "clicked", (GtkSignalFunc)file_saveas_ok_sel, 
                      vflow);
   
   gtk_signal_connect(GTK_OBJECT(GTK_FILE_SELECTION(ssel)->cancel_button),
                      "clicked", (GtkSignalFunc)file_saveas_destroy, 
                      ssel);
   
   gtk_widget_show(ssel);
   
   g_free (title);

}

/**********************************************************************************************************

**********************************************************************************************************/
void save_doc_event(GtkWidget *widget, vflowGUI *vflow) 
{
   GUIDocument *doc = vflowGUI::instance()->getCurrentDoc();
   if (!doc) 
   {
      return;
   }

   if (doc->isUntitled()) {

      saveas_doc_event(widget, vflow);
      return;
   }

   if (doc->isModified())
      doc->save();

   doc->resetModified();
}


/**********************************************************************************************************

**********************************************************************************************************/
static void on_cut_activate (GtkMenuItem *menuitem, gpointer user_data) 
{

   GUIDocument *doc = vflowGUI::instance()->getCurrentDoc();
   if (!doc) 
   {
      return;
   }
   //calling application cut
   vflowGUI::instance()->cut(doc);
}

/**********************************************************************************************************

**********************************************************************************************************/
static void on_copy_activate (GtkMenuItem *menuitem, gpointer user_data) 
{

   GUIDocument *doc = vflowGUI::instance()->getCurrentDoc();
   if (!doc) 
   {
      return;
   }
   //calling application copy
   vflowGUI::instance()->copy(doc);
}
/**********************************************************************************************************

**********************************************************************************************************/
static void on_paste_activate (GtkMenuItem *menuitem, gpointer user_data) 
{
   GUIDocument *doc = vflowGUI::instance()->getCurrentDoc();
   if (!doc) 
   {
      return;
   }

   //calling application paste
   vflowGUI::instance()->paste(doc);

}
/**********************************************************************************************************

**********************************************************************************************************/
static void on_clear_activate (GtkMenuItem *menuitem, gpointer user_data) 
{
   GUIDocument *doc = vflowGUI::instance()->getCurrentDoc();
   if (!doc) 
   {
      return;
   }

  GUINetwork *net = dynamic_cast<GUINetwork*>(doc->getCurrentNet());

  if (!net)
  {
     return;
  }
  //calling application clear
  vflowGUI::instance()->clear(doc);

}

/**********************************************************************************************************

**********************************************************************************************************/
static int net_create_id=0;
static int iter_create_id=1;
static int threaded_create_id=2;

static void add_net_event  (GtkMenuItem     *menuitem,
                            gpointer         user_data)
{
   GUIDocument *doc = vflowGUI::instance()->getCurrentDoc();
   if (!doc) 
   {
      return;
   }

   char *ask_phrase;
   string default_str;
   UINetwork::Type net_type;
   switch (*(int*)user_data)
   {
   case 0:
      ask_phrase="What's the name of the network?";
      default_str = doc->getNewNetName(UINetwork::subnet);
      net_type = UINetwork::subnet;
      break;
   case 1:
      ask_phrase="What's the name of the iterator?";
      default_str = doc->getNewNetName(UINetwork::iterator);
      net_type = UINetwork::iterator;
      break;
   case 2:
      ask_phrase="What's the name of the threaded iterator?";
      default_str = doc->getNewNetName(UINetwork::threaded);
      net_type = UINetwork::threaded;
      break;
   }

   string str = ask_string_dialog(ask_phrase, default_str.c_str());
   if (str != "")
   {
      try {
         doc->addNetwork(str, net_type);
      }
      catch (BaseException *e) {
         
         stringstream str;
         e->print(str);
         GtkWidget*  dialog = gnome_warning_dialog (str.str().c_str());
         delete e;
      }  
   }
}


/**********************************************************************************************************

**********************************************************************************************************/
static void rename_net_event  (GtkMenuItem     *menuitem,
                            gpointer         user_data)
{

   GUIDocument *doc = vflowGUI::instance()->getCurrentDoc();
   if (!doc) 
   {
      return;
   }

   doc->renameCurrentNet();
   //gtk_notebook_get_current_page (GTK_NOTEBOOK(notebook1));
   //doc
 //cerr << "remove net\n";
}
/**********************************************************************************************************

**********************************************************************************************************/
static void remove_net_event  (GtkMenuItem     *menuitem,
                            gpointer         user_data)
{

   GUIDocument *doc = vflowGUI::instance()->getCurrentDoc();
   if (!doc) 
   {
      return;
   }

   doc->removeCurrentNet();
   //gtk_notebook_get_current_page (GTK_NOTEBOOK(notebook1));
   //doc
 //cerr << "remove net\n";
}



/**********************************************************************************************************

**********************************************************************************************************/
void vflowGUI::set_run_mode (bool isRuning) 
{
   gtk_widget_set_sensitive(button5,!isRuning);
   gtk_widget_set_sensitive(button6,isRuning);
}
/**********************************************************************************************************

**********************************************************************************************************/
#include <stdio.h>

void run_doc_event(GtkWidget *widget, vflowGUI *vflow) {


   GUIDocument *doc = vflowGUI::instance()->getCurrentDoc();
   if (!doc) 
   {
      return;
   }

   if (FlowPref::getBool("VFLOW", "RunProcess"))
   {
      int size;
      char *mem = doc->saveToMemory(size);
      FILE *pipe = popen("cat | gflow /dev/stdin", "w");
      fwrite(mem, 1, size, pipe);
      fprintf(pipe,"\n");
      fflush(pipe);
      pclose(pipe);
      //FIXME: delete mem
   } else {
      vflow->set_run_mode(true);
      doc->threadRun();
   }

}

/**********************************************************************************************************

**********************************************************************************************************/
void doc_prop_event(GtkWidget *widget, vflowGUI *vflow)  
{
   GUIDocument *doc = vflowGUI::instance()->getCurrentDoc();
   if (!doc) 
   {
      return;
   }
   doc->showParams();

}

/**********************************************************************************************************

**********************************************************************************************************/
void stop_doc_event(GtkWidget *widget, vflowGUI *vflow)  
{
  
  //until user_data (2nd arg works)
  vflow = vflowGUI::instance();

   //cerr<<"Stopping the thread!"<<endl; 
   GUIDocument::threadStop();
}

/**********************************************************************************************************

**********************************************************************************************************/
void build_event  (GtkMenuItem *menuitem, vflowGUI *vflow) 
{

  GUIDocument *doc = vflowGUI::instance()->getCurrentDoc();
   if (!doc) 
   {
      return;
   }

   GUIDocument_codegen(doc);

}
/**********************************************************************************************************

**********************************************************************************************************/
void exit_event  (GtkMenuItem  *menuitem, vflowGUI *vflow) 
{
   /*vflow = vflowGUI::instance();
   
   GUIDocument *doc;
   while (doc = vflow->getCurrentDoc())
   {
      if (!doc->closeRequest())
         return;
   }
   */

   while (1)
   {
      int close = close_doc_event(NULL, vflow);
      if (close==0)
         return;
      else if (close==-1)
         break;
   }

   gtk_main_quit();
}
/**********************************************************************************************************
(strange) child set to NULL?
**********************************************************************************************************/
gint remove_doc_cb (GnomeMDI *mdi, GnomeMDIChild *child) {

   GnomeMessageBox *msgbox;
   int ret, *ptr;
   char *fname;
   
   vflowGUI *vflow = vflowGUI::instance();
   /*
   GUIDocument *doc = (GUIDocument*)gtk_object_get_data(GTK_OBJECT(child), "doc");
   string msg = " '" + doc->getName() + "' has been modified. Do you wish to save it?";

   if (vflow->mdi->active_view) {
	
      if (doc->isModified()) {

         msgbox = GNOME_MESSAGE_BOX (gnome_message_box_new (
                                                            msg.c_str(),
                                                            GNOME_MESSAGE_BOX_QUESTION,
                                                            GNOME_STOCK_BUTTON_YES,
                                                            GNOME_STOCK_BUTTON_NO,
                                                            GNOME_STOCK_BUTTON_CANCEL,
                                                            NULL));
	    							
         gnome_dialog_set_default (GNOME_DIALOG (msgbox), 2);
         ret = gnome_dialog_run_and_close (GNOME_DIALOG (msgbox));
	    	    
         switch (ret) {
	    
         case 0:
	 {
	    if (doc->isUntitled())
	    {
	       return FALSE;
	    }
	    if (doc->isModified())
	       doc->save();
	    doc->resetModified();
	 }
         case 1:
	    delete doc;
            return TRUE;
	    
         default:
            return FALSE;
         }

      }
          
   }
   */	
   return TRUE;
}
/**********************************************************************************************************

**********************************************************************************************************/
void overflow_doc_event(GtkMenuItem *menuitem, vflowGUI *vflow) 
{
   gnome_url_show("http://freespeech.sourceforge.net/FreeSpeech/html/Overflow/user-guide.html", NULL);
}
/**********************************************************************************************************

**********************************************************************************************************/
void overflow_noderef_event(GtkMenuItem *menuitem, vflowGUI *vflow) 
{
   gnome_url_show("http://freespeech.sourceforge.net/FreeSpeech/html/Overflow/nodes.html",NULL);
}

/**********************************************************************************************************

**********************************************************************************************************/
void on_preferences1_activate  (GtkMenuItem *menuitem, vflowGUI *vflow) 
{
   new VFlowPrefDialog;
   //create_propertybox1();
}
/**********************************************************************************************************

**********************************************************************************************************/
void about_event  (GtkMenuItem *menuitem, vflowGUI *vflow) 
{
   const gchar *authors[] = {
      "Jean-Marc Valin (jean-marc.valin@hermes.usherbrooke.ca)", 
      "Dominic Letourneau (dominic.letourneau@usherbrooke.ca)",
      NULL
   };
  const gchar *documenters[] = { NULL };
  /* TRANSLATORS: Replace this string with your names, one name per line. */
  gchar *translators = _("translator_credits");
  GtkWidget *about2;

  if (!strcmp (translators, "translator_credits"))
    translators = NULL;
  about2 = gnome_about_new ("VFlow", VERSION,
                        _("(C) 1999-2003 Jean-Marc Valin & Dominic Letourneau"),
                        _("Overflow (http://freespeech.sourceforge.net/overflow.html) is a free (LGPL) \"data flow oriented\" development environment. It can be use to build complex applications by combining small, reusable building blocks. In some way, it has similarities with Simulink and LabView, although it is not designed (and far) to be a \"clone\" of any of them. This software is part of the Open Mind Speech project (http://freespeech.sourceforge.net/)."),
                        authors,
                        documenters,
                        translators,
                        NULL);
  gtk_window_set_destroy_with_parent (GTK_WINDOW (about2), TRUE);

  /* Store pointers to all widgets, for use by lookup_widget(). */
  GLADE_HOOKUP_OBJECT_NO_REF (about2, about2, "about2");

  gtk_window_set_transient_for(GTK_WINDOW(about2), GTK_WINDOW(vflowGUI::instance()->get_mdi()));
  gtk_window_set_position (GTK_WINDOW(about2), GTK_WIN_POS_CENTER_ON_PARENT);

  gtk_widget_show(about2);
}


/**********************************************************************************************************

**********************************************************************************************************/
void on_segfault(int sig) {

   static pthread_t last;
   static bool in_segfault=false;
   if (in_segfault)
   {
      cerr << "nested segfaults caught (that's nasty!)\n";
      abort();
   }
   in_segfault=true;
   if (pthread_equal (GUIDocument::runThread, pthread_self()))
   {
      if (pthread_equal (GUIDocument::runThread, last))
      {
	 cerr << "same thread segfaulted twice... something's wrong" << endl;
	 abort();
      }
      last = GUIDocument::runThread;
      cerr << "segfault caught in user program\n";
      gdk_threads_enter();
      gnome_error_dialog("The user program caused a segmentation violation.\nMemory corruption might have occured, so it is recommended\nto restart Overflow (unless you know what you're doing).");
      gdk_threads_leave();
      in_segfault=false;
      pthread_exit(NULL);
   } else {
      cerr << "Overflow just crashed... you should have saved :-(\n";
      gnome_error_dialog("Overflow just crashed... you should have saved :-(");
      abort();
   }
   in_segfault=false;
}

void on_gchld(int sig)
{
   //cerr << "Child died\n";
}

/**********************************************************************************************************

**********************************************************************************************************/
void vflowGUI::create_mdi () 
{
static GnomeUIInfo file1_menu_uiinfo[] =
{
  GNOMEUIINFO_MENU_NEW_ITEM (N_("_New"), NULL, new_doc_event, NULL),
  GNOMEUIINFO_MENU_OPEN_ITEM (open_doc_event, NULL),
  GNOMEUIINFO_MENU_SAVE_ITEM (save_doc_event, NULL),
  GNOMEUIINFO_MENU_SAVE_AS_ITEM (saveas_doc_event, NULL),
  GNOMEUIINFO_MENU_CLOSE_ITEM (close_doc_event, NULL),
  GNOMEUIINFO_SEPARATOR,
  GNOMEUIINFO_MENU_EXIT_ITEM (exit_event, NULL),
  GNOMEUIINFO_END
};

static GnomeUIInfo edit1_menu_uiinfo[] =
{
  GNOMEUIINFO_MENU_CUT_ITEM (on_cut_activate, NULL),
  GNOMEUIINFO_MENU_COPY_ITEM (on_copy_activate, NULL),
  GNOMEUIINFO_MENU_PASTE_ITEM (on_paste_activate, NULL),
  GNOMEUIINFO_MENU_CLEAR_ITEM (on_clear_activate, NULL),
  GNOMEUIINFO_SEPARATOR,
  GNOMEUIINFO_MENU_PROPERTIES_ITEM (doc_prop_event, NULL),
  GNOMEUIINFO_SEPARATOR,
  GNOMEUIINFO_MENU_PREFERENCES_ITEM (on_preferences1_activate, NULL),
   
  GNOMEUIINFO_END
};

static GnomeUIInfo networks_menu_uiinfo[] =
{
   
  {
    GNOME_APP_UI_ITEM, N_("Add _Network"),
    NULL,
    (gpointer) add_net_event, &net_create_id, NULL,
    GNOME_APP_PIXMAP_NONE, NULL,
    0, (GdkModifierType) 0, NULL
  },
  {
    GNOME_APP_UI_ITEM, N_("Add _Iterator"),
    NULL,
    (gpointer) add_net_event, &iter_create_id, NULL,
    GNOME_APP_PIXMAP_NONE, NULL,
    0, (GdkModifierType) 0, NULL
  },
  {
    GNOME_APP_UI_ITEM, N_("Add _Threaded Iterator"),
    NULL,
    (gpointer) add_net_event, &threaded_create_id, NULL,
    GNOME_APP_PIXMAP_NONE, NULL,
    0, (GdkModifierType) 0, NULL
  },
  GNOMEUIINFO_SEPARATOR,
  {
    GNOME_APP_UI_ITEM, N_("Rename Network"),
    NULL,
    (gpointer) rename_net_event, NULL, NULL,
    GNOME_APP_PIXMAP_NONE, NULL,
    0, (GdkModifierType) 0, NULL
  },
  GNOMEUIINFO_SEPARATOR,
  {
    GNOME_APP_UI_ITEM, N_("Remove Network"),
    NULL,
    (gpointer) remove_net_event, NULL, NULL,
    GNOME_APP_PIXMAP_NONE, NULL,
    0, (GdkModifierType) 0, NULL
  },
   
  GNOMEUIINFO_END
};

static GnomeUIInfo view1_uiinfo[] =
{
  GNOMEUIINFO_END
};

static GnomeUIInfo help1_menu_uiinfo[] =
{
  {
    GNOME_APP_UI_ITEM, N_("User Guide"),
    NULL,
    (gpointer) overflow_doc_event, NULL, NULL,
    GNOME_APP_PIXMAP_NONE, NULL,
    0, (GdkModifierType) 0, NULL
  },
  {
    GNOME_APP_UI_ITEM, N_("Node Reference"),
    NULL,
    (gpointer) overflow_noderef_event, NULL, NULL,
    GNOME_APP_PIXMAP_NONE, NULL,
    0, (GdkModifierType) 0, NULL
  },
  GNOMEUIINFO_SEPARATOR,
  GNOMEUIINFO_MENU_ABOUT_ITEM (about_event, NULL),
  GNOMEUIINFO_END
};

static GnomeUIInfo menubar1_uiinfo[] =
{
  GNOMEUIINFO_MENU_FILE_TREE (file1_menu_uiinfo),
  GNOMEUIINFO_MENU_EDIT_TREE (edit1_menu_uiinfo),
  {
    GNOME_APP_UI_SUBTREE, N_("_Networks"),
    NULL,
    networks_menu_uiinfo, NULL, NULL,
    GNOME_APP_PIXMAP_NONE, NULL,
    0, (GdkModifierType) 0, NULL
  },
  GNOMEUIINFO_MENU_VIEW_TREE (view1_uiinfo),
  GNOMEUIINFO_MENU_HELP_TREE (help1_menu_uiinfo),
  GNOMEUIINFO_END
};

  GtkWidget *app1;
  GtkWidget *bonobodock1;
  GtkWidget *toolbar1;
  GtkWidget *button1;
  GtkWidget *button2;
  GtkWidget *button3;
  GtkWidget *vseparator1;
  GtkWidget *button4;
//GtkWidget *button5;
// GtkWidget *button6;
  GtkWidget *button7;
  GtkWidget *vseparator2;
  GtkWidget *button9;
//GtkWidget *notebook1;
  GtkWidget *empty_notebook_page;
  GtkWidget *label1;
  GtkWidget *appbar1;

  app1 = gnome_app_new ("VFlow", _("VFlow"));

  //call the delete_app function when closing window
  gtk_signal_connect (GTK_OBJECT (app1), "delete-event",GTK_SIGNAL_FUNC (delete_app), this);



  bonobodock1 = GNOME_APP (app1)->dock;
  gtk_widget_show (bonobodock1);

  gnome_app_create_menus (GNOME_APP (app1), menubar1_uiinfo);

  toolbar1 = gtk_toolbar_new ();
  gtk_widget_show (toolbar1);
  gnome_app_add_toolbar (GNOME_APP (app1), GTK_TOOLBAR (toolbar1), "toolbar1",
                                BONOBO_DOCK_ITEM_BEH_EXCLUSIVE,
                                BONOBO_DOCK_TOP, 1, 0, 0);
  gtk_container_set_border_width (GTK_CONTAINER (toolbar1), 1);
  gtk_toolbar_set_style (GTK_TOOLBAR (toolbar1), GTK_TOOLBAR_BOTH);

  button1 = gtk_toolbar_insert_stock (GTK_TOOLBAR (toolbar1),
                                "gtk-new",
                                _("New"),
                                NULL, NULL, NULL, -1);

gtk_widget_show (button1);
gtk_signal_connect (GTK_OBJECT (button1), "clicked",
                    GTK_SIGNAL_FUNC (new_doc_event),
                       this);

  button2 = gtk_toolbar_insert_stock (GTK_TOOLBAR (toolbar1),
                                "gtk-open",
                                _("Open"),
                                NULL, NULL, NULL, -1);
  gtk_widget_show (button2);
  gtk_signal_connect (GTK_OBJECT (button2), "clicked",
                      GTK_SIGNAL_FUNC (open_doc_event),
                      this);

  button3 = gtk_toolbar_insert_stock (GTK_TOOLBAR (toolbar1),
                                "gtk-save",
                                _("Save"),
                                NULL, NULL, NULL, -1);
  gtk_widget_show (button3);
  gtk_widget_show (button2);
  gtk_signal_connect (GTK_OBJECT (button3), "clicked",
                      GTK_SIGNAL_FUNC (save_doc_event),
                      this);

  vseparator1 = gtk_vseparator_new ();
  gtk_widget_show (vseparator1);
  gtk_toolbar_append_widget (GTK_TOOLBAR (toolbar1), vseparator1, NULL, NULL);

  button4 = gtk_toolbar_insert_stock (GTK_TOOLBAR (toolbar1),
                                "gtk-properties",
                                _("Properties"),
                                NULL, NULL, NULL, -1);
  gtk_widget_show (button4);
  gtk_signal_connect (GTK_OBJECT (button4), "clicked",
                      GTK_SIGNAL_FUNC (doc_prop_event),
                      this);

  button5 = gtk_toolbar_insert_stock (GTK_TOOLBAR (toolbar1),
                                "gtk-execute",
                                _("Run"),
                                NULL, NULL, NULL, -1);
  gtk_widget_show (button5);
  gtk_signal_connect (GTK_OBJECT (button5), "clicked",
                      GTK_SIGNAL_FUNC (run_doc_event),
                      this);

  button6 = gtk_toolbar_insert_stock (GTK_TOOLBAR (toolbar1),
                                "gtk-stop",
                                _("Stop"),
                                NULL, NULL, NULL, -1);
  gtk_widget_show (button6);
  gtk_signal_connect (GTK_OBJECT (button6), "clicked",
                      GTK_SIGNAL_FUNC (stop_doc_event),
                      this);

  button7 = gtk_toolbar_insert_stock (GTK_TOOLBAR (toolbar1),
                                "gtk-convert",
                                _("Build"),
                                NULL, NULL, NULL, -1);
  gtk_widget_show (button7);
  gtk_signal_connect (GTK_OBJECT (button7), "clicked",
                      GTK_SIGNAL_FUNC (build_event),
                      this);

  vseparator2 = gtk_vseparator_new ();
  gtk_widget_show (vseparator2);
  gtk_toolbar_append_widget (GTK_TOOLBAR (toolbar1), vseparator2, NULL, NULL);

  button9 = gtk_toolbar_insert_stock (GTK_TOOLBAR (toolbar1),
                                "gtk-quit",
                                _("Quit"),
                                NULL, NULL, NULL, -1);
  gtk_widget_show (button9);
  gtk_signal_connect (GTK_OBJECT (button9), "clicked",
                      GTK_SIGNAL_FUNC (exit_event),
                      this);

  notebook1 = gtk_notebook_new ();
  gtk_widget_show (notebook1);
  gnome_app_set_contents (GNOME_APP (app1), notebook1);
/*
  empty_notebook_page = gtk_vbox_new (FALSE, 0);
  gtk_widget_show (empty_notebook_page);
  gtk_container_add (GTK_CONTAINER (notebook1), empty_notebook_page);

  label1 = gtk_label_new (_("label1"));
  gtk_widget_show (label1);
  gtk_notebook_set_tab_label (GTK_NOTEBOOK (notebook1), gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook1), 0), label1);
  gtk_label_set_justify (GTK_LABEL (label1), GTK_JUSTIFY_LEFT);
*/

  appbar1 = gnome_appbar_new (TRUE, TRUE, GNOME_PREFERENCES_NEVER);
  gtk_widget_show (appbar1);
  gnome_app_set_statusbar (GNOME_APP (app1), appbar1);

  gnome_app_install_menu_hints (GNOME_APP (app1), menubar1_uiinfo);

  /* Store pointers to all widgets, for use by lookup_widget(). */
  GLADE_HOOKUP_OBJECT_NO_REF (app1, app1, "app1");
  GLADE_HOOKUP_OBJECT (app1, bonobodock1, "bonobodock1");
  GLADE_HOOKUP_OBJECT (app1, menubar1_uiinfo[0].widget, "file1");
  GLADE_HOOKUP_OBJECT (app1, file1_menu_uiinfo[0].widget, "new1");
  GLADE_HOOKUP_OBJECT (app1, file1_menu_uiinfo[1].widget, "open1");
  GLADE_HOOKUP_OBJECT (app1, file1_menu_uiinfo[2].widget, "save1");
  GLADE_HOOKUP_OBJECT (app1, file1_menu_uiinfo[3].widget, "save_as1");
  GLADE_HOOKUP_OBJECT (app1, file1_menu_uiinfo[4].widget, "fermer1");
  GLADE_HOOKUP_OBJECT (app1, file1_menu_uiinfo[5].widget, "separator1");
  GLADE_HOOKUP_OBJECT (app1, file1_menu_uiinfo[6].widget, "quit1");
  GLADE_HOOKUP_OBJECT (app1, menubar1_uiinfo[1].widget, "edit1");
GLADE_HOOKUP_OBJECT (app1, edit1_menu_uiinfo[0].widget, "cut1");
  GLADE_HOOKUP_OBJECT (app1, edit1_menu_uiinfo[1].widget, "copy1");
  GLADE_HOOKUP_OBJECT (app1, edit1_menu_uiinfo[2].widget, "paste1");
  GLADE_HOOKUP_OBJECT (app1, edit1_menu_uiinfo[3].widget, "clear1");
  GLADE_HOOKUP_OBJECT (app1, edit1_menu_uiinfo[4].widget, "separator2");
GLADE_HOOKUP_OBJECT (app1, edit1_menu_uiinfo[5].widget, "properties1");
  GLADE_HOOKUP_OBJECT (app1, edit1_menu_uiinfo[6].widget, "separator3");
  GLADE_HOOKUP_OBJECT (app1, edit1_menu_uiinfo[7].widget, "preferences1");
  GLADE_HOOKUP_OBJECT (app1, menubar1_uiinfo[2].widget, "networks");
  GLADE_HOOKUP_OBJECT (app1, networks_menu_uiinfo[0].widget, "add_network1");
  GLADE_HOOKUP_OBJECT (app1, networks_menu_uiinfo[1].widget, "add_iterator1");
  GLADE_HOOKUP_OBJECT (app1, networks_menu_uiinfo[2].widget, "add_threaded_iterator1");
  GLADE_HOOKUP_OBJECT (app1, networks_menu_uiinfo[3].widget, "separator4");
  GLADE_HOOKUP_OBJECT (app1, networks_menu_uiinfo[4].widget, "rename_network1");
  GLADE_HOOKUP_OBJECT (app1, networks_menu_uiinfo[5].widget, "separator5");
  GLADE_HOOKUP_OBJECT (app1, networks_menu_uiinfo[6].widget, "remove_network1");
  GLADE_HOOKUP_OBJECT (app1, menubar1_uiinfo[3].widget, "view1");
  GLADE_HOOKUP_OBJECT (app1, menubar1_uiinfo[4].widget, "help1");
  GLADE_HOOKUP_OBJECT (app1, help1_menu_uiinfo[0].widget, "user_guide1");
  GLADE_HOOKUP_OBJECT (app1, help1_menu_uiinfo[1].widget, "node_reference1");
  GLADE_HOOKUP_OBJECT (app1, help1_menu_uiinfo[2].widget, "s__parateur1");
  GLADE_HOOKUP_OBJECT (app1, help1_menu_uiinfo[3].widget, "about1");

  GLADE_HOOKUP_OBJECT (app1, toolbar1, "toolbar1");
  GLADE_HOOKUP_OBJECT (app1, button1, "button1");
  GLADE_HOOKUP_OBJECT (app1, button2, "button2");
  GLADE_HOOKUP_OBJECT (app1, button3, "button3");
  GLADE_HOOKUP_OBJECT (app1, vseparator1, "vseparator1");
  GLADE_HOOKUP_OBJECT (app1, button4, "button4");
  GLADE_HOOKUP_OBJECT (app1, button5, "button5");
  GLADE_HOOKUP_OBJECT (app1, button6, "button6");
  GLADE_HOOKUP_OBJECT (app1, button7, "button7");
  GLADE_HOOKUP_OBJECT (app1, vseparator2, "vseparator2");
  GLADE_HOOKUP_OBJECT (app1, button9, "button9");
  GLADE_HOOKUP_OBJECT (app1, notebook1, "notebook1");
//GLADE_HOOKUP_OBJECT (app1, label1, "label1");
  GLADE_HOOKUP_OBJECT (app1, appbar1, "appbar1");

//return app1;
gtk_widget_show (app1);
mdi=app1;

set_run_mode(false);
}



int main (int argc, char *argv[]) 
{
   xmlKeepBlanksDefault(0);
   try {
      
      IExtensions::detect();
      scanDL();
      UINodeRepository::Scan();

      g_thread_init(NULL); 
      gdk_threads_init();

      gnome_program_init ("VFlow", OVERFLOW_VERSION, LIBGNOMEUI_MODULE,
                          argc, argv,
                          GNOME_PARAM_APP_DATADIR, PACKAGE_DATA_DIR,
	                  NULL);
      //gnome_init ("vflow", OVERFLOW_VERSION, argc, argv);
      setlocale (LC_NUMERIC, "C");

      //setting segfault callback
      signal(SIGSEGV,on_segfault);
      //signal(SIGCHLD,on_gchld);
      
      //cerr<<"creating vflow instance"<<endl;
      vflowGUI *vflow = vflowGUI::instance();
      
      if (argc > 1) {
	 for (int i=1; i<argc; i++) {
	   vflow->load_document(argv[i]);
	 }
      } 
      else {
	vflow->create_empty_document();
      }
      

      //starting GTK loop
      gdk_threads_enter(); 
      gtk_main ();
      gdk_threads_leave(); 

   }
   catch (BaseException *e) {
      e->print();
      delete e;
      exit(1);
   } catch (...) {
      cerr << "Unhandled exception\n"; 
      exit(1);
   }
   
   return 0;
}

