// Copyright (C) 2001 Jean-Marc Valin & Dominic Letourneau

#include "vflow.h"
#include "iextensions.h"
#include <list>
#include <sstream>
#include "vflow_pref.h"

void GUIDocument_codegen(GUIDocument *doc);

vflowGUI* vflowGUI::vflowApp = NULL;


const int vflowGUI::CLIPBOARD_COPY_MODE = 0;
const int vflowGUI::CLIPBOARD_CUT_MODE = 1;
const int vflowGUI::CLIPBOARD_NONE_MODE = 2;

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

/**********************************************************************************************************

**********************************************************************************************************/
void vflowGUI::create_empty_document() {

  new_doc_event(NULL,this);

}
/**********************************************************************************************************

**********************************************************************************************************/
void vflowGUI::load_document (const string fname) {

  GUIDocument *doc = new GUIDocument("Untitled");

  gnome_mdi_add_child (mdi, GNOME_MDI_CHILD (doc->getMDIChild()));
  gnome_mdi_add_view  (mdi, GNOME_MDI_CHILD (doc->getMDIChild()));

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


void vflowGUI::copy(GUIDocument *doc) {
  //cerr<<"vflowGUI::copy called"<<endl;

  GUINetwork *net = dynamic_cast<GUINetwork*>(doc->getCurrentNet());
  
  if (net) {

    clipboard = net->getSelectedNodes();
    clipboardMode = CLIPBOARD_COPY_MODE;
    //cerr<<"clipboard size : "<<clipboard.size()<<endl;
  }
  

}

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

void vflowGUI::cut(GUIDocument *doc) {
  //cerr<<"vflowGUI::cut called"<<endl;

  GUINetwork *net = dynamic_cast<GUINetwork*>(doc->getCurrentNet());
  
  if (net) {
    clipboard = net->getSelectedNodes();
    clipboardMode = CLIPBOARD_CUT_MODE;
    //cerr<<"clipboard size : "<<clipboard.size()<<endl;    
  }
}

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
void new_doc_event  (GtkMenuItem *menuitem, vflowGUI *vflow) {

   //until user_data (2nd arg works)
   vflow = vflowGUI::instance();

   GUIDocument *doc = new GUIDocument("Untitled");
   gnome_mdi_add_child (vflow->mdi, GNOME_MDI_CHILD (doc->getMDIChild()));
   gnome_mdi_add_view  (vflow->mdi, GNOME_MDI_CHILD (doc->getMDIChild()));
   
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
      gnome_mdi_add_child (vflow->mdi, GNOME_MDI_CHILD (doc->getMDIChild()));
      gnome_mdi_add_view  (vflow->mdi, GNOME_MDI_CHILD (doc->getMDIChild()));
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
void close_doc_event (GtkMenuItem *menuitem, vflowGUI *vflow) {
   
  //until user_data (2nd arg works)
  vflow = vflowGUI::instance();
   
   if (vflow->mdi->active_child == NULL)
      return;
   
   if (gnome_mdi_remove_child (vflow->mdi, vflow->mdi->active_child, FALSE)) {
      
      if (vflow->mdi->active_child == NULL) {
         
         /*if (!settings->close_doc) {
	    
            doc = UIDocument_new ();
            gnome_mdi_add_child (vflow->mdi, GNOME_MDI_CHILD (doc));
            gnome_mdi_add_view (vflow->mdi, GNOME_MDI_CHILD (doc));
	    
         } else {
	    
            g_print ("Er.. Unimplemented alternative!\n");
	    
            }*/
         
      }
      
   }
   
}


/**********************************************************************************************************

**********************************************************************************************************/
void file_saveas_ok_sel(GtkWidget *w, vflowGUI *vflow) {


  //until user_data (2nd arg works)
  vflow = vflowGUI::instance();

   GtkWidget *ssel = GTK_WIDGET(gtk_object_get_user_data(GTK_OBJECT(w)));

   gchar *fname = g_strdup(gtk_file_selection_get_filename (GTK_FILE_SELECTION(ssel)));
   
   if (vflow->mdi->active_child == NULL) {
      
      g_free (fname);
      return;
   }		  	
   
   GUIDocument *doc = (GUIDocument*) gtk_object_get_data(GTK_OBJECT(vflow->mdi->active_child), "doc");

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
   vflow = vflowGUI::instance();


   if (vflow->mdi->active_child == NULL) {
      return;
   }

   GUIDocument *doc = (GUIDocument*)gtk_object_get_data(GTK_OBJECT(vflow->mdi->active_child), "doc");

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
void save_doc_event(GtkWidget *widget, vflowGUI *vflow) {

  //until user_data (2nd arg works)
  vflow = vflowGUI::instance();

   if (vflow->mdi->active_child == NULL) {
      return;
   }

   GUIDocument *doc = (GUIDocument*)gtk_object_get_data(GTK_OBJECT(vflow->mdi->active_child), "doc");

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
void vflowGUI::set_run_mode (bool isRuning) {

   GtkWidget *w = gnome_mdi_get_toolbar_info (gnome_mdi_get_active_window(mdi))[5].widget;
   gtk_widget_set_sensitive(w,!isRuning);

   w = gnome_mdi_get_toolbar_info (gnome_mdi_get_active_window(mdi))[6].widget;
   gtk_widget_set_sensitive(w,isRuning);

}
/**********************************************************************************************************

**********************************************************************************************************/
#include <stdio.h>

void run_doc_event(GtkWidget *widget, vflowGUI *vflow) {

  //until user_data (2nd arg works)
  vflow = vflowGUI::instance();


   if (vflow->mdi->active_child == NULL)
      return;

   GUIDocument *doc = (GUIDocument*)gtk_object_get_data(GTK_OBJECT(vflow->mdi->active_child), "doc");

   if (FlowPref::getBool("VFLOW", "RunProcess"))
   {
      int size;
      char *mem = doc->saveToMemory(size);
      FILE *pipe = popen("gflow /dev/stdin", "w");
      fwrite(mem, 1, size, pipe);
      pclose(pipe);
   } else {
      vflow->set_run_mode(true);
      doc->threadRun();
   }

}

/**********************************************************************************************************

**********************************************************************************************************/
void doc_prop_event(GtkWidget *widget, vflowGUI *vflow)  {

  //until user_data (2nd arg works)
  vflow = vflowGUI::instance();


   if (vflow->mdi->active_child == NULL)
      return;

   GUIDocument *doc = (GUIDocument*)gtk_object_get_data(GTK_OBJECT(vflow->mdi->active_child), "doc");
   
   doc->showParams();
}

/**********************************************************************************************************

**********************************************************************************************************/
void stop_doc_event(GtkWidget *widget, vflowGUI *vflow)  {
  
  //until user_data (2nd arg works)
  vflow = vflowGUI::instance();

   //cerr<<"Stopping the thread!"<<endl; 
   GUIDocument::threadStop();
}

/**********************************************************************************************************

**********************************************************************************************************/
void build_event  (GtkMenuItem *menuitem, vflowGUI *vflow) {

  //until user_data (2nd arg works)
  vflow = vflowGUI::instance();

   if (vflow->mdi->active_child == NULL)
      return;

   GUIDocument *doc = (GUIDocument*)gtk_object_get_data(GTK_OBJECT(vflow->mdi->active_child), "doc");

   GUIDocument_codegen(doc);
}
/**********************************************************************************************************

**********************************************************************************************************/
void exit_event  (GtkMenuItem  *menuitem, vflowGUI *vflow) {

  //until user_data (2nd arg works)
  vflow = vflowGUI::instance();


   if (gnome_mdi_remove_all(vflow->mdi,FALSE)) {
      gtk_object_destroy (GTK_OBJECT (vflow->mdi));
   } else {
     return;
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
	
   return TRUE;
}

/**********************************************************************************************************

**********************************************************************************************************/
void overflow_doc_event(GtkMenuItem *menuitem, vflowGUI *vflow) {
  //until user_data (2nd arg works)
  vflow = vflowGUI::instance();

   gnome_url_show("http://freespeech.sourceforge.net/FreeSpeech/html/Overflow/user-guide.html");
}

/**********************************************************************************************************

**********************************************************************************************************/
void overflow_noderef_event(GtkMenuItem *menuitem, vflowGUI *vflow) {

  //until user_data (2nd arg works)
  vflow = vflowGUI::instance();

   gnome_url_show("http://freespeech.sourceforge.net/FreeSpeech/html/Overflow/nodes.html");
}


void on_preferences1_activate  (GtkMenuItem *menuitem, vflowGUI *vflow) 
{
   new VFlowPrefDialog;
   //create_propertybox1();
}

/**********************************************************************************************************


**********************************************************************************************************/
void about_event  (GtkMenuItem *menuitem, vflowGUI *vflow) {

  //until user_data (2nd arg works)
  vflow = vflowGUI::instance();
   
   const gchar *authors[] = {
      "Jean-Marc Valin (valj01@gel.usherb.ca)", "Dominic Letourneau (letd01@gel.usherb.ca)",
      NULL
   };

   GtkWidget *about;
   
   about = gnome_about_new ("vflow", OVERFLOW_VERSION,
                            _("(C) 1999-2001 Jean-Marc Valin & Dominic Letourneau"),
                            authors,
                            _("Overflow (http://freespeech.sourceforge.net/overflow.html) is a free (LGPL) ""data flow oriented"" development environment. It can be use to build complex applications by combining small, reusable building blocks. In some way, it has similarities with Simulink and LabView, although it is not designed (and far) to be a ""clone"" of any of them.\n\nThis software is part of the Open Mind Speech project (http://freespeech.sourceforge.net/).\nOther contributors: Brad Chapman, Luc Lussier, Andre Charbonneau"),
                            NULL);
   gtk_object_set_data (GTK_OBJECT (about), "about", about);
   gtk_window_set_modal (GTK_WINDOW (about), TRUE);
   gtk_widget_show (about);
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
      //_exit(NULL);
      pthread_exit(NULL);
//pthread_cancel (GUIDocument::runThread);
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
void vflowGUI::create_mdi () {
  
  //static data structures

  static GnomeUIInfo toolbar_data[] = {
    
    { GNOME_APP_UI_ITEM, N_("New"), N_("Create a new document"), (gpointer)new_doc_event,
      NULL, NULL, GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_NEW },
    
    { GNOME_APP_UI_ITEM, N_("Open"), N_("Open a file"), (gpointer)open_doc_event,
      NULL, NULL, GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_OPEN },
    
    { GNOME_APP_UI_ITEM, N_("Save"), N_("Save the current file"), (gpointer)save_doc_event,
      NULL, NULL, GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_SAVE },
    
    GNOMEUIINFO_SEPARATOR,
    
    { GNOME_APP_UI_ITEM, N_("Parameters"), N_("Specify document parameters"), (gpointer)doc_prop_event,
      NULL, NULL, GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_PROPERTIES },
    
    { GNOME_APP_UI_ITEM, N_("Run"), N_("Run the current document"), (gpointer)run_doc_event,
      NULL, NULL, GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_EXEC },
    
    { GNOME_APP_UI_ITEM, N_("Stop"), N_("Stop the current document"), (gpointer)stop_doc_event,
      NULL, NULL, GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_STOP },
    
    { GNOME_APP_UI_ITEM, N_("Build"), N_("Generate C++ code"), (gpointer)build_event,
      NULL, NULL, GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_CONVERT },
    
    GNOMEUIINFO_SEPARATOR,
    
    { GNOME_APP_UI_ITEM, N_("Exit"), N_("Exit the program"), (gpointer)exit_event,
      NULL, NULL, GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_QUIT },
    
    GNOMEUIINFO_END
  };
  
  static GnomeUIInfo file1_menu_uiinfo[] = {
    
    GNOMEUIINFO_MENU_NEW_ITEM (N_("_New File"), NULL, new_doc_event, NULL),
    GNOMEUIINFO_MENU_OPEN_ITEM (open_doc_event, NULL),
    GNOMEUIINFO_MENU_SAVE_ITEM (save_doc_event, NULL),
    GNOMEUIINFO_MENU_SAVE_AS_ITEM (saveas_doc_event, NULL),
    GNOMEUIINFO_MENU_CLOSE_ITEM (close_doc_event, NULL),
    GNOMEUIINFO_SEPARATOR,
    GNOMEUIINFO_MENU_EXIT_ITEM (exit_event, NULL),
    GNOMEUIINFO_END
  };
  
  
  static GnomeUIInfo view1_menu_uiinfo[] = {
    GNOMEUIINFO_END
  };
  
  static GnomeUIInfo settings1_menu_uiinfo[] = {
    GNOMEUIINFO_MENU_PREFERENCES_ITEM (on_preferences1_activate, NULL),
    GNOMEUIINFO_END
  };
  
  static GnomeUIInfo help1_menu_uiinfo[] = {
    
    //GNOMEUIINFO_MENU_ABOUT_ITEM (user_manual_event, NULL),
    GNOMEUIINFO_ITEM("User Guide", "", overflow_doc_event, NULL),
    GNOMEUIINFO_ITEM("Node reference", "", overflow_noderef_event, NULL),
    GNOMEUIINFO_MENU_ABOUT_ITEM (about_event, NULL),
    GNOMEUIINFO_END
  };
  
  
  static GnomeUIInfo menubar1_uiinfo[] = {
    
    GNOMEUIINFO_MENU_FILE_TREE (file1_menu_uiinfo),
    //GNOMEUIINFO_MENU_EDIT_TREE (edit1_menu_uiinfo),
    //GNOMEUIINFO_MENU_VIEW_TREE (view1_menu_uiinfo),
    GNOMEUIINFO_MENU_SETTINGS_TREE (settings1_menu_uiinfo),
    GNOMEUIINFO_MENU_HELP_TREE (help1_menu_uiinfo),
    GNOMEUIINFO_END
  };
  

  //creating multiple document interface
  mdi = GNOME_MDI(gnome_mdi_new ("vflow", _("vflow")));
  mdi->tab_pos = GTK_POS_TOP;

  gtk_signal_connect(GTK_OBJECT(mdi), "destroy", GTK_SIGNAL_FUNC(exit_event), this);

  gtk_object_set_data (GTK_OBJECT (mdi), "mdi", mdi);
  gtk_signal_connect(GTK_OBJECT(mdi), "remove_child", GTK_SIGNAL_FUNC(remove_doc_cb), NULL);


  
  //creating toolbar & menus

  gnome_mdi_set_mode (mdi, GNOME_MDI_NOTEBOOK);
  gnome_mdi_set_menubar_template (mdi, menubar1_uiinfo);
  gnome_mdi_set_toolbar_template (mdi, toolbar_data);
  gnome_mdi_set_child_menu_path (mdi, GNOME_MENU_FILE_STRING);
  gnome_mdi_set_child_list_path (mdi, GNOME_MENU_FILES_PATH);


  gnome_mdi_open_toplevel(mdi);

  set_run_mode(false);
}
/**********************************************************************************************************

**********************************************************************************************************/
int main (int argc, char *argv[]) 
{
   try {
      IExtensions::detect();
      scanDL();
      UINodeRepository::Scan();
      //UIDocument::loadAllInfo();

      g_thread_init(NULL); 
      
      gnome_init ("vflow", OVERFLOW_VERSION, argc, argv);
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

