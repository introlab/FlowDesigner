// Copyright (C) 2001 Jean-Marc Valin

#include "GUIDocument.h"
#include "GUINetwork.h"
#include <typeinfo>
#include "ParameterSet.h"
#include "Network.h"
#include <pthread.h>
#include "rc_ptrs.h"
//#include <strstream>
#include <sstream>

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif


//#include <gdk/gdk.h>

//UIDocument *UIDocument::currentDocument;
extern GnomeMDI *mdi;

//static GnomeMDIChildClass *parent_class = NULL;

bool GUIDocument::isRunning=false;
pthread_t GUIDocument::runThread;
Network * GUIDocument::runningNet=NULL;


void create_net(gchar * str, GUIDocument *doc)
{
   if (str)
      doc->addNetwork(string(str), UINetwork::subnet);
}
static void add_net_event  (GtkMenuItem     *menuitem,
                            gpointer         user_data)
{
   GUIDocument *doc = (GUIDocument*)gtk_object_get_data(GTK_OBJECT(mdi->active_child), "doc");
   GtkWidget *dialog = gnome_request_dialog (FALSE, "What's the network name?", "MAIN", 20, (GnomeStringCallback)create_net, doc, NULL);
   gnome_dialog_run_and_close(GNOME_DIALOG(dialog));
}

void create_threaded(gchar * str, GUIDocument *doc)
{
   if (str)
      doc->addNetwork(string(str), UINetwork::threaded);
}
static void add_threaded_event  (GtkMenuItem     *menuitem,
                            gpointer         user_data)
{
   GUIDocument *doc = (GUIDocument*)gtk_object_get_data(GTK_OBJECT(mdi->active_child), "doc");
   GtkWidget *dialog = gnome_request_dialog (FALSE, "What's the threaded iterator name?", "MAIN", 20, (GnomeStringCallback)create_threaded, doc, NULL);
   gnome_dialog_run_and_close(GNOME_DIALOG(dialog));
}

void create_iter(gchar * str, GUIDocument *doc)
{
   if (str)
      doc->addNetwork(string(str), UINetwork::iterator);
}
static void add_iter_event  (GtkMenuItem     *menuitem,
                            gpointer         user_data)
{
   GUIDocument *doc = (GUIDocument*)gtk_object_get_data(GTK_OBJECT(mdi->active_child), "doc");
   GtkWidget *dialog = gnome_request_dialog (FALSE, "What's the iterator's name?", "MAIN", 20, (GnomeStringCallback)create_iter, doc, NULL);
   gnome_dialog_run_and_close(GNOME_DIALOG(dialog));
}

static void rename_net_event  (GtkMenuItem     *menuitem,
                            gpointer         user_data)
{
   GUIDocument *doc = (GUIDocument*)gtk_object_get_data(GTK_OBJECT(mdi->active_child), "doc");
   doc->renameCurrentNet();
   //gtk_notebook_get_current_page (GTK_NOTEBOOK(notebook1));
   //doc
 //cerr << "remove net\n";
}

static void remove_net_event  (GtkMenuItem     *menuitem,
                            gpointer         user_data)
{
   GUIDocument *doc = (GUIDocument*)gtk_object_get_data(GTK_OBJECT(mdi->active_child), "doc");
   doc->removeCurrentNet();
   //gtk_notebook_get_current_page (GTK_NOTEBOOK(notebook1));
   //doc
 //cerr << "remove net\n";
}

static GnomeUIInfo view_menu[] = {
   
   GNOMEUIINFO_ITEM_NONE (N_("Add _Network"),
                          N_("Add a new network the document"), add_net_event),
   GNOMEUIINFO_ITEM_NONE (N_("Add _Iterator"),
			     N_("Add a new iterator the document"), add_iter_event),
   GNOMEUIINFO_ITEM_NONE (N_("Add _Threaded Iterator"),
			     N_("Add a new threaded iterator the document (EXPERIMENTAL)"), add_threaded_event),
   GNOMEUIINFO_SEPARATOR,
   GNOMEUIINFO_ITEM_NONE (N_("_Rename Network"),
			     N_("Remove a network from the document"), rename_net_event),
   GNOMEUIINFO_SEPARATOR,
   GNOMEUIINFO_ITEM_NONE (N_("Remove Network"),
			     N_("Remove a network from the document"), remove_net_event),
   GNOMEUIINFO_END
      
};

static GnomeUIInfo doc_menu[] = {

   { GNOME_APP_UI_SUBTREE, N_("_Networks"), NULL, view_menu, NULL, NULL,
     GNOME_APP_PIXMAP_NONE, NULL, 0, (GdkModifierType)0, NULL },
   GNOMEUIINFO_END
};


static GtkWidget *create_view (GnomeMDIChild *child, GUIDocument *doc)
{
   //GUIDocument *doc = gtk_object_get_data (GTK_OBJECT (child), "doc");
   return doc->getView();
}


GUIDocument::GUIDocument(string _name)
   : UIDocument(_name)
   , docproperty(NULL)
   , vbox2(NULL)
   , notebook1(NULL)
   , less2(NULL)
//   , untitled(true)
//   , modified(false)
{
   //cerr << "GUIDocument::GUIDocument\n";
   mdiChild = gnome_mdi_generic_child_new((gchar *)docName.c_str());
   gtk_object_set_data (GTK_OBJECT (mdiChild), "doc", this);
   gnome_mdi_child_set_name (GNOME_MDI_CHILD(mdiChild), (gchar *)docName.c_str());
   gnome_mdi_generic_child_set_view_creator(mdiChild, (GnomeMDIChildViewCreator)create_view, this);

   create();

   createParamDialog();
}



void GUIDocument::create()
{
   createView();
   gnome_mdi_child_set_menu_template (GNOME_MDI_CHILD (mdiChild), doc_menu);
}


GtkWidget *GUIDocument::createView()
{
   
  /*
    window2 = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_object_set_data (GTK_OBJECT (window2), "window2", window2);
    gtk_window_set_title (GTK_WINDOW (window2), _("window2"));
  */

  
   //vbox2 = gtk_vbox_new (FALSE, 0);
  vbox2 = gtk_vpaned_new ();

  //gtk_widget_set_usize(vbox2, -1, 400);

  gtk_widget_ref (vbox2);
  //gtk_object_set_data_full (GTK_OBJECT (mdi), "vbox2", vbox2,
  //                          (GtkDestroyNotify) gtk_widget_unref);

  gtk_widget_show (vbox2);
  
  
  //gtk_container_add (GTK_CONTAINER (vbox2), vbox2);
  
  

  notebook1 = gtk_notebook_new ();
  gtk_widget_set_usize(notebook1, -1, 320);
  gtk_notebook_set_scrollable(GTK_NOTEBOOK(notebook1), TRUE);
  gtk_notebook_popup_enable (GTK_NOTEBOOK(notebook1));

  gtk_widget_ref (notebook1);
  //gtk_object_set_data_full (GTK_OBJECT (mdi), "notebook1", notebook1,
  //                          (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (notebook1);

  gtk_paned_pack1 (GTK_PANED(vbox2), notebook1, TRUE, TRUE);
  //gtk_container_add (GTK_CONTAINER (vbox2), notebook1);
  //gtk_box_pack_start (GTK_BOX (vbox2), notebook1, TRUE, TRUE, 0);

  

  less2 = gnome_less_new ();
  gtk_widget_set_usize(less2, -1, 110);

  gtk_widget_ref (less2);
  //gtk_object_set_data_full (GTK_OBJECT (mdi), "less2", less2,
  //                          (GtkDestroyNotify) gtk_widget_unref);
  
  gtk_widget_show (less2);
  //gtk_box_pack_start (GTK_BOX (vbox2), less2, TRUE, TRUE, 0);
  
  //gtk_container_add (GTK_CONTAINER (vbox2), less2);
  gtk_paned_pack2 (GTK_PANED(vbox2), less2, FALSE, TRUE);

  
  /*
    
    view = gtk_notebook_new();
    gtk_widget_show (view);
   
  */
   
  
  gtk_widget_show(vbox2);

  //view = vbox2;
  less_print("VFlow by Jean-Marc Valin & Dominic Letourneau");
  //less_print("You can print here by using GUIDocument::less_print (const char* message).");
  less_print("--\n");
  

}

void GUIDocument::load()
{
   //cerr << "GUIDocument::load\n";
   //cerr << "this = " << this << endl;
   UIDocument::load();
	
   // convert the textParams into GUI params
   /*for (int i=0; i<textParams.size(); i++)
   {
	 DocParameterData newParam;
	 newParam.name = textParams[i].name;
	 newParam.value = textParams[i].value;
	 newParam.type = textParams[i].type;
	 params.insert(params.end(), newParam);
	 }*/
   params.resize(textParams.size());

   //cerr << "almost loaded\n";
   //cerr << "doc loaded\n";
   for (int i=0;i<networks.size();i++)
   {
      dynamic_cast<GUINetwork *> (networks[i])->updateScroll();
   }
   //cerr << "aa\n";
   //BUG I guess we should delete this, but it screws up with the threads
   if (docproperty)
   {
      gtk_widget_destroy(docproperty);
   }
   //cerr << "bb\n";
   createParamDialog();
   //cerr << "cc\n";

   for (int i=0;i<params.size();i++)
   {
      insertLoadedParam(&(params[i]), textParams[i]->type, textParams[i]->value);
   }
   //cerr << "doc load updated\n";
}



void GUIDocument::removeCurrentNet()
{
   int netID = gtk_notebook_get_current_page (GTK_NOTEBOOK(notebook1));
   //cerr << "netID = " << netID << endl;
   if (netID == -1) 
      return;
   delete networks[netID];
   gtk_notebook_remove_page (GTK_NOTEBOOK(notebook1), netID);
   for (int i=netID;i<networks.size()-1;i++)
   {
      networks[i]=networks[i+1];
   }
   networks.resize(networks.size()-1);
}

void rename_net(gchar * str, UINetwork *net)
{
   
   if (str)
      net->rename(str);
}

void GUIDocument::renameCurrentNet()
{
   int netID = gtk_notebook_get_current_page (GTK_NOTEBOOK(notebook1));
   //cerr << "netID = " << netID << endl;
   if (netID == -1) 
      return;
   UINetwork *net = networks[netID];
   string message;
   message = string("Rename network ") + net->getName();
   GtkWidget *dialog = gnome_request_dialog (FALSE, message.c_str(), "", 20, (GnomeStringCallback)rename_net, net, NULL);
   gnome_dialog_run_and_close(GNOME_DIALOG(dialog));
}


UINetwork *GUIDocument::newNetwork(const string &_name, UINetwork::Type type)
{
   //cerr << "GUIDocument::newNetwork\n";
   return new GUINetwork(this, _name, type);
}

UINetwork *GUIDocument::newNetwork(xmlNodePtr _net)
{
   //cerr << "GUIDocument::newNetwork\n";
   return new GUINetwork(this, _net);
}


void GUIDocument::setFullPath(const string &fullpath)
{
   // call the non-gui code in UIDocument
   UIDocument::setFullPath(fullpath);
   gnome_mdi_child_set_name (GNOME_MDI_CHILD(mdiChild), (gchar *)docName.c_str());
}



static void param_apply (GnomePropertyBox *propertybox, gint arg1, gpointer user_data)
{
   ((GUIDocument *)(user_data))->applyParams();
}

static void entry_changed (GtkEntry *entry, gpointer user_data)
{
   ((GUIDocument *)(user_data))->changedParams();
}

static void type_changed (GnomePropertyBox *propertybox, gpointer user_data)
{
   ((GUIDocument *)(user_data))->changedParams();
}

void GUIDocument::showParams()
{
   gtk_widget_show (docproperty);

}

void GUIDocument::changedParams()
{
   gnome_property_box_changed(GNOME_PROPERTY_BOX(docproperty));
}

void GUIDocument::applyParams()
{
   for (int i=0;i<params.size();i++)
   {
      //GtkWidget *gtk_option_menu_get_menu(params[i].optionmenu);
      GtkWidget *menu = gtk_menu_get_active (GTK_MENU(params[i].optionmenu_menu));
      textParams[i]->type = (char *)gtk_object_get_user_data (GTK_OBJECT(menu));

      GtkWidget *gtkentr = gnome_entry_gtk_entry(GNOME_ENTRY(params[i].entry));
      textParams[i]->value = gtk_entry_get_text(GTK_ENTRY(gtkentr));
      
      //cerr << "<param: " << params[i].name << ", " << params[i].type << ":" << params[i].value << ">\n";
   }
   //cerr << "apply\n";
   setModified();
}

static const vector<string> &allDocTypes()
{
   static vector<string> types;
   static int init=false;
   if (!init)
   {
      types.insert(types.end(), "int");
      types.insert(types.end(), "float");
      types.insert(types.end(), "string");
      types.insert(types.end(), "bool");
      init=true;
   }
   return types;
}

void GUIDocument::insertLoadedParam(DocParameterData *param, string type, string value)
{
   const vector<string> &types=allDocTypes();
   for (int i=0;i<types.size();i++)
      if (types[i] == type)
         gtk_option_menu_set_history (GTK_OPTION_MENU (param->optionmenu), i);
   GtkWidget *gtkentr = gnome_entry_gtk_entry(GNOME_ENTRY(param->entry));
   gtk_entry_set_text(GTK_ENTRY(gtkentr),(gchar *)value.c_str());

}

void GUIDocument::createParamDialog()
{
   int i;

   /*vector<string> tmp = getNetParams("MAIN");
   cerr << "Got " << tmp.size() << " params in GUIDocument::createParamDialog\n";
   params.resize(tmp.size());
   for (i=0;i<tmp.size();i++)
      params[i].name=tmp[i];
      cerr << "--\n";*/
   
   //cerr << "GUINodeParameters::GUINodeParameters\n";

   //GtkWidget *nodeproperty;
  GtkWidget *notebook2;
  GtkWidget *table2;
  GtkWidget *label14;
  GtkWidget *label15;
  GtkWidget *label16;
  GtkWidget *hseparator4;
  GtkWidget *hseparator5;
  GtkWidget *hseparator6;
  GtkWidget *glade_menuitem;
  

  GtkWidget *label12;
  GtkWidget *scrolledwindow2;
  GtkWidget *text1;
  GtkWidget *label13;

  docproperty = gnome_property_box_new ();
  gnome_dialog_close_hides (GNOME_DIALOG(docproperty), TRUE);

  gtk_object_set_data (GTK_OBJECT (docproperty), "docproperty", docproperty);

  notebook2 = GNOME_PROPERTY_BOX (docproperty)->notebook;
  gtk_object_set_data (GTK_OBJECT (docproperty), "notebook2", notebook2);
  gtk_widget_show (notebook2);

  table2 = gtk_table_new (2+params.size(), 3, FALSE);
  gtk_widget_ref (table2);
  gtk_object_set_data_full (GTK_OBJECT (docproperty), "table2", table2,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (table2);
  gtk_container_add (GTK_CONTAINER (notebook2), table2);

  label14 = gtk_label_new (_("Name"));
  gtk_widget_ref (label14);
  gtk_object_set_data_full (GTK_OBJECT (docproperty), "label14", label14,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label14);
  gtk_table_attach (GTK_TABLE (table2), label14, 0, 1, 0, 1,
                    (GtkAttachOptions) (0),
                    (GtkAttachOptions) (0), 0, 0);

  label15 = gtk_label_new (_("Type"));
  gtk_widget_ref (label15);
  gtk_object_set_data_full (GTK_OBJECT (docproperty), "label15", label15,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label15);
  gtk_table_attach (GTK_TABLE (table2), label15, 1, 2, 0, 1,
                    (GtkAttachOptions) (0),
                    (GtkAttachOptions) (0), 0, 0);

  label16 = gtk_label_new (_("Value"));
  gtk_widget_ref (label16);
  gtk_object_set_data_full (GTK_OBJECT (docproperty), "label16", label16,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label16);
  gtk_table_attach (GTK_TABLE (table2), label16, 2, 3, 0, 1,
                    (GtkAttachOptions) (0),
                    (GtkAttachOptions) (0), 0, 0);

  hseparator4 = gtk_hseparator_new ();
  gtk_widget_ref (hseparator4);
  gtk_object_set_data_full (GTK_OBJECT (docproperty), "hseparator4", hseparator4,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (hseparator4);
  gtk_table_attach (GTK_TABLE (table2), hseparator4, 0, 1, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);

  hseparator5 = gtk_hseparator_new ();
  gtk_widget_ref (hseparator5);
  gtk_object_set_data_full (GTK_OBJECT (docproperty), "hseparator5", hseparator5,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (hseparator5);
  gtk_table_attach (GTK_TABLE (table2), hseparator5, 1, 2, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (GTK_FILL), 0, 0);

  hseparator6 = gtk_hseparator_new ();
  gtk_widget_ref (hseparator6);
  gtk_object_set_data_full (GTK_OBJECT (docproperty), "hseparator6", hseparator6,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (hseparator6);
  gtk_table_attach (GTK_TABLE (table2), hseparator6, 2, 3, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (GTK_FILL), 0, 0);



  
  for (i=0;i<params.size();i++)
  {
     params[i].label = gtk_label_new (textParams[i]->name.c_str());
     gtk_widget_ref (params[i].label);
     gtk_object_set_data_full (GTK_OBJECT (docproperty), "label", params[i].label,
                               (GtkDestroyNotify) gtk_widget_unref);
     gtk_widget_show (params[i].label);
     gtk_table_attach (GTK_TABLE (table2), params[i].label, 0, 1, 2+i, 3+i,
                       (GtkAttachOptions) (0),
                       (GtkAttachOptions) (0), 0, 0);



     params[i].optionmenu = gtk_option_menu_new ();
     gtk_widget_ref (params[i].optionmenu);
     gtk_object_set_data_full (GTK_OBJECT (docproperty), "optionmenu", params[i].optionmenu,
                               (GtkDestroyNotify) gtk_widget_unref);
     gtk_widget_show (params[i].optionmenu);
     gtk_table_attach (GTK_TABLE (table2), params[i].optionmenu, 1, 2, 2+i, 3+i,
                       (GtkAttachOptions) (0),
                       (GtkAttachOptions) (0), 0, 0);
     params[i].optionmenu_menu = gtk_menu_new ();

     const vector<string> &types=allDocTypes();
     //types.insert(types.end(), string("Int"));
     //types.insert(types.end(), string("Float"));
     //types.insert(types.end(), string("String"));
     for (int j=0;j<types.size();j++)
     {
        glade_menuitem = gtk_menu_item_new_with_label ((const gchar *)types[j].c_str());
        gtk_object_set_user_data(GTK_OBJECT(glade_menuitem), (void *)types[j].c_str());
        gtk_widget_show (glade_menuitem);
        gtk_menu_append (GTK_MENU (params[i].optionmenu_menu), glade_menuitem);
     }

     gtk_option_menu_set_menu (GTK_OPTION_MENU (params[i].optionmenu), params[i].optionmenu_menu);
     gtk_option_menu_set_history (GTK_OPTION_MENU (params[i].optionmenu), 0);
          

     gtk_signal_connect (GTK_OBJECT ( params[i].optionmenu_menu ), "selection-done",
                        GTK_SIGNAL_FUNC( type_changed), this);




     params[i].entry = gnome_entry_new (NULL);
     gtk_widget_ref (params[i].entry);
     gtk_object_set_data_full (GTK_OBJECT (docproperty), "entry", params[i].entry,
                               (GtkDestroyNotify) gtk_widget_unref);
     gtk_widget_show (params[i].entry);
     gtk_table_attach (GTK_TABLE (table2), params[i].entry, 2, 3, 2+i, 3+i,
                       (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                       (GtkAttachOptions) (0), 0, 0);
     
     params[i].combo_entry = gnome_entry_gtk_entry (GNOME_ENTRY (params[i].entry));
     gtk_widget_ref (params[i].combo_entry);
     gtk_object_set_data_full (GTK_OBJECT (docproperty), "combo_entry", params[i].combo_entry,
                               (GtkDestroyNotify) gtk_widget_unref);
     gtk_widget_show (params[i].combo_entry);
     
     GtkWidget *gtkentr = gnome_entry_gtk_entry(GNOME_ENTRY(params[i].entry));
     gtk_signal_connect (GTK_OBJECT ( gtkentr  ), "changed",
                         GTK_SIGNAL_FUNC(entry_changed), this);
  }
  






  label12 = gtk_label_new (_("Parameters"));
  gtk_widget_ref (label12);
  gtk_object_set_data_full (GTK_OBJECT (docproperty), "label12", label12,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label12);
  gtk_notebook_set_tab_label (GTK_NOTEBOOK (notebook2), gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook2), 0), label12);

  scrolledwindow2 = gtk_scrolled_window_new (NULL, NULL);
  gtk_widget_ref (scrolledwindow2);
  gtk_object_set_data_full (GTK_OBJECT (docproperty), "scrolledwindow2", scrolledwindow2,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (scrolledwindow2);
  gtk_container_add (GTK_CONTAINER (notebook2), scrolledwindow2);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolledwindow2), GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);

  text1 = gtk_text_new (NULL, NULL);
  gtk_widget_ref (text1);
  gtk_object_set_data_full (GTK_OBJECT (docproperty), "text1", text1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (text1);
  gtk_container_add (GTK_CONTAINER (scrolledwindow2), text1);
  gtk_text_set_editable(GTK_TEXT(text1),TRUE);

  label13 = gtk_label_new (_("Comments"));
  gtk_widget_ref (label13);
  gtk_object_set_data_full (GTK_OBJECT (docproperty), "label13", label13,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label13);
  gtk_notebook_set_tab_label (GTK_NOTEBOOK (notebook2), gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook2), 1), label13);

  //gnome_property_box_changed(GNOME_PROPERTY_BOX(docproperty));
  gtk_signal_connect (GTK_OBJECT (docproperty), "apply",
                      GTK_SIGNAL_FUNC(param_apply), this);

}


extern void set_run_mode (bool isRuning);

static void disposeFunct(void *dummy)
{
   gdk_threads_enter();
   set_run_mode(false);
   gdk_threads_leave();
   //cerr << "disposeFunct called\n";
   
   //GUIDocument *doc = (GUIDocument*) dummy;
   GUIDocument::isRunning = false;
   //cerr <<  "Deleting the running network.\n"; 
   
   if (GUIDocument::runningNet)
   {
      GUIDocument::runningNet->cleanupNotify();
      delete GUIDocument::runningNet;
      GUIDocument::runningNet=NULL;
   }
}

static void threadFunct(GUIDocument *doc)
{
   doc->run();
}

void GUIDocument::threadRun()
{
   less_print("Running " + docName + "...");
   if (!isRunning)
   {
      isRunning=true;

      //Let's create the thread already in a detached state
      pthread_attr_t tattr;
      pthread_attr_init(&tattr);
      pthread_attr_setdetachstate(&tattr,PTHREAD_CREATE_DETACHED);
      pthread_create(&runThread, &tattr, (void * (*)(void *))threadFunct, this);
      //pthread_detach(runThread); //Dominic

      
   }
}

//extern void set_run_mode (bool isRuning);

void GUIDocument::threadStop()
{
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   
   pthread_mutex_lock (&mutex);
     
   if (isRunning) {
      //cerr << "stopping...\n";
      isRunning=false;
      //runningNet->cleanupNotify();
      #ifdef HAVE_PTHREAD_CANCEL
      pthread_cancel(runThread);
      #endif
      //less_print("Stopping " + docName);
   }

   pthread_mutex_unlock (&mutex);
   

}

//Run with a GUI
void GUIDocument::run()
{
   //cerr << "GUIDocument::run\n";
   pthread_cleanup_push(disposeFunct, this);
   //Network *net;
   try{
      ParameterSet parameters;
      {
	 //cerr << "there are " << params.size() << " params\n";
	 for (int i=0;i<params.size();i++)
	 {
	    DocParameterDataText *curr = textParams[i];
	    if (curr->value == "")
	       continue;
	    ObjectRef value;
	    if (curr->type == "int")
	    {
	       int val = atoi (curr->value.c_str());
	       value = ObjectRef(new Int(val));
	    } 
	    else if (curr->type == "bool")
	    {
	       if (curr->value == "true")
		  value = ObjectRef(new Bool(true));
	       else 
		  value = ObjectRef(new Bool(false));
	    } 
	    else if (curr->type == "float")
	    {
	       float val = atof (curr->value.c_str());
	       value = ObjectRef(new Float(val)); 
	    } 
	    else if (curr->type == "string")
	    {
	       //cerr << "string\n";
	       value = ObjectRef(new String(curr->value));	 	 
	    }
	    else {
	       cerr << "UNKNOWN PARAM TYPE: \"" << curr->type << "\"" << endl;
	    }
	    
	    parameters.add(curr->name,value);
	 }

      }
      //cerr << "building net...\n";
      //parameters.print();
      
      bool buildError = false;

      runningNet = NULL;
      Network *net=NULL;
      //try {
	 net = build("MAIN", parameters);

      {
      if (net->getInputNode())
	 throw new GeneralException ("main network has input node", __FILE__, __LINE__);
      runningNet = net;
      //RCPtr<Network> net(build("MAIN", parameters));
      //cerr << "initializing...\n";
      //net->verifyConnect();
      net->initialize();
      //cerr << "running...\n";
      
      // Getting all the network outputs.
      for (int k=0; ;k++) {
	if (net->hasOutput(k)) {
	   /*char str[3000];
	   strstream execOut(str, 2999);
	   
	   //cerr<<"before main getOutput"<<endl;
	   execOut << *net->getOutput(k,0);
	   //cerr<<"after main getOutput"<<endl;
	   */
	   stringstream execOut;
	   execOut << *net->getOutput(k,0);
	   

	   gdk_threads_enter();
	   less_print(execOut.str());
	   //less_print(str);
	   gdk_threads_leave();

	}
	else {
	  break;
	}
      }
      
      gdk_threads_enter();
      less_print("Exited normally");
      gdk_threads_leave();
      //delete net;
      //ask for params and desired output
      }  
      //run in a window in a separated thread
   } catch (BaseException *e)
   {
      /*char str[3000];
	strstream excOut(str, 2999);*/
      stringstream excOut;

      e->print (excOut);
      gdk_threads_enter();
      less_print(excOut.str());
      //less_print(str);
      gdk_threads_leave();
      //cerr << "exception caught\n";
      //e->print();
      //cerr << "---\n";

      delete e;
      /* The net will be deleted in the dispose function. */
      //delete net;
      //runningNet=NULL;
   } 
   catch (...)
   {
      gdk_threads_enter();
      less_print("Unknown exception caught");
      gdk_threads_leave();
   }
     
   
   pthread_cleanup_pop(1);

}


void GUIDocument::less_print(const string &message) {

  less_text += message + string("\n");

  if (less2) {
    gnome_less_show_string(GNOME_LESS(less2),less_text.c_str());
  }
  
}

void GUIDocument::less_print(const char *message) {

  less_text += string(message) + string("\n");

  if (less2) {
    gnome_less_show_string(GNOME_LESS(less2),less_text.c_str());
  }
  
}

void GUIDocument::less_clear() {
  if (less2) {
    less_text = string();
    gnome_less_show_string(GNOME_LESS(less2),less_text.c_str());
  }
}
