#include "GUIDocument.h"
#include "GUINetwork.h"
#include <typeinfo>
#include "ParameterSet.h"
#include "Network.h"
#include <pthread.h>

//UIDocument *UIDocument::currentDocument;
extern GnomeMDI *mdi;

static GnomeMDIChildClass *parent_class = NULL;

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
   GtkWidget *dialog = gnome_request_dialog (FALSE, "What's the network name?", "MAIN", 20, create_net, mdi->active_child, NULL);
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
   GtkWidget *dialog = gnome_request_dialog (FALSE, "What's the threaded iterator name?", "MAIN", 20, create_threaded, mdi->active_child, NULL);
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
   GtkWidget *dialog = gnome_request_dialog (FALSE, "What's the iterator's name?", "MAIN", 20, create_iter, mdi->active_child, NULL);
   gnome_dialog_run_and_close(GNOME_DIALOG(dialog));
}

static void remove_net_event  (GtkMenuItem     *menuitem,
                            gpointer         user_data)
{
   //cerr << "remove net\n";
}

static GnomeUIInfo view_menu[] = {

   GNOMEUIINFO_ITEM_NONE (N_("_Add Network"),
                          N_("Add a new network the document"), add_net_event),
   GNOMEUIINFO_ITEM_NONE (N_("_Add Iterator"),
                          N_("Add a new iterator the document"), add_iter_event),
   GNOMEUIINFO_ITEM_NONE (N_("_Add Threaded Iterator"),
                          N_("Add a new threaded iterator the document"), add_threaded_event),
   GNOMEUIINFO_ITEM_NONE (N_("Remove Network"),
                          N_("Remove a network from the document"), remove_net_event),
   GNOMEUIINFO_END
	
};

static GnomeUIInfo doc_menu[] = {

   { GNOME_APP_UI_SUBTREE, N_("_Networks"), NULL, view_menu, NULL, NULL,
     GNOME_APP_PIXMAP_NONE, NULL, 0, (GdkModifierType)0, NULL },
   GNOMEUIINFO_END
};




GUIDocument::GUIDocument(string _name)
   : UIDocument(_name)
   , docproperty(NULL)

//   , untitled(true)
//   , modified(false)
{
   //cerr << "GUIDocument::GUIDocument\n";
   gnome_mdi_child_set_name (GNOME_MDI_CHILD(this), (gchar *)docName.c_str());
   create();

   createParamDialog();
}



void GUIDocument::create()
{
   createView();
   gnome_mdi_child_set_menu_template (GNOME_MDI_CHILD (this), doc_menu);
}


GtkWidget *GUIDocument::createView()
{
   
   view = gtk_notebook_new();
   gtk_widget_show (view);
   //gtk_notebook_popup_enable(GTK_NOTEBOOK(view));
   /*gtk_signal_connect (GTK_OBJECT (view), "switch-page",
                       GTK_SIGNAL_FUNC (page_switch),
                       this);*/

}

void GUIDocument::load()
{
   //cerr << "GUIDocument::load\n";
   //cerr << "this = " << this << endl;
   UIDocument::load();
   //cerr << "almost loaded\n";
   for (int i=0;i<networks.size();i++)
   {
      dynamic_cast<GUINetwork *> (networks[i])->updateScroll();
   }
   if (docproperty)
   {
      gtk_widget_destroy(docproperty);
   }

   createParamDialog();

   for (int i=0;i<params.size();i++)
   {
      insertLoadedParam(&(params[i]), params[i].type, params[i].value);
   }
}






static void GUIDocument_class_init(GUIDocument_class *);
static void GUIDocument_init(GUIDocument *);


GtkType GUIDocument_get_type ()
{

   static GtkType doc_type = 0;
	
   if (!doc_type) {
	  	
      static const GtkTypeInfo doc_info = {
         "GUIDocument",
         sizeof (GUIDocument),
         sizeof (GUIDocument_class),
         (GtkClassInitFunc) GUIDocument_class_init,
         (GtkObjectInitFunc) GUIDocument_init,
         (GtkArgSetFunc) NULL,
         (GtkArgGetFunc) NULL,
	  
      };
	  
      doc_type = gtk_type_unique (gnome_mdi_child_get_type (), &doc_info);
	  
   }
	  
   return doc_type;
	
}


static GtkWidget *GUIDocument_create_view (GnomeMDIChild *child)
{
   return GUIDOCUMENT (child)->getView();
}

static gchar *GUIDocument_get_config_string (GnomeMDIChild *child)
{

   return (gchar *)(GUIDOCUMENT (child)->getName().c_str());
	
}
static void GUIDocument_destroy (GtkObject *obj)
{

   GUIDocument *doc;
	
   doc = GUIDOCUMENT(obj);
	
   doc->~GUIDocument();

   //GnomeMDIChildClass *parent_class = gtk_type_class (gnome_mdi_child_get_type ());

   if (GTK_OBJECT_CLASS (parent_class)->destroy)
      (* GTK_OBJECT_CLASS (parent_class)->destroy)(GTK_OBJECT (doc));

}


static void GUIDocument_class_init (GUIDocument_class *klass)
{

   GtkObjectClass 		*object_class;
   GnomeMDIChildClass	*child_class;
	
   object_class = (GtkObjectClass*)klass;
   child_class = GNOME_MDI_CHILD_CLASS (klass);
	
	
   object_class->destroy = GUIDocument_destroy;
	
   child_class->create_view = (GnomeMDIChildViewCreator)(GUIDocument_create_view);
   //child_class->get_config_string = (GnomeMDIChildConfigFunc)(GUIDocument_get_config_string);
	
	//class->document_changed = gE_document_real_changed;
	
   parent_class = (GnomeMDIChildClass*)gtk_type_class (gnome_mdi_child_get_type ());
	
}

void GUIDocument_init (GUIDocument *doc)
{

   new(doc) GUIDocument("Untitled");
   //GUIDOCUMENT (child)->getView();
		
   //gnome_mdi_child_set_menu_template (GNOME_MDI_CHILD (doc), doc_menu);
	
}

GUIDocument *GUIDocument_new ()
{

   GUIDocument *doc;
	
   int i;
	
   // FIXME: Blarg!! 
	
   if ((doc = (GUIDocument*)gtk_type_new (GUIDocument_get_type ()))) {
	
      //gnome_mdi_child_set_name(GNOME_MDI_CHILD(doc), "Untitled");
	  

      return doc;
   }
	
   g_print ("Eeek.. bork!\n");
   gtk_object_destroy (GTK_OBJECT(doc));
	
   return NULL;
	
}


UINetwork *GUIDocument::newNetwork(UIDocument *_doc, const string &_name, UINetwork::Type type)
{
   //cerr << "GUIDocument::newNetwork\n";
   return new GUINetwork(_doc, _name, type);
}

UINetwork *GUIDocument::newNetwork(UIDocument *_doc, xmlNodePtr _net)
{
   //cerr << "GUIDocument::newNetwork\n";
   return new GUINetwork(_doc, _net);
}


void GUIDocument::setFullPath(const string &fullpath)
{
   //cerr << "fullpath is: \"" << fullpath << "\"" << endl;
   int slashpos = fullpath.rfind("/");
   //cerr << "slashpos = " << slashpos << endl;
   path="";
   path.append(fullpath,0,slashpos+1);
   docName=fullpath;
   docName.erase(0,slashpos+1);
   //cerr << "path is: \"" << path << "\"" << endl;
   //cerr << "name is: \"" << name << "\"" << endl;
   gnome_mdi_child_set_name (GNOME_MDI_CHILD(this), (gchar *)docName.c_str());
   untitled=false; 
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
      params[i].type = (char *)gtk_object_get_user_data (GTK_OBJECT(menu));

      GtkWidget *gtkentr = gnome_entry_gtk_entry(GNOME_ENTRY(params[i].entry));
      params[i].value = gtk_entry_get_text(GTK_ENTRY(gtkentr));
      
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
     params[i].label = gtk_label_new (params[i].name.c_str());
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

static void disposeFunct(void *dummy)
{
   //delete net;
   cerr << "deleting\n";
   GUIDocument::isRunning=false;
   delete GUIDocument::runningNet; 
   GUIDocument::runningNet=NULL;

   GtkWidget *w = gnome_mdi_get_toolbar_info (gnome_mdi_get_active_window(mdi))[5].widget;
   gtk_widget_set_sensitive(w,true);
   w = gnome_mdi_get_toolbar_info (gnome_mdi_get_active_window(mdi))[6].widget;
   gtk_widget_set_sensitive(w,false);

   //pthread_cleanup_push(routine,arg) 
}

static void threadFunct(GUIDocument *doc)
{
   doc->run();
   // pthread_cleanup_push(disposeFunct, NULL);
}

void GUIDocument::threadRun()
{
   if (!isRunning)
   {
      isRunning=true;
      pthread_create(&runThread, NULL, (void * (*)(void *))threadFunct, this);
      pthread_detach(runThread);
   }
}

void GUIDocument::threadStop()
{
   if (isRunning)
   {
      isRunning=false;
      pthread_cancel(runThread);
      //pthread_create(&runThread, NULL, threadFunct, this);
   }

}

//Run with a GUI
void GUIDocument::run()
{
   //cerr << "GUIDocument::run\n";
   pthread_cleanup_push(disposeFunct, NULL);
   try{
      ParameterSet parameters;
      {
	 cerr << "there are " << params.size() << " params\n";
	 for (int i=0;i<params.size();i++)
	 {
	    DocParameterData &curr = params[i];
	    if (curr.value == "")
	       continue;
	    ObjectRef value;
	    if (curr.type == "int")
	    {
	       int val = atoi (curr.value.c_str());
	       value = ObjectRef(new Int(val));
	    } 
	    else if (curr.type == "float")
	    {
	       float val = atof (curr.value.c_str());
	       value = ObjectRef(new Float(val)); 
	    } 
	    else if (curr.type == "string")
	    {
	       cerr << "string\n";
	       value = ObjectRef(new String(curr.value));	 	 
	    }
	    else {
	       cerr << "UNKNOWN PARAM TYPE: \"" << curr.type << "\"" << endl;
	    }
	    
	    parameters.add(curr.name,value);
	 }

      }
      cerr << "building net...\n";
      parameters.print();
      Network *net = build("MAIN", parameters);
      cerr << "initializing...\n";
      net->initialize();
      cerr << "running...\n";
      runningNet=net;
      cout << *net->getOutput(0,0) << endl;

      //delete net;
      //ask for params and desired output
      
      //run in a window in a separated thread
   } catch (BaseException &e)
   {
      e.print();
      
   }
   pthread_cleanup_pop(1);

}
