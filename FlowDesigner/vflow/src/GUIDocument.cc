#include "GUIDocument.h"
#include "GUINetwork.h"
#include <typeinfo>

//UIDocument *UIDocument::currentDocument;
extern GnomeMDI *mdi;

static GnomeMDIChildClass *parent_class = NULL;


void create_net(gchar * str, GUIDocument *doc)
{
   cerr << "Adding net to doc type " << typeid(doc).name() << endl;
   //GUIDocument *doc2 = dynamic_cast<GUIDocument *> (doc);
   /*cerr << "name is " << GUIDOCUMENT(doc)->getName() << endl;
   cerr << "castptr = " << GUIDOCUMENT(doc) << endl;
   cerr << "dynptr = " << reinterpret_cast<GUIDocument *> (doc) << endl;
   cerr << "name is " << doc->getName() << endl;*/
   if (str)
      doc->addNetwork(string(str), false);
}
static void add_net_event  (GtkMenuItem     *menuitem,
                            gpointer         user_data)
{
   cerr << "name in add_net_event is " << GUIDOCUMENT(mdi->active_child)->getName() << endl;
   cerr << "ptr in add_net_event is " << mdi->active_child << endl;
   GtkWidget *dialog = gnome_request_dialog (FALSE, "What's the network name?", "MAIN", 20, create_net, mdi->active_child, NULL);
   gnome_dialog_run_and_close(GNOME_DIALOG(dialog));
}

void create_iter(gchar * str, GUIDocument *doc)
{
   if (str)
      doc->addNetwork(string(str), true);
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
//   , untitled(true)
//   , modified(false)
{
   cerr << "GUIDocument::GUIDocument\n";
   gnome_mdi_child_set_name (GNOME_MDI_CHILD(this), (gchar *)docName.c_str());
   create();
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
   cerr << "GUIDocument::load\n";
   cerr << "this = " << this << endl;
   UIDocument::load();
   cerr << "almost loaded\n";
   for (int i=0;i<networks.size();i++)
   {
      dynamic_cast<GUINetwork *> (networks[i])->updateScroll();
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


UINetwork *GUIDocument::newNetwork(UIDocument *_doc, const string &_name, bool iter)
{
   cerr << "GUIDocument::newNetwork\n";
   return new GUINetwork(_doc, _name, iter);
}

UINetwork *GUIDocument::newNetwork(UIDocument *_doc, xmlNodePtr _net, bool iter)
{
   cerr << "GUIDocument::newNetwork\n";
   return new GUINetwork(_doc, _net, iter);
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


