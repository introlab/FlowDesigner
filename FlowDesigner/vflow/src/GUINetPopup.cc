// Copyright (C) 2001 Jean-Marc Valin

#include "GUINetPopup.h"
#include "GUINetwork.h"
#include "UIDocument.h"
#include "GUIDocument.h"
#include "Node.h"
#include <iostream>
#include <string>
#include <set>

using namespace std;

static GtkMenuItemClass *parent_class = NULL;

extern int theTime;

static void new_node_event(GtkMenuItem     *menuitem,
                           UINetwork * net)
{

  //should add a node where we clicked!
  GUINetwork *my_net = dynamic_cast<GUINetwork*>(net);

  double x, y;

  my_net->get_last_click(x,y);

  gchar *name = (gchar*)gtk_object_get_user_data(GTK_OBJECT(menuitem));

  //dynamic_cast<GUINetwork *>(net)->addNode((char *)name, 100.0,100.0);
  dynamic_cast<GUINetwork *>(net)->addNode((char *)name, x, y);


}

static void zoomin(GtkMenuItem     *menuitem,
                           UINetwork * net)
{
   dynamic_cast<GUINetwork *> (net)->zoomIn();
}

static void zoomout(GtkMenuItem     *menuitem,
                           UINetwork * net)
{
   dynamic_cast<GUINetwork *> (net)->zoomOut();
}

GUINetPopup::GUINetPopup(UIDocument *_doc, UINetwork *_net)
   : doc(_doc)
   , net(_net)
{
   menu =  gtk_menu_new();
   gtk_widget_show(menu);


   GtkWidget *label = gtk_menu_item_new_with_label("New Node");
   gtk_object_ref(GTK_OBJECT(label));
   gtk_object_set_data_full (GTK_OBJECT (menu), "new",
                             label,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show(label);
   gtk_menu_append(GTK_MENU(menu),label);


   newMenu = gtk_menu_new();
   gtk_widget_show(newMenu);
   gtk_object_ref(GTK_OBJECT(newMenu));
   gtk_object_set_data_full (GTK_OBJECT (menu), "new",
                             newMenu,
                             (GtkDestroyNotify) gtk_widget_unref);
   
   GtkWidget *tear = gtk_tearoff_menu_item_new();
   gtk_object_ref(GTK_OBJECT(tear));
   gtk_object_set_data_full (GTK_OBJECT (newMenu), "new",
                             tear,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show(tear);
   gtk_menu_append(GTK_MENU(newMenu),tear);

   //gtk_menu_set_tearoff_state(GTK_MENU(newMenu),TRUE);
 
   gtk_menu_item_set_submenu (GTK_MENU_ITEM(label),newMenu);

   //addCategory("Signal");
   //addCategory("General");
   //addType("Signal", "FFT");
   //addType("General", "Constant");

   /*map<string,_NodeFactory*> &factories = Node::factoryDictionary();
   map<string,_NodeFactory*>::iterator f=factories.begin();
   while (f!=factories.end())
   {
      //newMenu->add(f->second->getCategory(),f->second->getName());
      addType(f->second->getCategory(),f->second->getName());
      f++;
      }*/
   UINodeRepository::iterator info = UINodeRepository::Begin();
   set<string> strCategories;
   while (info != UINodeRepository::End()) 
   {
      strCategories.insert(info->second->category);
      //addType(info->second->category,info->first);
      info++;
   }
   
   set<string>::iterator cat = strCategories.begin();
   while (cat != strCategories.end())
   {
      addCategory(*cat);
      cat++;
   }

   info = UINodeRepository::Begin();
   while (info != UINodeRepository::End()) 
   {

      addType(info->second->category,info->first);
      info++;
   }




   label = gtk_menu_item_new_with_label("Zoom");
   gtk_object_ref(GTK_OBJECT(label));
   gtk_object_set_data_full (GTK_OBJECT (menu), "new",
                             label,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show(label);
   gtk_menu_append(GTK_MENU(menu),label);




   GtkWidget *zoom = gtk_menu_new();
   gtk_widget_show(zoom);
   gtk_object_ref(GTK_OBJECT(zoom));
   gtk_object_set_data_full (GTK_OBJECT (menu), "new",
                             zoom,
                             (GtkDestroyNotify) gtk_widget_unref);
   
   tear = gtk_tearoff_menu_item_new();
   gtk_object_ref(GTK_OBJECT(tear));
   gtk_object_set_data_full (GTK_OBJECT (zoom), "new",
                             tear,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show(tear);
   gtk_menu_append(GTK_MENU(zoom),tear);

   //gtk_menu_set_tearoff_state(GTK_MENU(newMenu),TRUE);
 
   gtk_menu_item_set_submenu (GTK_MENU_ITEM(label),zoom);



   label = gtk_menu_item_new_with_label("Zoom In");
   gtk_object_ref(GTK_OBJECT(label));
   gtk_object_set_data_full (GTK_OBJECT (zoom), "new",
                             label,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show(label);
   gtk_menu_append(GTK_MENU(zoom),label);
   gtk_signal_connect (GTK_OBJECT (label), "activate",
                       GTK_SIGNAL_FUNC (zoomin),
                       net);






   label = gtk_menu_item_new_with_label("Zoom Out");
   gtk_object_ref(GTK_OBJECT(label));
   gtk_object_set_data_full (GTK_OBJECT (zoom), "new",
                             label,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show(label);
   gtk_menu_append(GTK_MENU(zoom),label);
   gtk_signal_connect (GTK_OBJECT (label), "activate",
                       GTK_SIGNAL_FUNC (zoomout),
                       net);

   //gtk_menu_item_set_submenu (GTK_MENU_ITEM(zoom),label);





   label = gtk_menu_item_new_with_label("Properties");
   gtk_object_ref(GTK_OBJECT(label));
   gtk_object_set_data_full (GTK_OBJECT (menu), "new",
                             label,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show(label);
   gtk_menu_append(GTK_MENU(menu),label);

}

GUINetPopup::~GUINetPopup()
{
   gtk_widget_destroy(menu);
}

void GUINetPopup::addCategory(string name)
{
   //if the category has not been added yet
   if (categories.find(name) == categories.end())
   {
      int colonPos = name.rfind(":");
      //cerr << colonPos << endl;
      //cerr << "slashpos = " << slashpos << endl;
      string higher="";
      if (colonPos != -1)
	 higher.append(name,0,colonPos);
      string base=name;
      base.erase(0,colonPos+1);
      //cerr << higher << "/" << base << endl;

      GtkWidget *label = gtk_menu_item_new_with_label((gchar*)base.c_str());
      gtk_object_ref(GTK_OBJECT(label));
      gtk_object_set_data_full (GTK_OBJECT (menu), "new",
                                label,
                                (GtkDestroyNotify) gtk_widget_unref);
      gtk_widget_show(label);
      
      if (higher == "")
	 gtk_menu_append(GTK_MENU(newMenu),label);
      else 
      {
	 addCategory(higher);
	 gtk_menu_append(GTK_MENU(categories[higher]),label);
      }
      GtkWidget *newCat = gtk_menu_new();
      gtk_object_ref(GTK_OBJECT(newCat));
      gtk_object_set_data_full (GTK_OBJECT (label), "new",
                                newCat,
                                (GtkDestroyNotify) gtk_widget_unref);

      GtkWidget *tear = gtk_tearoff_menu_item_new();
      gtk_object_ref(GTK_OBJECT(tear));
      gtk_object_set_data_full (GTK_OBJECT (newCat), "new",
                                tear,
                                (GtkDestroyNotify) gtk_widget_unref);
      gtk_widget_show(tear);
      gtk_menu_append(GTK_MENU(newCat),tear);


      gtk_widget_show(newCat);
      gtk_menu_item_set_submenu (GTK_MENU_ITEM(label),newCat);
      categories[name] = newCat;
   }
}
static int a=0;
void GUINetPopup::addType(string cat, string type)
{
   addCategory(cat);
   GtkWidget *label = gtk_menu_item_new_with_label((gchar*)type.c_str());
   gtk_object_ref(GTK_OBJECT(label));
   gtk_object_set_data_full (GTK_OBJECT (menu), "new",
			     label,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show(label);
   gtk_signal_connect (GTK_OBJECT (label), "activate",
		       GTK_SIGNAL_FUNC (new_node_event),
		       net);
   gtk_object_set_user_data(GTK_OBJECT(label), (gchar*)type.c_str());
   gtk_menu_append(GTK_MENU(categories[cat]),label);
}

void GUINetPopup::popup(GdkEvent *event)
{
   //theTime = event->time;
   gnome_popup_menu_do_popup_modal (menu,NULL,NULL,&(event->button),NULL);
   //gnome_popup_menu_do_popup_modal(menu, NULL, NULL, event, NULL);
}







static void NodeLabel_class_init(NodeLabel_class *);
static void NodeLabel_init(NodeLabel *);


GtkType NodeLabel_get_type ()
{

   static GtkType doc_type = 0;
	
   if (!doc_type) {
	  	
      static const GtkTypeInfo doc_info = {
         "NodeLabel",
         sizeof (NodeLabel),
         sizeof (NodeLabel_class),
         (GtkClassInitFunc) NodeLabel_class_init,
         (GtkObjectInitFunc) NodeLabel_init,
         //(GtkArgSetFunc) NULL,
         //(GtkArgGetFunc) NULL,
         (gpointer) NULL,
         (gpointer) NULL,
	  
      };
	  
      doc_type = gtk_type_unique (gnome_mdi_child_get_type (), &doc_info);
	  
   }
	  
   return doc_type;
	
}


static void NodeLabel_destroy (GtkObject *obj)
{

   if (GTK_OBJECT_CLASS (parent_class)->destroy)
      (* GTK_OBJECT_CLASS (parent_class)->destroy)(GTK_OBJECT (obj));

}


static void NodeLabel_class_init (NodeLabel_class *klass)
{

   GtkObjectClass 		*object_class;
   GnomeMDIChildClass	*child_class;

   object_class = (GtkObjectClass*)klass;
   child_class = GNOME_MDI_CHILD_CLASS (klass);


   object_class->destroy = NodeLabel_destroy;	
   parent_class = (GtkMenuItemClass*)gtk_type_class (gtk_menu_item_get_type ());
	
}

void NodeLabel_init (NodeLabel *doc)
{

   //new(doc) NodeLabel("Untitled");
}

GtkWidget *node_item_new (gchar *label, UINetwork *net)
{

   GtkWidget *doc;
   if ((doc = (GtkWidget*)gtk_type_new (NodeLabel_get_type ())))
      return doc;
  
	
   g_print ("Eeek.. bork!\n");
   gtk_object_destroy (GTK_OBJECT(doc));
	
   return NULL;
	
}
