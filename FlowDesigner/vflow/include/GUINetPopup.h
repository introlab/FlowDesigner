// Copyright (C) 2001 Jean-Marc Valin

#ifndef GUINETPOPUP_H
#define GUINETPOPUP_H

#include <gnome.h>
#include <map>
#include <string>


class UINetwork;
class UIDocument;

class GUINetPopup {
protected:
   UIDocument *doc;
   UINetwork *net;
   GtkWidget *menu;
   GtkWidget *newMenu;
   std::map<std::string,GtkWidget *> categories;
public:
   GUINetPopup(UIDocument *_doc, UINetwork *_net);
   ~GUINetPopup();
   void popup(GdkEvent *event);
   void addCategory(std::string name);
   void addType(std::string cat, std::string type);
};





class NodeLabel {
protected:
   GtkMenuItem item;
   UINetwork *net;
public:
};

class NodeLabel_class {
   GtkMenuItemClass parent_class;
};


#define NODELABEL(obj)	         GTK_CHECK_CAST (obj, NodeLabel_get_type (), NodeLabel)
#define NODELABEL_CLASS(klass)  GTK_CHECK_CLASS_CAST (klass, NodeLabel_get_type (), NodeLabel_class)
#define IS_NODELABEL(obj)	 GTK_CHECK_TYPE (obj, NodeLabel_get_type ())
GtkType NodeLabel_get_type ();
GtkWidget *node_item_new (gchar *label, UINetwork *net);
//NodeLabel *NodeLabel_new ();

#endif
