// Copyright (C) 2001 Jean-Marc Valin

#ifndef GUINETWORK_H
#define GUINETWORK_H

#include "UINetwork.h"
#include "GUINode.h"
#include <gnome.h>

class UIDocument;
class GUINetPopup;
class GUINodeTooltip;

class GUINetwork : public UINetwork {

  friend void network_description_changed_event (GtkTextBuffer *textbuffer, GUINetwork *network);

protected:
   GnomeCanvas *canvas;
   GtkWidget *scrolledwindow1;
   GtkWidget *networkProperties;
   GnomeCanvasGroup *group;
   GUINetPopup *popup;
   double zoom;

   list<GUINode*> selectedNodes;
   GUINodeTooltip *tooltip;

   double x_last_click;
   double y_last_click;

public:

   //useful to know where we clicked on the canvas.
   void get_last_click(double &x, double &y) {x = x_last_click; y = y_last_click;}

   void emptySelectedNodes();

   void addSelectedNode(GUINode *node);

   void removeSelectedNode(GUINode *node);

   list<GUINode*>& getSelectedNodes(){return selectedNodes;}

   void moveSelectedNodes(double dx, double dy);

   void clearSelectedNodes();

   bool isNodeSelected(GUINode *node);

   GUINetwork(UIDocument *_doc, string _name, Type _type);

   GUINetwork(UIDocument *_doc, xmlNodePtr net);

   ~GUINetwork();

   UINode *addNode (string type, double xx, double yy);

   void create();

   GnomeCanvasGroup * getGroup() {return group;}
   GnomeCanvas * getCanvas() {return canvas;}

   GtkWidget* getView(){return scrolledwindow1;}

   void updateScroll ();

   gboolean buttonEvent(GdkEvent *event);

   void newNetNotify(const string &cat, const string &type);

   UITerminal *isNearInputTerminal (double &x, double &y);

   UITerminal *isNearOutputTerminal (double &x, double &y);

   virtual UINode *newNode(UINetwork* _net, xmlNodePtr def);

   virtual UINode *newNode(UINetwork* _net, string _name, string _type, double _x, double _y, bool doInit);

//   virtual UITerminal *newTerminal (string _name, UINode *_node, bool _isInput, double _x, double _y);

   virtual UILink *newLink (UITerminal *_from, UITerminal *_to, char *str);

   virtual UINetTerminal *newNetTerminal (UITerminal *_terminal, UINetTerminal::NetTermType _type, const string &_name ,
					  const string &_objType="any", const string &_description="No description available");

   void zoomIn() {zoom *= 1.5; gnome_canvas_set_pixels_per_unit (canvas,zoom);}

   void zoomOut() {zoom /= 1.5; gnome_canvas_set_pixels_per_unit (canvas,zoom);}

   double getZoom() {return zoom;}
   //virtual void tata() {};

   void popTooltip(GUINode *node);

   void redraw() {gtk_widget_queue_draw(GTK_WIDGET(canvas));}

   void get_scroll_region(double &x1, double &y1, double &x2, double &y2) { 
     gnome_canvas_get_scroll_region  (GNOME_CANVAS(canvas),&x1,&y1,&x2,&y2);
   }
   
   void get_scroll_offsets(int &c1, int &c2) {
     gnome_canvas_get_scroll_offsets (GNOME_CANVAS(canvas),&c1,&c2);
   }
   
   virtual void rename(string newName);
   

   void showProperties();

   void applyNetworkProperties();
};

#endif
