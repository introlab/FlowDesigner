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
protected:
   GnomeCanvas *canvas;
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

   void updateScroll ();

   gboolean buttonEvent(GdkEvent *event);

   void newNetNotify(const string &cat, const string &type);

   UITerminal *isNearInputTerminal (double &x, double &y);

   UITerminal *isNearOutputTerminal (double &x, double &y);

   virtual UINode *newNode(UINetwork* _net, xmlNodePtr def);

//   virtual UITerminal *newTerminal (string _name, UINode *_node, bool _isInput, double _x, double _y);

   virtual UILink *newLink (UITerminal *_from, UITerminal *_to, char *str);

   virtual UINetTerminal *newNetTerminal (UITerminal *_terminal, UINetTerminal::NetTermType _type, string _name);

   void zoomIn() {zoom *= 1.5; gnome_canvas_set_pixels_per_unit (canvas,zoom);}

   void zoomOut() {zoom /= 1.5; gnome_canvas_set_pixels_per_unit (canvas,zoom);}

   double getZoom() {return zoom;}
   //virtual void tata() {};

   void popTooltip(GUINode *node);

   void redraw() {gtk_widget_queue_draw(GTK_WIDGET(canvas));}
};

#endif
