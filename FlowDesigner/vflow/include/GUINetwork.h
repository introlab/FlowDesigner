#ifndef GUINETWORK_H
#define GUINETWORK_H

#include "UINetwork.h"
#include <gnome.h>

class UIDocument;
class GUINetPopup;


class GUINetwork : public UINetwork {
protected:
   GnomeCanvas *canvas;
   GnomeCanvasGroup *group;
   GUINetPopup *popup;
   double zoom;
public:
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

   virtual UITerminal *newTerminal (string _name, UINode *_node, bool _isInput, double _x, double _y);

   virtual UILink *newLink (UITerminal *_from, UITerminal *_to);

   virtual UINetTerminal *newNetTerminal (UITerminal *_terminal, UINetTerminal::NetTermType _type, string _name);

   void zoomIn() {zoom *= 1.5; gnome_canvas_set_pixels_per_unit (canvas,zoom);}

   void zoomOut() {zoom /= 1.5; gnome_canvas_set_pixels_per_unit (canvas,zoom);}

   double getZoom() {return zoom;}
   //virtual void tata() {};
};

#endif
