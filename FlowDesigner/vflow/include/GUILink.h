#ifndef GUILINK_H
#define GUILINK_H

#include <gnome.h>
#include <gnome-xml/tree.h>
#include "UILink.h"


class UINode;
class UINetwork;
class UITerminal;
//struct xmlNode;

class GUILink : public UILink  {
protected:
   GnomeCanvasItem *item;
   GnomeCanvasGroup *group;

public:
   GUILink(UITerminal *_from, UITerminal *_to);

   ~GUILink();

   gint event(GdkEvent *event);

   void grab(guint32 etime);

   void move (bool isInput, double dx,double dy);

};


#endif
