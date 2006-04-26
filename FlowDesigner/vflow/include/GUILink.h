// Copyright (C) 2001 Jean-Marc Valin

#ifndef GUILINK_H
#define GUILINK_H

#include <gnome.h>
//#include <tree.h>
#include "UILink.h"
#include <math.h>

namespace FD {

class UINode;
class UINetwork;
class UITerminal;


class GUILink : public UILink  {

protected:

   GnomeCanvasItem *item;

   GnomeCanvasGroup *group;
   
   const double MIN_POINT_DISTANCE;




public:
   GUILink(UITerminal *_from, UITerminal *_to, const char *points_str=NULL);

   ~GUILink();

   gint event(GdkEvent *event);

   void grab(guint32 etime);

   void move (bool isInput, double dx,double dy);

   void update();


};

}//namespace FD

#endif
