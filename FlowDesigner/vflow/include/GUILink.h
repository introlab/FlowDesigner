// Copyright (C) 2001 Jean-Marc Valin

#ifndef GUILINK_H
#define GUILINK_H

#include <gnome.h>
#include <tree.h>
#include "UILink.h"
#include <math.h>


using namespace std;

class UINode;
class UINetwork;
class UITerminal;


class GUILink : public UILink  {

protected:

   GnomeCanvasItem *item;

   GnomeCanvasGroup *group;
   
   const double MIN_POINT_DISTANCE;
   const double MAX_POINT_DISTANCE;




public:
   GUILink(UITerminal *_from, UITerminal *_to, char *points_str=NULL);

   ~GUILink();

   gint event(GdkEvent *event);

   void grab(guint32 etime);

   void move (bool isInput, double dx,double dy);

   void update();


};


#endif
