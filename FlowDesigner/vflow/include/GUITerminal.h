#ifndef GUITERMINAL_H
#define GUITERMINAL_H

#include <gnome.h>
#include <vector>
#include <string>
#include <fstream.h>
#include "UITerminal.h"

class UINode;
class UILink;
class UINetTerminal;
class ItemInfo;

class GUITerminal : public UITerminal {
protected:

   /**The item that corresponds to the terminal*/
   GnomeCanvasItem *item;


public:

   GnomeCanvasItem * getItem() {return item;}

   GUITerminal (ItemInfo *terminalInfo, UINode *_node, bool _isInput, double _x, double _y);

   ~GUITerminal() {}

   gint event(GdkEvent *event);

   /**moves the terminal by (dx,dy)*/
   void move (double dx,double dy);

   /**distance to the point (wx,wy)*/
   double dist(double &wx, double &wy) 
   {
      double ix=x;
      double iy=y;
      gnome_canvas_item_i2w(item->parent, &ix, &iy);
      return sqrt((ix-wx)*(ix-wx) + (iy-wy)*(iy-wy));
   }

   /**returns the position in world coord*/
   void getPos(double &wx, double &wy)
   {
      wx=x;
      wy=y;
      gnome_canvas_item_i2w(item->parent, &wx, &wy);
   }

   void updatePos() {

     double x1,y1,x2,y2;
     
     
     gnome_canvas_item_get_bounds(item,&x1,&y1,&x2,&y2);
     
     x = (x1+x2) /2.0;
     y = (y1+y2) /2.0;
     


   }



};


#endif
