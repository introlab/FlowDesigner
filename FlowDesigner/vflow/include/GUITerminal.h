#ifndef GUITERMINAL_H
#define GUITERMINAL_H

#include <gnome.h>
#include <vector>
#include <string>
#include <fstream.h>
#include "UITerminal.h"
#include "GUILink.h"

class UINode;
class UILink;
class UINetTerminal;
class ItemInfo;

class GUITerminal : public UITerminal {
protected:

   /**The item that corresponds to the terminal*/
   GnomeCanvasItem *item;

   GnomeCanvasItem *item_text;

   bool hidden;


public:

   void showName() {
     gnome_canvas_item_show(item_text);
     hidden = false;
   }

   void hideName() {
     gnome_canvas_item_hide(item_text);
     hidden = true;
   }

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

   void setAbsPos(double x1, double y1) {

     double dx = x1 - x;
     double dy = y1 - y;
     
     x = x1;
     y = y1;


     gnome_canvas_item_set(item,
			   "x1",x1 - 2,
			   "y1",y1 - 2,
			   "x2",x1 + 2,
			   "y2",y1 + 2,NULL);

     
     if (isInput) {
       gnome_canvas_item_set(item_text,
			     "x",x + 2,
			     "y",y - 0.5,NULL);
     }
     else {
       gnome_canvas_item_set(item_text,
			     "x",x - 2,
			     "y",y - 0.5,NULL);
     }
			 

     move(dx,dy);


   }


   double getWidth() {

     double x1,y1,x2,y2;

     if (!hidden) {
       gnome_canvas_item_get_bounds(item_text,&x1,&y1,&x2,&y2);
       
       return x2 - x1 + 2;
     }
     else {
       return 2.0;
     }
   }
   


};


#endif
