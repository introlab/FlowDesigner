#ifndef GUILINK_H
#define GUILINK_H

#include <gnome.h>
#include <gnome-xml/tree.h>
#include "UILink.h"
#include <list>
#include <math.h>


using namespace std;

class UINode;
class UINetwork;
class UITerminal;

class GUILinkPoint {

 public:

  GUILinkPoint(double _x, double _y)
    : x(_x), y(_y) {}

  GUILinkPoint(const GUILinkPoint &cpy)
    : x(cpy.x), y(cpy.y) {}

  GUILinkPoint & operator= (const GUILinkPoint &eq) {
    
    x = eq.x; 
    y = eq.y;
    return *this;
  }
  
  double dist (const GUILinkPoint &p) {
    return sqrt(pow(p.x - x,2) + pow(p.y -y,2));
  }
  
  bool between (const GUILinkPoint &p1, const GUILinkPoint &p2) {

    //cout<<"p1 x "<<p1.x<<" y "<<p1.y<<endl;
    //cout<<"p2 x "<<p2.x<<" y "<<p2.y<<endl;
    
    double dx1 = x - p1.x;
    double dy1 = y - p1.y;
    double delta1 = dx1 / dy1;

    double dx2 = p2.x - x;
    double dy2 = p2.y - y;
    double delta2 = dx2 / dy2;

    //cout<<"delta1 : "<<delta1<<endl;
    //cout<<"delta2 : "<<delta2<<endl;

    //accepting 10% deviation
    if (delta1 / delta2 > 0.90 &&
	delta1 / delta2 < 1.10) {
      return true;
    }
    else {
      return false;
    }


  }


  double x;
  double y;

};


class GUILink : public UILink  {

protected:

   GnomeCanvasItem *item;

   GnomeCanvasGroup *group;
   
   const double MIN_POINT_DISTANCE;




public:
   GUILink(UITerminal *_from, UITerminal *_to);

   ~GUILink();

   gint event(GdkEvent *event);

   void grab(guint32 etime);

   void move (bool isInput, double dx,double dy);

   list<GUILinkPoint*> m_points;


};


#endif
