#ifndef UILINK_H
#define UILINK_H

using namespace std;

#include <list>
#include "misc.h"
#include <math.h>

//#include <gnome.h>
#include <gnome-xml/tree.h>

class UINode;
class UINetwork;
class UITerminal;
//struct xmlNode;

class Network;

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
  
  void setxy(double _x, double _y) {
    x = _x;
    y = _y;
  }

  double dist (const GUILinkPoint &p) {
    return sqrt(sqr(p.x - x) + sqr(p.y -y));
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


class UILink {
protected:
   double x1,y1,x2,y2;
   UITerminal *from;
   UITerminal *to;
   bool complete;
   UINetwork *net;
   list<GUILinkPoint*> m_points;

public:
   //UILink(UITerminal *_from, UITerminal *_to, double _x1, double _y1, double _x2, double _y2);
   UILink(UITerminal *_from, UITerminal *_to, char *points_str=NULL);

   virtual ~UILink();

   void saveXML(xmlNode *root);

   void build(Network *net);

   friend class UITerminal;
   
   UITerminal *getFromTerminal() {return from;}
   UITerminal *getToTerminal() {return to;}
};

#endif
