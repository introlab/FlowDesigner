// Copyright (C) 2001 Jean-Marc Valin

#ifndef UILINK_H
#define UILINK_H

#include <list>
#include "misc.h"
#include <math.h>

//#include <gnome.h>
#include <libxml/tree.h>

#include <ostream>

namespace FD {

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
     //Maximum offset allowed
     const float moff=2;
     //If x is between p1.x and p2.x, within a margin of moff
     if ( ! ((x-moff < p1.x && x+moff > p2.x) || (x+moff > p1.x && x-moff < p2.x)))
	return false;

     //If y is between p1.y and p2.y, within a margin of moff
     if ( ! ((y-moff < p1.y && y+moff > p2.y) || (y+moff > p1.y && y-moff < p2.y)))
	return false;

     //Just some geometry
     double dx, dy, rx, ry;
     dx=x-p1.x; dy=y-p1.y;
     rx=p2.x-p1.x; ry=p2.y-p1.y;
     //cerr << "(" << dx << ", " << dy << ") vs. (" << rx << ", " << ry << ")" << endl;
     //d*r
     double prod = dx*rx + dy*ry;
     //cerr << "prod = " << prod << endl;
     // 1/(norm)2 of R
     double normR_2=1/(.001+rx*rx+ry*ry);
     //cerr << "norm = " << 1/normR_2 << endl;
     //projection of d on r
     double px,py;
     px=rx*prod*normR_2; py=ry*prod*normR_2;

     //distance:
     double dist = sqrt(sqr(px-dx) + sqr(py-dy));
     //cerr << "dist = " << dist << endl << endl;
     return (dist < moff);
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
   //UINetwork *net;
   std::list<GUILinkPoint*> m_points;
   int id;

public:
   //UILink(UITerminal *_from, UITerminal *_to, double _x1, double _y1, double _x2, double _y2);
   UILink(UITerminal *_from, UITerminal *_to, const char *points_str=NULL, int _id = 0);

   virtual ~UILink();

   virtual void saveXML(xmlNode *root, int newId);

   virtual void build(Network *net);

   virtual void genCode(std::ostream &out);

   std::list<GUILinkPoint*> & get_link_points() {return m_points;}

   friend class UITerminal;

   UITerminal *getFromTerminal() {return from;}
   UITerminal *getToTerminal() {return to;}

   UINetwork *getNetwork();

   int getId() const {return id;}
};

}//namespace FD

#endif
