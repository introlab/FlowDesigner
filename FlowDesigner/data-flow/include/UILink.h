#ifndef UILINK_H
#define UILINK_H

//#include <gnome.h>
#include <gnome-xml/tree.h>

class UINode;
class UINetwork;
class UITerminal;
//struct xmlNode;

class Network;

class UILink {
protected:
   double x1,y1,x2,y2;
   UITerminal *from;
   UITerminal *to;
   bool complete;
   UINetwork *net;
public:
   //UILink(UITerminal *_from, UITerminal *_to, double _x1, double _y1, double _x2, double _y2);
   UILink(UITerminal *_from, UITerminal *_to);

   virtual ~UILink();

   void saveXML(xmlNode *root);

   void build(Network *net);

   friend class UITerminal;
   
   UITerminal *getFromTerminal() {return from;}
   UITerminal *getToTerminal() {return to;}
};

#endif
