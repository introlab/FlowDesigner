// Copyright (C) 2001 Jean-Marc Valin

#ifndef UINETTERMINAL_H
#define UINETTERMINAL_H

//#include <gnome.h>
#include <string>
#include <tree.h>

using namespace std;

class UINode;
class UINetwork;
class UITerminal;
//struct xmlNode;

class UINetTerminal {
public:
   enum NetTermType {INPUT, OUTPUT, CONDITION};

protected:
   string name;
   UITerminal *terminal;
   double x,y;
   NetTermType type;
public:
   UINetTerminal(UITerminal *_terminal, NetTermType _type, string _name);
   //void setup();
   virtual ~UINetTerminal();
   void setName(const string &_name);
   string getName() {return name;}
   void saveXML(xmlNode *root);
   NetTermType getType() {return type;}
   UITerminal *getTerminal() {return terminal;}

};


#endif
