// Copyright (C) 2001 Jean-Marc Valin

#ifndef UINETTERMINAL_H
#define UINETTERMINAL_H

//#include <gnome.h>
#include <string>
#include <libxml/tree.h>

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
   string m_objectType;
   string m_description;

   UITerminal *terminal;
   double x,y;
   NetTermType type;


public:

   UINetTerminal(UITerminal *_terminal, NetTermType _type, const string &_name, 
		 const string &_objType = "any", const string &_description = "No description available");

   //void setup();
   virtual ~UINetTerminal();

   void setName(const string &_name) {name = _name;}
   string getName() {return name;}

   void setObjectType(const string &_objType) {m_objectType = _objType;}
   string  getObjectType(){return m_objectType;}

   void setDescription(const string &description) {m_description = description;}
   string getDescription(){return m_description;}

   void saveXML(xmlNode *root);
   NetTermType getType() {return type;}
   UITerminal *getTerminal() {return terminal;}

};


#endif
