// Copyright (C) 2001 Jean-Marc Valin

#ifndef UINETTERMINAL_H
#define UINETTERMINAL_H

//#include <gnome.h>
#include <string>
#include <libxml/tree.h>

namespace FD {

class UINode;
class UINetwork;
class UITerminal;
//struct xmlNode;

class UINetTerminal {
public:
   enum NetTermType {INPUT, OUTPUT, CONDITION};

protected:
   std::string name;
   std::string m_objectType;
   std::string m_description;

   UITerminal *terminal;
   double x,y;
   NetTermType type;


public:

   UINetTerminal(UITerminal *_terminal, NetTermType _type, const std::string &_name, 
		 const std::string &_objType = "any", const std::string &_description = "No description available");

   //void setup();
   virtual ~UINetTerminal();

   void setName(const std::string &_name) {name = _name;}
   std::string getName() const {return name;}

   void setObjectType(const std::string &_objType) {m_objectType = _objType;}
   std::string  getObjectType(){return m_objectType;}

   void setDescription(const std::string &description) {m_description = description;}
   std::string getDescription() const {return m_description;}

   void saveXML(xmlNode *root);
   NetTermType getType() const {return type;}
   UITerminal *getTerminal() {return terminal;}

};

}//namespace FD
#endif
