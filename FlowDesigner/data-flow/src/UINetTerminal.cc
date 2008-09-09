// Copyright (C) 2001 Jean-Marc Valin

#include "UINetTerminal.h"
#include "UITerminal.h"
#include "UINode.h"
#include "UINetwork.h"
#include <iostream>

using namespace std;

namespace FD {

UINetTerminal::UINetTerminal(UITerminal *_terminal, NetTermType _type, const string &_name, 
			     const string &_objType, const string &_description)
   : name(_name)
   , m_objectType(_objType)
   , m_description (_description)
   , terminal(_terminal)
   , type(_type)
{
   terminal->getNode()->getNetwork()->addTerminal(this);
   terminal->connectNetTerminal (this);
}

UINetTerminal::~UINetTerminal()
{
  terminal->getNode()->getNetwork()->removeTerminal(this);
  terminal->disconnectNetTerminal();
}


void UINetTerminal::saveXML(xmlNode *root)
{
   xmlNodePtr tree;

   if (type == INPUT)
      tree = xmlNewChild(root, NULL, (xmlChar *)"NetInput", NULL);
   else if (type == OUTPUT)
      tree = xmlNewChild(root, NULL, (xmlChar *)"NetOutput", NULL);
   else
      tree = xmlNewChild(root, NULL, (xmlChar *)"NetCondition", NULL);

   xmlSetProp(tree, (xmlChar *)"name", (xmlChar *)name.c_str());
   xmlSetProp(tree, (xmlChar *)"node", (xmlChar *)terminal->getNode()->getName().c_str());
   xmlSetProp(tree, (xmlChar *)"terminal", (xmlChar *)terminal->getName().c_str());
   
   //(DL 12/12/2003) Input & Output ObjectType & Description
   if (type == INPUT || type == OUTPUT) {
     xmlSetProp(tree, (xmlChar *)"object_type", (xmlChar *)m_objectType.c_str());
     xmlSetProp(tree, (xmlChar *)"description", (xmlChar *)m_description.c_str());
   }

}

}//namespace FD
