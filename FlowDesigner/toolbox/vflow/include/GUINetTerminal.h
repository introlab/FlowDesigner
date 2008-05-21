// Copyright (C) 2001 Jean-Marc Valin

#ifndef GUINETTERMINAL_H
#define GUINETTERMINAL_H

#include <gnome.h>
#include <string>
#include <libxml/tree.h>
#include "UINetTerminal.h"

namespace FD {

class UINode;
class UINetwork;
class UITerminal;
//struct xmlNode;

class GUINetTerminal : public UINetTerminal {

public:
   //enum NetTermType {INPUT, OUTPUT, CONDITION};

protected:
   GnomeCanvasItem *item;
      
public:

   GUINetTerminal(UITerminal *_terminal, NetTermType _type, const std::string &_name,
		  const std::string &_objType = "any", const std::string &_description = "No description available");
   void setup();
   ~GUINetTerminal();
   gint event(GdkEvent *event);
   std::string find_unique_name(const std::string &_name, NetTermType _type);
   void setAbsPos(double x, double y);

};

}//namespace FD

#endif
