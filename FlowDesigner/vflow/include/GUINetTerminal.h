// Copyright (C) 2001 Jean-Marc Valin

#ifndef GUINETTERMINAL_H
#define GUINETTERMINAL_H

#include <gnome.h>
#include <string>
#include <tree.h>
#include "UINetTerminal.h"

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
   GUINetTerminal(UITerminal *_terminal, NetTermType _type, string _name);
   void setup();
   ~GUINetTerminal();
   gint event(GdkEvent *event);
   string find_unique_name(const string &_name, NetTermType _type);

};


#endif
