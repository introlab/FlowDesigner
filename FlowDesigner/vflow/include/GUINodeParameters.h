// Copyright (C) 2001 Jean-Marc Valin

#ifndef GUINODEPARAMETERS_H
#define GUINODEPARAMETERS_H

#include <gnome.h>
#include <string>
#include <vector>
#include <tree.h>
#include "UINodeParameters.h"
#include "GUINode.h"

class UINode;

class ParameterData {
public:
   
   GtkWidget *optionmenu;
   GtkWidget *optionmenu_menu;
   GtkWidget *combo_entry;
   GtkWidget *entry;
   GtkWidget *label;

};


class GUINodeParameters {
protected:
   GtkWidget *nodeproperty;
   UINodeParameters *nodeParams;
   vector<ParameterData> params;
   vector<ParameterText *> &textParams;
   GUINode *node;
   GtkWidget *text_comments;

public:
   GUINodeParameters(GUINode *_node, string type, UINodeParameters *_nodeParams);
   ~GUINodeParameters();
   void show();
   void hide();
   void apply();
   void changed();
   void insertNetParams(vector<string> &par);
   ParameterData *getParamDataNamed(string n);
   void insertLoadedParam(ParameterText *param, string type, string value);
   GUINode *getGUINode() {return node;}

};


#endif
