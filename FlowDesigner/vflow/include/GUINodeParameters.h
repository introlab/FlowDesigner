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


class GUINodeParameters : public UINodeParameters{
protected:
   GtkWidget *nodeproperty;
   vector<ParameterData> params;

public:
   GUINodeParameters(UINode *_node, string type);
   ~GUINodeParameters();
   void show();
   void hide();
   void apply();
   void changed();
   void insertNetParams(vector<string> &par);
   ParameterData *getParamDataNamed(string n);
   void insertLoadedParam(ParameterText *param, string type, string value);

};


#endif
