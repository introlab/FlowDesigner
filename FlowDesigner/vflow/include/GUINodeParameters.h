// Copyright (C) 2001 Jean-Marc Valin

#ifndef GUINODEPARAMETERS_H
#define GUINODEPARAMETERS_H

#include <gnome.h>
#include <string>
#include <vector>
//#include <tree.h>
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

/*
  (DL)
  Now deriving from UINodeParameters, July 16 2003
*/

class GUINodeParameters : public UINodeParameters{

protected:
   GtkWidget *nodeproperty;
   vector<ParameterData> params;
   GtkWidget *text_comments;
   GtkWidget *list1;
   GtkWidget *list2;
   GtkWidget *input_entry, *output_entry;
   GtkWidget *inputSelect, *outputSelect;
   void createWindow();

public:
   GUINodeParameters(UINode *_node, string type);
   virtual ~GUINodeParameters();
   void show();
   void hide();
   void apply();
   void changed();
   ParameterData *getParamDataNamed(string n);
   //virtual void insertLoadedParam(ParameterText *param, string type, string value);
   GUINode *getGUINode() {return dynamic_cast<GUINode*>(node);}
   void addInput();
   void addOutput();
   void removeInput();
   void removeOutput();
   void setInputSelect(GtkWidget *w) {inputSelect=w;}
   void setOutputSelect(GtkWidget *w) {outputSelect=w;}
   void unsetInputSelect(GtkWidget *w) {if (inputSelect==w) inputSelect=NULL;}
   void unsetOutputSelect(GtkWidget *w) {if (outputSelect==w) outputSelect=NULL;}
};
#endif
