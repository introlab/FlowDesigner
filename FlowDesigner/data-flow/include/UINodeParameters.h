#ifndef UINODEPARAMETERS_H
#define UINODEPARAMETERS_H

using namespace std;

//#include <gnome.h>
#include <string>
#include <vector>
#include <tree.h>

class UINode;
class ParameterSet;
class ItemInfo;

class ParameterText {
public:
   string name;
   string value;
   string type;	
   string description;
};

class UINodeParameters {
protected:
   vector<ParameterText *> textParams;
   UINode *node;
public:
   UINodeParameters(UINode *_node, string type);
   virtual ~UINodeParameters();
   void saveXML(xmlNode *root);
   void export2net(ostream &out);
   void load(xmlNodePtr node);
   ParameterText *getParamNamed(string n);
   static const vector<string> &allTypes();
   void insertNetParams(vector<ItemInfo *> &par);
   virtual void insertLoadedParam(ParameterText *param, string type, string value);
   ParameterText *addParameterText(string name, string type, string value,
								   string description);
   void removeParameterText(string nameToRemove);
   
   ParameterSet *build(const ParameterSet &par);
   
   vector<ParameterText *> &get_textParams() {return textParams;}   
   UINode *getUINode() {return node;}

};


#endif
