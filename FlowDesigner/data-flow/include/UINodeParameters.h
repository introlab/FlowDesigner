// Copyright (C) 2001 Jean-Marc Valin

#ifndef UINODEPARAMETERS_H
#define UINODEPARAMETERS_H



//#include <gnome.h>
#include <string>
#include <vector>
#include <libxml/tree.h>

class UINode;
class ParameterSet;
class ItemInfo;

//FIXME: Should replace with ItemInfo
class ParameterText 
{
   public:
      std::string name;
      std::string value;
      std::string type;	
      std::string description;
};

class UINodeParameters 
{
   protected:
      std::vector<ParameterText *> textParams;
      std::string comments;
      UINode *node;

   public:
      UINodeParameters(UINode *_node, std::string type);
      virtual ~UINodeParameters();
      void saveXML(xmlNode *root);
      void export2net(std::ostream &out);
      void load(xmlNodePtr node);
      ParameterText *getParamNamed(std::string n);
      void insertNetParams(std::vector<ItemInfo *> &par);

      void updateNetParams(std::vector<ItemInfo *> &par);

      virtual void insertLoadedParam(ParameterText *param, std::string type, std::string value);
      ParameterText *addParameterText(std::string name, std::string type, std::string value,
								   std::string description);
      void removeParameterText(std::string nameToRemove);
   
      ParameterSet *build(const ParameterSet &par);

      void copyParameterText(UINodeParameters *cpy);
   
      void genCode(std::ostream &out);

      const std::string &getComments() {return comments;}
      void setComments(const std::string &_comments) {comments = _comments;}

      std::vector<ParameterText *> &get_textParams() {return textParams;}   
      UINode *getUINode() {return node;}

};


#endif
