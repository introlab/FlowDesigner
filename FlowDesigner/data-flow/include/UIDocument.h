#ifndef UIDOCUMENT_H
#define UIDOCUMENT_H

//#include "UINetwork.h"
#include <vector>
#include <string>
#include <tree.h>
#include <map>
#include "UINetwork.h"

//class UINetwork;
class UINodeMenu;
class Network;
class ParameterSet;

class ItemInfo {
  public:
	string name;
	string type;
	string value;
	string description;
	
	ItemInfo() : type("ANY"), value(""), description("No description available") {}
};

class SubnetInfo {
  public:
   vector<ItemInfo *> inputs;
   vector<ItemInfo *> outputs;
   vector<ItemInfo *> params;
   string category;
   string description;
  public:
   SubnetInfo() : category("Unknown"), description("No description available") {}
};

class DocParameterDataText {
  public:
   string name;
   string value;
   string type;
};

class UIDocument {//: public GnomeMDIChild{
   
protected:
   //GnomeMDIChild child; //this must be first!!!

   //GtkWidget *view;
   vector<UINetwork *> networks;
   bool modified;
   
   static map<string, SubnetInfo *> externalDocInfo;

   map<string, SubnetInfo *> preloadInfo;
   vector<DocParameterDataText *> textParams;

   string docName;
   string path;
   bool untitled;

public:
   UIDocument(string _name);

   virtual ~UIDocument();

   virtual void load();

   virtual void loadXML(xmlNodePtr root);

   virtual void loadFromMemory(char *mem, int size);
   
   void setModified() {modified=true;}
   
   void resetModified() {modified=false;}
   
   bool isModified() {return modified;}
   
   UINetwork *addNetwork(string name, UINetwork::Type type);
   
   UINetwork *addNetwork(xmlNodePtr xmlNet);
   
   void removeNetwork(UINetwork *toRemove);
   
   bool isUntitled() {return untitled;}
   
   virtual const string &getName() {return docName;}
   
   const string &getPath() {return path;}
   
   void save();
   
   void export2net();
   
   void addParameterText(string name, string value, string type);
   
   UINetwork *getNetworkNamed(const string &n);
   
   vector<ItemInfo *> getNetInputs(const string &netName);
   
   vector<ItemInfo *> getNetOutputs(const string &netName);
   
   vector<ItemInfo *> getNetParams(const string &netName);
   
   string getDescription(const string &type);
   
   static void loadNetInfo(xmlNodePtr net, map<string, SubnetInfo *> &infoMap, string netName = "");

   void loadAllSubnetInfo(xmlNodePtr net);

   static void scanDL();

   static void loadNodeDefInfo(const string &path, const string &name);

   static void loadExtDocInfo(const string &path, const string &name);
   
   vector<UINetwork *> get_networks() {return networks;}
   vector<DocParameterDataText *> get_textParams() {return textParams;}

   static void loadAllInfo();
   
   virtual UINetwork *newNetwork(UIDocument *_doc, const string &_name, UINetwork::Type type);
   
   virtual UINetwork *newNetwork(UIDocument *_doc, xmlNodePtr _net);
   
   Network *build(const string &_name, const ParameterSet &params);

   static Network *buildExternal(const string &type, const string &_name, const ParameterSet &params);

   virtual void run();

   virtual void run(ParameterSet &p);

   virtual void setFullPath(const string &fullpath);
   
   vector<string> getAvailableNodes();
   
   
};




#endif
