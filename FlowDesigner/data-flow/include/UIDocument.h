// Copyright (C) 2001 Jean-Marc Valin

#ifndef UIDOCUMENT_H
#define UIDOCUMENT_H

#include <vector>
#include <string>
#include <tree.h>
#include <map>
#include "UINetwork.h"
#include "UINodeParameters.h"
#include <set>

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
   string sourceFile;
   string requireList;
  public:
   SubnetInfo() : category("Unknown"), description("No description available") {}
};

class DocParameterDataText {
  public:
   string name;
   string value;
   string type;
};

/**GUI-independent memory representation for an XML Overflow document
   @author: Jean-Marc Valin
*/
class UIDocument {
   
protected:
   /**Pointers to all networks included (defined) in the document*/
   vector<UINetwork *> networks;

   /**Flag is set to true anytime there's a change that hasn't been saved*/
   bool modified;
   
   /**Info about all internal networks that can be included as nodes - I think (should be cleaned up)*/
   map<string, SubnetInfo *> preloadInfo;

   /**Document parameters (should be re-written)*/
   vector<DocParameterDataText *> textParams;

   /**Input for the document if used as an external node*/
   vector<ParameterText *> docInputs;

   /**Output for the document if used as an external node*/
   vector<ParameterText *> docOutputs;

   /**Parameters for the document if used as an external node*/
   vector<ParameterText *> docParams;

   /**File name (no path)*/
   string docName;
   
   /**Document path*/
   string path;
   
   /**True if document has no real name yet*/
   bool untitled;

   /**List of all files required for each module*/
   static map<string, set<string> > moduleDepend;
   
   /**List of all modules required for each file*/
   static map<string, set<string> > fileDepend;
 
   /**List of all headers required for each file*/
   static map<string, set<string> > headerDepend;

public:
   static map<string, SubnetInfo *> externalDocInfo;

   /**Document constructor with name, DOES NOT load the document (document created as untitled)*/
   UIDocument(string _name);

   /**Virtual destructor*/
   virtual ~UIDocument();

   /**Explicitly loads a document using the name specified at construction*/
   virtual void load();

   /**Loads the document from a parsed XML tree*/
   virtual void loadXML(xmlNodePtr root);

   /**Loads the document from memory*/
   virtual void loadFromMemory(char *mem, int size);
   
   /**Sets the 'modified' flag*/
   void setModified() {modified=true;}
   
   /**Resets the 'modified' flag*/
   void resetModified() {modified=false;}
   
   /**Has the document been modified since last saved?*/
   bool isModified() {return modified;}
   
   /**Add a network to the document*/
   UINetwork *addNetwork(string name, UINetwork::Type type);
   
   /**Add a network to the document from an XML node (used in load())*/
   UINetwork *addNetwork(xmlNodePtr xmlNet);
   
   /**Remove and destroy network from the document*/
   void removeNetwork(UINetwork *toRemove);
   
   /**Is the document untitled?*/
   bool isUntitled() {return untitled;}
   
   /**Document name (no path*/
   virtual const string &getName() {return docName;}
   
   /**Document path*/
   const string &getPath() {return path;}
   
   /**Save the document to file specified in internal variables 'path' and 'docName'*/
   void save();
   
   /**Save in old Overflow format (deprecated*/
   void export2net();
   
   void addParameterText(string name, string value, string type);
   
   UINetwork *getNetworkNamed(const string &n);
   
   vector<ItemInfo *> getNetInputs(const string &netName);
   
   vector<ItemInfo *> getNetOutputs(const string &netName);
   
   vector<ItemInfo *> getNetParams(const string &netName);
   
   string getDescription(const string &type);
   
   static void loadNetInfo(xmlNodePtr net, map<string, SubnetInfo *> &infoMap, string netName = "");

   void loadAllSubnetInfo(xmlNodePtr net);

   static void loadNodeDefInfo(const string &path, const string &name);

   static void loadExtDocInfo(const string &path, const string &name);
   
   vector<UINetwork *> get_networks() {return networks;}
   vector<DocParameterDataText *> get_textParams() {return textParams;}

   static void loadAllInfo();
   
   virtual UINetwork *newNetwork(const string &_name, UINetwork::Type type);
   
   virtual UINetwork *newNetwork(xmlNodePtr _net);
   
   Network *build(const string &_name, const ParameterSet &params);

   /**Generate C++ code for building the document, instead of using XML*/
   set<string> genCode(ostream &out, const string &functName, bool localIncludes=false);

   static Network *buildExternal(const string &type, const string &_name, const ParameterSet &params);

   virtual void run();

   virtual void run(ParameterSet &p);

   virtual void setFullPath(const string &fullpath);
   
   vector<string> getAvailableNodes();

   static string findExternal(const string &filename, char *searchPath="VFLOW_PATH", bool include_home=true, bool fullPathOutput=true);

   static void genCodeExternal(const string &type, ostream &out, int &id, set<string> &nodeList);

   static void processDependencies(set<string> &initial_files, bool toplevel=true);

 protected:
   
   virtual void error(char *err);

 private:
   
   static void loadAllInfoRecursive(const string &path);
   
   static bool findExternalRecursive(const string &basePath, const string &path, const string &type, string &fullname, bool fullPathOutput);

};




#endif
