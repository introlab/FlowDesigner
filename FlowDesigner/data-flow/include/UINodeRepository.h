// Copyright (C) 2001 Jean-Marc Valin

#ifndef UINODE_REPOSITORY_H
#define UINODE_REPOSITORY_H

#include <vector>
#include <string>
#include <map>
#include <set>
#include <libxml/tree.h>

namespace FD {

class UINetwork;

class ItemInfo {
  public:
        std::string name;
	std::string type;
	std::string value;
	std::string description;
	
	ItemInfo() : type("any"), value(""), description("No description available") {}
};

class NodeInfo {
  public:
   enum NodeKind {builtin, subnet, external};
   std::vector<ItemInfo *> inputs;
   std::vector<ItemInfo *> outputs;
   std::vector<ItemInfo *> params;
   std::string category;
   std::string description;
   std::string sourceFile;
   std::string requireList;
   NodeKind kind;
  public:
   NodeInfo() : category("Unknown"), description("No description available") {}
   ~NodeInfo() {for (unsigned int i=0;i<inputs.size();i++) delete inputs[i];
                  for (unsigned int i=0;i<outputs.size();i++) delete outputs[i];
                  for (unsigned int i=0;i<params.size();i++) delete params[i];}
  private:
   NodeInfo(const NodeInfo&) {}

};

class UINodeRepository {
  public:
   typedef std::map<std::string, NodeInfo *>::iterator iterator;

  private:
   std::map<std::string, NodeInfo *> info;

   static UINodeRepository &GlobalRepository();

   /**List of all files required for each module*/
   static std::map<std::string, std::set<std::string> > &ModuleDepend();
   
   /**List of all modules required for each file*/
   static std::map<std::string, std::set<std::string> > &FileDepend();
 
   /**List of all headers required for each file*/
   static std::map<std::string, std::set<std::string> > &HeaderDepend();

   static void LoadAllInfoRecursive(const std::string &path);
   static void LoadNodeDefInfo(const std::string &path, const std::string &name);
   static void LoadExtDocInfo(const std::string &path, const std::string &name);
   //NodeInfo *&operator[] (const std::string &name) {return info[name];}
  public:

   UINodeRepository() {}

   UINodeRepository(const UINodeRepository &);

   ~UINodeRepository();

   iterator begin() {return info.begin();}
   iterator end() {return info.end();}

   NodeInfo *findNode(const std::string &name);

   void loadDocInfo(xmlDocPtr doc, const std::string &basename);

   void loadAllSubnetInfo(xmlNodePtr net);
      
   void loadNetInfo(xmlNodePtr net);

   void updateNetInfo(UINetwork *net);

   void clean();

   static iterator Begin() {return GlobalRepository().info.begin();}

   static iterator End() {return GlobalRepository().info.end();}

   static void Scan();

   static NodeInfo *Find(const std::string &name);

   static std::set<std::string> &FindFileFromModule(const std::string &name);
   
   static std::set<std::string> &FindModuleFromFile(const std::string &name);
   
   static std::set<std::string> &FindHeaderFromFile(const std::string &name);
   
   static std::vector<std::string> Available();

   static void ProcessDependencies(std::set<std::string> &initial_files, bool toplevel=true);
};
}//namespace FD

#endif
