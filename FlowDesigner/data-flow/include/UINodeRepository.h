// Copyright (C) 2001 Jean-Marc Valin

#ifndef UINODE_REPOSITORY_H
#define UINODE_REPOSITORY_H

#include <vector>
#include <string>
#include <map>
#include <set>

class ItemInfo {
  public:
	string name;
	string type;
	string value;
	string description;
	
	ItemInfo() : type("any"), value(""), description("No description available") {}
};

class NodeInfo {
  public:
   vector<ItemInfo *> inputs;
   vector<ItemInfo *> outputs;
   vector<ItemInfo *> params;
   string category;
   string description;
   string sourceFile;
   string requireList;
  public:
   NodeInfo() : category("Unknown"), description("No description available") {}
};

class UINodeRepository {
  public:
   typedef map<string, NodeInfo *>::iterator iterator;

  private:
   map<string, NodeInfo *> info;

   static UINodeRepository &GlobalRepository();

   /**List of all files required for each module*/
   static map<string, set<string> > &ModuleDepend();
   
   /**List of all modules required for each file*/
   static map<string, set<string> > &FileDepend();
 
   /**List of all headers required for each file*/
   static map<string, set<string> > &HeaderDepend();

   static void loadAllInfoRecursive(const string &path);
   static void loadNodeDefInfo(const string &path, const string &name);
   static void loadExtDocInfo(const string &path, const string &name);
   //NodeInfo *&operator[] (const string &name) {return info[name];}
  public:
   
   iterator begin() {return info.begin();}
   iterator end() {return info.end();}

   NodeInfo *findNode(const string &name);

   static iterator Begin() {return GlobalRepository().info.begin();}
   static iterator End() {return GlobalRepository().info.end();}

   static void Scan();

   static NodeInfo *Find(const string &name);

   static set<string> &FindFileFromModule(const string &name);
   
   static set<string> &FindModuleFromFile(const string &name);
   
   static set<string> &FindHeaderFromFile(const string &name);
   
   static vector<string> Available();

   static void ProcessDependencies(set<string> &initial_files, bool toplevel=true);

};

#endif
