// Copyright (C) 2001 Jean-Marc Valin

#ifndef UITERMINAL_H
#define UITERMINAL_H

#include <vector>
#include <string>
#include <fstream>
#include <list>

namespace FD {

class UINode;
class UILink;
class UINetTerminal;
class ItemInfo;

class UITerminal {
protected:

   /**The terminal name*/
   std::string name;

   /**The terminal type*/
   std::string type;

   /**The description of the terminal */
   std::string description;

   /**The node that owns this terminal*/
   UINode *node;

   /**position relative to node group*/
   double x,y;

   /**Whether it's an input of the node (otherwise, output)*/
   bool isInput;

   /**All connected links (only one allowed for inputs)*/
   std::vector <UILink *> connections;

   /**The net input/output connected to the terminal (NULL if none)*/
   UINetTerminal *netTerminal;

public:

   UITerminal (ItemInfo *terminalInfo, UINode *_node, bool _isInput, double _x, double _y);

   virtual ~UITerminal();


   UINode *getNode() const {return node;}


   /**returns the position in world coord*/
   virtual void getPos(double &wx, double &wy) const
   {
      wx=x;
      wy=y;
   }

   /**connect to a link*/
   void connect(UILink *link) {connections.insert(connections.end(), link);}

   /**
    *  Verify if we have a specified link
    *  \param link The link to verify
    *  \return true if link exists, false otherwise
    */
   bool haveLink(const UILink *link);

   /**disconnect from a link*/
   void disconnect(UILink *link);

   /**connect to a network terminal*/
   void connectNetTerminal(UINetTerminal *term);

   /**connect to a network terminal*/
   void disconnectNetTerminal();

   std::string getName() const {return name;}
   std::string getType() const {return type;}
   std::string getDescription() const {return description;}
   bool isInputTerminal() const {return isInput;}

   bool isConnected() const {return connections.size()!=0 || netTerminal;}

   std::vector<UILink *> getConnections() const {return connections;}

   UINetTerminal* getNetTerminal() {return netTerminal;}
   void removeNetTerminal();

};

}//namespace FD

#endif
