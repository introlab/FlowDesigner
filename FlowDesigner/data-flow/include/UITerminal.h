#ifndef UITERMINAL_H
#define UITERMINAL_H

using namespace std;

//#include <gnome.h>
#include <vector>
#include <string>
#include <fstream>
#include <pthread.h>

class UINode;
class UILink;
class UINetTerminal;
class ItemInfo;

class UITerminal {
protected:

   /**The terminal name*/
   string name;
   
   /**The terminal type*/
   string type;
  
   /**The description of the terminal */
   string description;

   /**The node that owns this terminal*/
   UINode *node;

   /**position relative to node group*/
   double x,y;

   /**Whether it's an input of the node (otherwise, output)*/
   bool isInput;

   /**All connected links (only one allowed for inputs)*/
   vector <UILink *> connections;

   /**The net input/output connected to the terminal (NULL if none)*/
   UINetTerminal *netTerminal;

public:
   UITerminal (ItemInfo *terminalInfo, UINode *_node, bool _isInput, 
			   double _x, double _y);

   virtual ~UITerminal();


   UINode *getNode() {return node;}


   /**returns the position in world coord*/
   virtual void getPos(double &wx, double &wy)
   {
      wx=x;
      wy=y;
      //gnome_canvas_item_i2w(item->parent, &wx, &wy);
   }

   /**returns the position in item coord*/
   void getItemPos(double &wx, double &wy)
   {
      wx=x;
      wy=y;
   }

   /**connect to a link*/
   void connect(UILink *link) {connections.insert(connections.end(), link);}
   
   /**disconnect from a link*/
   void disconnect(UILink *link) 
   {
      //Now, this should comply to ANSI C++
      vector<UILink *>::iterator i=connections.begin();
      while (i != connections.end())
      {
	 if (*i == link)
	 {
	    connections.erase(i);
	    break;
	 }
	 ++i;
      }
      /*for (int i=0;i<connections.size();i++)
         if (connections[i]==link)
            connections.erase(&connections[i]);
      */
   }

   /**connect to a network terminal*/
   void connectNetTerminal(UINetTerminal *term);

   /**connect to a network terminal*/
   void disconnectNetTerminal();

   const string &getName() {return name;}
   const string &getType() {return type;}
   const string &getDescription() {return description;}
   bool isInputTerminal() {return isInput;}
   
   bool isConnected() {return connections.size()!=0 || netTerminal;}
   
   void export2net (ostream &out);
   
   vector<UILink *> getConnections() {return connections;}

};

#endif
