// Copyright (C) 1999 Jean-Marc Valin


#include "Node.h"
#include "ObjectRef.h"
#include "FFNet.h"
#include "Buffer.h"
#include <strstream>
#include "Vector.h"

using namespace std;

namespace FD {

class NNetNew;

DECLARE_NODE(NNetNew)
/*Node
 *
 * @name NNetNew
 * @category NNet
 * @description Returns a new (MLP) neural network
 *
 * @output_name OUTPUT
 * @output_type NNet
 * @output_description New (MLP) neural network
 *
 * @parameter_name TOPO
 * @parameter_type Vector<string>
 * @parameter_description No description available
 *
END*/


class NNetNew : public Node {

protected:
   
   /**The ID of the 'output' output*/
   int outputID;

   ObjectRef currentNet;

   Vector<int> topo;
  
   int processCount;

public:
   /**Constructor, takes the name of the node and a set of parameters*/
   NNetNew(string nodeName, ParameterSet params)
      : Node(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      
      istrstream topo_str(object_cast <String> (parameters.get("TOPO")).c_str());
      topo_str >> topo;

   }
      
   /**Class specific initialization routine.
      Each class will call its subclass initialize() method*/
   virtual void initialize()
   {
      processCount = -1;
      this->Node::initialize();
   }

   /**Class reset routine.
      Each class will call its superclass reset() method*/
   virtual void reset()
   {
      processCount = -1;
      this->Node::reset();
   }

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count)
      {
	 if (output_id==outputID)
	 {
	    if (count != processCount)
	    {

	       /*topo[0]=10;
	       topo[1]=11;
	       topo[2]=12;
	       topo[3]=13;*/
	       FFNet *net = new FFNet( topo ); 
	       
	       currentNet = ObjectRef(net);
	       //exit(1);
	    }
	    return currentNet;
	 }
	 else 
	    throw new NodeException (this, "NNetNew: Unknown output id", __FILE__, __LINE__);
      }

protected:
   /**Default constructor, should not be used*/
   NNetNew() {throw new GeneralException("NNetNew copy constructor should not be called",__FILE__,__LINE__);}

};
}//namespace FD
