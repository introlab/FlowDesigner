// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau


#include "Node.h"
#include "Vector.h"
#include "ObjectParser.h"
#include <strstream>
#include <math.h>

class LPFilter;

DECLARE_NODE(LPFilter)
/*Node

 * @name LPFilter
 * @category Signal:DSP
 * @description No description available

 * @output_name OUTPUT
 * @output_description No description available

 * @parameter_name LENGTH
 * @parameter_description No description available

 * @parameter_name THETA
 * @parameter_description No description available

 * @parameter_name HP
 * @parameter_description No description available

END*/


/** A constant node contains a value that will never changes. */
class LPFilter : public Node
{

protected:

   /**The value of the constant*/
   ObjectRef value;

   /**The ID of the 'value' output*/
   int outputID;
public:

   /**Constructor, takes the name of the node and a set of parameters*/
   LPFilter(string nodeName, ParameterSet params)
      : Node(nodeName, params) 
      //, value (parameters.get("VALUE"))
   {
      int i;
      outputID = addOutput("OUTPUT");
      
      int length = dereference_cast <int> (parameters.get("LENGTH"));
      float theta = dereference_cast <float> (parameters.get("THETA"));
      value = ObjectRef(new Vector<float> (length));
      Vector<float> &val = object_cast<Vector<float> > (value);
      for (i=0;i<length;i++)
      {
         if (i-length/2 != 0)
            val[i]=sin(M_PI*(i-length/2)*theta)/(M_PI*(i-length/2)*theta);
         else val[i]=1.0;
         val[i]*=.54-.46*cos(2*M_PI*i/double(length-1));
      }
      float DC=0.0;
      for (i=0;i<length;i++)
         DC+=val[i];
      for (i=0;i<length;i++)
         val[i]/=DC;
      if (parameters.exist("NODC"))
      {
         for (i=0;i<length;i++)
            val[i] -= (.54-.46*cos(2*M_PI*i/double(length-1))) / (.54*(length-1));
      }
      
      if (parameters.exist("HP"))
      {
         for (i=0;i<length;i++)
            val[i]=-val[i];
         val[length/2]+=1;
      }
      /*for (i=0;i<length;i++)
         cout << val[i] << " ";
	 cout << endl;*/
     
   }

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count)
   {
      if (output_id==outputID) return value;
      else throw new NodeException (this, "LPFilter: Unknown output id", __FILE__, __LINE__);
   }

protected:
   /**Default constructor, should not be used*/
   LPFilter() {throw new GeneralException("LPFilter copy constructor should not be called",__FILE__,__LINE__);}

};
