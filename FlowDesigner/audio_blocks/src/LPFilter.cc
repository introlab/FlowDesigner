// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this file.  If not, write to the Free Software Foundation,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.


#include "Node.h"
#include "Vector.h"
#include "ObjectParser.h"
#include <stream.h>
#include <strstream.h>
#include <math.h>

class LPFilter;

//DECLARE_NODE(LPFilter)
NODE_INFO(LPFilter,"Signal", "", "OUTPUT", "LENGTH:THETA:HP")

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
      for (i=0;i<length;i++)
         cout << val[i] << " ";
      cout << endl;
     
   }

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count)
   {
      if (output_id==outputID) return value;
      else throw NodeException (this, "LPFilter: Unknown output id", __FILE__, __LINE__);
   }

protected:
   /**Default constructor, should not be used*/
   LPFilter() {throw GeneralException("LPFilter copy constructor should not be called",__FILE__,__LINE__);}

};
