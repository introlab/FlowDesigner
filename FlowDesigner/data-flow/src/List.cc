// Copyright (C) 1999 Jean-Marc Valin
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

#include "List.h"
#include "net_types.h"
#include "Vector.h"
#include "multithread.h"

DECLARE_NODE(List)
/*Node

 * @name List
 * @category General
 * @description No description available

 * @input_name STREAM
 * @input_description No description available

 * @output_name OUTPUT
 * @output_description No description available

END*/


List::List(string nodeName, ParameterSet params) 
   : Node(nodeName, params)
{
   outputID = addOutput("OUTPUT");
   streamInputID = addInput("STREAM");
}

void List::specificInitialize()
{
   this->Node::specificInitialize();
}

void List::reset()
{
   this->Node::reset();
}

ObjectRef List::getOutput(int output_id, int count)
{
   //cerr << "Getting output in List\n";
   if (output_id==outputID)
   {
      lock();
      if (count != processCount)
      {
         processCount=count;
         Vector<ObjectRef> *strList = new Vector<ObjectRef>;
         
         NodeInput input = inputs[streamInputID];
         ObjectRef inputValue = input.node->getOutput(input.outputID,count);
         
         IStream &file = object_cast<IStream> (inputValue);

         char tmpLine[256];
         while (true)
         {
            file.getline(tmpLine, 255);
            if (file.fail()) break;
            strList->insert(strList->end(), ObjectRef (new String(tmpLine)));
         }
         currentList = ObjectRef(strList);
      }
      return unlock_and_return(currentList);
   }
   else 
      throw new NodeException (this, "List: Unknown output id", __FILE__, __LINE__);
}
