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


#include "BufferedNode.h"
#include "Vector.h"
#include "ObjectParser.h"

#include <stdio.h>

class ExecStream;

DECLARE_NODE(ExecStream)
/*Node
 *
 * @name ExecStream
 * @category IO
 * @description A command to be executed (stdout is streamed)
 *
 * @input_name INPUT
 * @input_description The command arg
 * @input_type string
 *
 * @output_name OUTPUT
 * @output_description The stream
 * @output_type Stream
 *
 * @parameter_name COMMAND
 * @parameter_description The command
 * @parameter_type string
 *
END*/


class ExecStream : public BufferedNode {

protected:
   
   /**The ID of the 'output' output*/
   int outputID;

   /**The ID of the 'input' input*/
   int inputID;

   String command;

public:
   ExecStream(string nodeName, ParameterSet params) 
      : BufferedNode(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      inputID = addInput("INPUT");
      command = object_cast<String> (parameters.get("COMMAND"));
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      //cerr << "opening for count = " << count << endl;
      ObjectRef inputValue = getInput(inputID, count);
      const String &args = object_cast<String> (inputValue);

      string cmd = command + " " + args ;
      FILE *tmp = popen(cmd.c_str(), "r");
      if (!tmp)
	 NodeException (this, "ExecStream: popen call failed", __FILE__, __LINE__);
      out[count] = ObjectRef (new FILEPTR(tmp));
   }

protected:
   /**Default constructor, should not be used*/
   ExecStream() {throw new GeneralException("ExecStream copy constructor should not be called",__FILE__,__LINE__);}

};
