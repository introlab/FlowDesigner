// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau


#include "BufferedNode.h"
#include "ObjectParser.h"

#include <stdio.h>
#include "stream_wrap.h"

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
      out[count] = ObjectRef (new IOStream(new pipe_iostream(cmd, false)));
   }

protected:
   /**Default constructor, should not be used*/
   ExecStream() {throw new GeneralException("ExecStream copy constructor should not be called",__FILE__,__LINE__);}

};
