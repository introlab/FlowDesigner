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

#include <stdio.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/soundcard.h>
#include <unistd.h>

class ExecStream;

//DECLARE_NODE(ExecStream)
NODE_INFO(ExecStream,"IO", "INPUT", "OUTPUT", "COMMAND")

/** A constant node contains a value that will never changes. */
class ExecStream : public Node
{

protected:

   /**The value of the constant*/
   ObjectRef value;

   /*the file descriptor*/
   int audio_fd;
   /**The ID of the 'value' output*/
   int outputID;

   int inputID;

   ObjectRef current;

   bool opened;
      
   String command;
public:

   /**Constructor, takes the name of the node and a set of parameters*/
   ExecStream(string nodeName, ParameterSet params)
      : Node(nodeName, params)
      , opened(false)
      //, value (parameters.get("VALUE"))
   {
      outputID = addOutput("OUTPUT");
      inputID = addInput("INPUT");
      command = object_cast<String> (parameters.get("COMMAND"));
   }

   void specificInitialize()
   {
      Node::specificInitialize();

      //String command = object_cast <String> (parameters.get("COMMAND"));

      
   }
      
   virtual ~ExecStream()
   {
      cerr << "ExecStream destructor\n";
      if (opened)
	 pclose (dereference_cast<FILE *> (current));
   }

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count)
   {
      if (output_id==outputID) 
      {
	 if (count == processCount)
	    return current;
	 //cerr << "opening pipe\n";
	 processCount=count;
	 NodeInput input = inputs[inputID];
	 ObjectRef inputValue = input.node->getOutput(input.outputID,count);
	 String name = object_cast<String> (inputValue);

	 if (opened)
	    pclose (dereference_cast<FILE *> (current));
	 //string cmd = "mpg123 --stdout " + name ;
	 string cmd = command + " " + name ;
	 FILE *tmp = popen(cmd.c_str(), "r");
	 if (!tmp)
	    NodeException (this, "ExecStream: popen call failed", __FILE__, __LINE__);
	 opened = true;
	 current = ObjectRef (new FILEPTR (tmp));
	 return current;
	 /*openedFile = ObjectRef (new IFStream());
	 IFStream &tmp = object_cast<IFStream> (openedFile);
	 tmp.open(fileName.c_str());*/
      }
      else throw NodeException (this, "ExecStream: Unknown output id", __FILE__, __LINE__);
      
      
   }

protected:
   /**Default constructor, should not be used*/
   ExecStream() {throw GeneralException("ExecStream copy constructor should not be called",__FILE__,__LINE__);}

};
