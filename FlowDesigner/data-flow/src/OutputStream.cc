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

#include "BufferedNode.h"
#include "net_types.h"
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>

class OutputStream;

DECLARE_NODE(OutputStream)
/*Node
 *
 * @name OutputStream
 * @category IO
 * @description Creates a write-only stream from a filename
 *
 * @input_name INPUT
 * @input_description The file name
 * @input_type string
 *
 * @output_name OUTPUT
 * @output_description The Stream
 * @output_type Stream
 *
 * @parameter_name TYPE
 * @parameter_type String
 * @parameter_description Type of stream: stream, fd, or FILE (default stream)
 *
END*/

class OutputStream : public BufferedNode {

protected:
   
   /**The ID of the 'output' output*/
   int outputID;

   /**The ID of the 'input' input*/
   int inputID;

   typedef enum {fd, fptr, cpp} StreamType;

   StreamType type;

public:
   OutputStream(string nodeName, ParameterSet params) 
      : BufferedNode(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      inputID = addInput("INPUT");
      if (parameters.exist("TYPE"))
      {
	 const String &strType = object_cast<String> (parameters.get("TYPE"));
	 if (strType == "stream")
	    type = cpp;
	 else if (strType == "FILE")
	    type = fptr;
	 else if (strType == "fd")
	    type = fd;
	 else 
	    throw NodeException(NULL, "Bad stream type: " + strType, __FILE__, __LINE__);
      }
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      //cerr << "opening for count = " << count << endl;
      ObjectRef inputValue = getInput(inputID, count);
      const String &fileName = object_cast<String> (inputValue);

      ObjectRef openedFile;
      switch (type)
      {
	 case cpp:
	    openedFile = ObjectRef (new OFStream(fileName.c_str()));
	    break;
	 case fptr:
	    openedFile = ObjectRef (new FILEPTR(fopen (fileName.c_str(), "w")));
	    break;
	 case fd:
	    openedFile = ObjectRef (new FILEDES(open (fileName.c_str(), O_WRONLY)));
	    break;
      }
      out[count] = openedFile;
   }

protected:
   /**Default constructor, should not be used*/
   OutputStream() {throw new GeneralException("OutputStream copy constructor should not be called",__FILE__,__LINE__);}

};



