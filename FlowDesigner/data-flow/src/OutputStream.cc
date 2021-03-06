// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "net_types.h"
#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include "Stream.h"

using namespace std;

namespace FD {

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
 * @parameter_type string
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
	 String strType = object_cast<String> (parameters.get("TYPE"));
	 if (strType == "stream")
	    type = cpp;
	 else if (strType == "FILE")
	    type = fptr;
#ifndef WIN32
	 else if (strType == "fd")
	    type = fd;
#endif
	 else 
	    throw new NodeException(NULL, "Bad stream type: " + strType, __FILE__, __LINE__);
      }
      else
	 type = cpp;
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
	 {
	    ofstream *file = new ofstream(fileName.c_str());
	    if (file->fail())
	    {
	       delete file;
	       throw new NodeException(this, "OutputStream: cannot open file: " + fileName, __FILE__, __LINE__);
	    }
	    openedFile = ObjectRef (new OStream(file));
	 }
	 break;
	 case fptr:
	 {
	    FILE *file = fopen (fileName.c_str(), "w");
	    if (!file)
	       throw new NodeException(this, "OutputStream: cannot open file: " + fileName, __FILE__, __LINE__);
	    openedFile = ObjectRef (new FILEPTR(file));
	 }
	 break;
#ifndef WIN32
	 case fd:
	 {
	    int file = open (fileName.c_str(), O_WRONLY);
	    if (file == -1)
	       throw new NodeException(this, "OutputStream: cannot open file: " + fileName, __FILE__, __LINE__);
	    openedFile = ObjectRef (new FILEDES(file));
	 }
	 break;
#endif
      }
      out[count] = openedFile;
   }

protected:
   /**Default constructor, should not be used*/
   OutputStream() {throw new GeneralException("OutputStream copy constructor should not be called",__FILE__,__LINE__);}

};

}//namespace FD

