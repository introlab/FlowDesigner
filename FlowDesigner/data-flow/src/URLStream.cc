
// Copyright (C) 2004 Dominic Letourneau

#include "URLHandler.h"
#include <string>
#include <map>
#include "Object.h"
#include "BufferedNode.h"
#include "net_types.h"
#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

using namespace std;
using namespace FD;

class URLStream;

DECLARE_NODE(URLStream)
/*Node
 *
 * @name URLStream
 * @category IO
 * @description Creates a stream from a URL (INPUT string)
 *
 * @input_name INPUT
 * @input_description The url string
 * @input_type string
 *
 * @output_name OUTPUT
 * @output_description The newly created Stream
 * @output_type Stream
 *
 * @parameter_name FLAGS
 * @parameter_type string
 * @parameter_description Flags for opening the stream (r, w, rw)
 *
 *
END*/

class URLStream : public BufferedNode {

protected:
   
  /**The ID of the 'output' output*/
  int m_outputID;
  
  /**The ID of the 'input' input*/
  int m_inputID;
  
  int m_flags;
  
 public:

  URLStream(string nodeName, ParameterSet params) 
    : BufferedNode(nodeName, params)
  {
    m_outputID = addOutput("OUTPUT");
    m_inputID = addInput("INPUT");
    
    if (parameters.exist("FLAGS"))
    {      
      RCPtr<String> flags = parameters.get("FLAGS");

      if (*flags == "r") {
	m_flags = URLHandler::URL_READ;
      } else if (*flags == "w") {
	m_flags = URLHandler::URL_WRITE;
      } else if (*flags == "rw") {
	m_flags = URLHandler::URL_READWRITE;
      } else {
	throw new GeneralException(string("Unknown flags : ") + *flags, __FILE__,__LINE__);
      }
    }      
  }
  
  void calculate(int output_id, int count, Buffer &out)
  {
    RCPtr<String> urlValue = getInput(m_inputID,count);     
    out[count] = URLHandler::openStream(*urlValue,m_flags);
  }

protected:

   /**Default constructor, should not be used*/
   URLStream() {throw new GeneralException("URLStream default constructor should not be called",__FILE__,__LINE__);}

  NO_ORDER_NODE_SPEEDUP(URLStream)
};

