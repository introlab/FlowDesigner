// Copyright (C) 2006 Dominic Letourneau

#include "BufferedNode.h"
#include <string>

using namespace std;

namespace FD {

class OctaveFullScriptPath;

DECLARE_NODE(OctaveFullScriptPath)
/*Node
 *
 * @name OctaveFullScriptPath
 * @category Octave
 * @description Get the Full Path of a script (to be executed)
 *
 * @parameter_name SCRIPT_NAME
 * @parameter_type string
 * @parameter_value script.oct
 * @parameter_description The name of the script (installed) to be converted with its full path. 
 *
 * @output_name SCRIPT_PATH
 * @output_type string
 * @output_description The script's full path
 *
END*/


  class OctaveFullScriptPath : public BufferedNode {
  
    int m_outputID;
    string m_fullPath;
    
  public:
    OctaveFullScriptPath(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
    {
      //output(s)
      m_outputID = addOutput("SCRIPT_PATH");
      
      //parameter(s)
      //OCTAVE_SCRIPTS_PATH is defined as a preprocessor macro (configure.in)
      m_fullPath = string(OCTAVE_SCRIPTS_PATH) + string("/") +  object_cast<String>(parameters.get("SCRIPT_NAME"));
		
    }
        
    void calculate(int output_id, int count, Buffer &out)
    {                        
      out[count] = ObjectRef(new String(m_fullPath));
    }
    
    NO_ORDER_NODE_SPEEDUP(OctaveFullScriptPath)
  };
}//namespace FD
