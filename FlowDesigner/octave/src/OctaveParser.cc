#ifndef _OCTAVEPARSER_CC_
#define _OCTAVEPARSER_CC_

#include "BufferedNode.h"
#include "octave/oct.h"
#include "octave/parse.h"
#include "octave/variables.h"
#include "octave/sysdep.h"

class OctaveParser;

DECLARE_NODE(OctaveParser)

/*Node
 * @name OctaveParser
 * @category Octave
 * @description Parse a .m or a .oct file and executes it.
 *
 * @input_name FILE_NAME
 * @input_type string
 * @input_description The file name of the script to execute.
 *
 * @output_name OUTPUT
 * @output_type Vector<ObjectRef>
 * @output_description Result of the octave code execution
 *
END*/

using namespace std;

class OctaveParser : public BufferedNode {
  
  //inputs
  int m_fileID;

  //outputs
  int m_outputID;

public:

   OctaveParser(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params) {

     //inputs
     m_fileID = addInput("FILE_NAME");

     //outputs
     m_outputID = addOutput("OUTPUT");

     //init octave
     sysdep_init();
     initialize_symbol_tables();
     
   }

   void calculate(int output_id, int count, Buffer &out) {

     RCPtr<String> file_name = getInput(m_fileID,count);

     //parse octave file, verbose = true
     parse_and_execute (*file_name, true);

     out[count] = nilObject;

   }//calculate
  
};



#endif
