#ifndef _OCTAVEPARSER_CC_
#define _OCTAVEPARSER_CC_

#include "BufferedNode.h"
#include "octave/oct.h"
#include "octave/parse.h"
#include "octave/variables.h"
#include "octave/sysdep.h"
#include "octave/pathsearch.h"
#include "octave/defaults.h"
#include "octave/file-io.h"
#include "octave/ops.h"
#include "octave/error.h"

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

extern void install_builtins (void);

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
     // The order of these calls is important.  The call to   
     // install_defaults must come before install_builtins because
     // default variable values must be available for the variables to be
     // installed, and the call to install_builtins must come before the
     // options are processed because some command line options override
     // defaults by calling bind_builtin_variable.
     
     sysdep_init ();
     
     //initialize_error_handlers ();
     
     install_defaults ();
     
     //initialize_pathsearch ();
     
     //install_signal_handlers ();
     
     initialize_file_io ();
     
     initialize_symbol_tables ();
     
     install_types ();
     
     install_ops ();
     
     install_builtins ();
     
   }

   void calculate(int output_id, int count, Buffer &out) {

     RCPtr<String> file_name = getInput(m_fileID,count);

     //parse octave file, verbose = true
     parse_and_execute (*file_name, true);

     out[count] = nilObject;

   }//calculate
  
};



#endif
