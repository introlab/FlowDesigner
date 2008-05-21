#ifndef _OCTAVEPARSER_CC_
#define _OCTAVEPARSER_CC_

#include "BufferedNode.h"
#include "Matrix.h"
#include "CompositeType.h"
#include "octave/oct.h"
#include "octave/parse.h"
#include "octave/variables.h"
#include "octave/sysdep.h"
#include "octave/pathsearch.h"
#include "octave/defaults.h"
#include "octave/file-io.h"
#include "octave/ops.h"
#include "octave/error.h"
#include "octave/oct-env.h"

namespace FD {

class OctaveParser;

DECLARE_NODE(OctaveParser)

}

/*Node
 * @name OctaveParser
 * @category Octave
 * @description Parse a .m or a .oct file and executes it.
 *
 * @input_name FILE_NAME
 * @input_type string
 * @input_description The file name of the script to execute.
 *
 * @input_name VARIABLES_IN
 * @input_type Composite
 * @input_description Input variables inside a composite (Matrices)
 *
 * @output_name VARIABLES_OUT
 * @output_type Vector<ObjectRef>
 * @output_description Result of the octave code execution
 *
 * @parameter_name VERBOSE
 * @parameter_type bool
 * @parameter_value true
 * @parameter_description Put Octave in verbose mode
END*/

using namespace std;

extern void install_builtins (void);

class FD::OctaveParser : public FD::BufferedNode {
  
  //inputs
  int m_fileID;
  int m_variableInID;

  //outputs
  int m_variableOutID;

  //parameters
  bool m_verbose;

public:

  OctaveParser(string nodeName, FD::ParameterSet params)
    : FD::BufferedNode(nodeName, params) {

     //inputs
     m_fileID = addInput("FILE_NAME");
     m_variableInID = addInput("VARIABLES_IN");

     //outputs
     m_variableOutID = addOutput("VARIABLES_OUT");


     //parameters
     m_verbose = dereference_cast<bool>(parameters.get("VERBOSE"));

     //init octave
     // The order of these calls is important.  The call to   
     // install_defaults must come before install_builtins because
     // default variable values must be available for the variables to be
     // installed, and the call to install_builtins must come before the
     // options are processed because some command line options override
     // defaults by calling bind_builtin_variable.
         
     //add script path
     string OctPath = octave_env::getenv("OCTAVE_PATH");
     OctPath += string(":") + string(OCTAVE_SCRIPTS_PATH) + string("//");
     octave_env::putenv("OCTAVE_PATH",OctPath);
     
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

     octave_env::chdir(OCTAVE_SCRIPTS_PATH);


   }

  void calculate(int output_id, int count, FD::Buffer &out) {

    FD::RCPtr<FD::String> file_name = getInput(m_fileID,count);
    FD::RCPtr<FD::CompositeType> inputVariables = getInput(m_variableInID,count);


    //create output composite
    RCPtr<CompositeType> outComposite = RCPtr<CompositeType>(new CompositeType);
    
    //clear symbol table
    if (top_level_sym_tab) {
      top_level_sym_tab->clear();
    }
        
    FD::CompositeType::map_type cMap = inputVariables->getAllFields();

    for (FD::CompositeType::map_type::iterator iter = cMap.begin();
	 iter != cMap.end(); iter++) {
	
      std::string mName = iter->first;
      FD::RCPtr<FD::Matrix<complex<double> > > mPtr = iter->second;

      //Create octave matrix
      ComplexMatrix *octaveMatrix = new ComplexMatrix(mPtr->nrows(), mPtr->ncols());


      //copy elements
      for (int r = 0; r < mPtr->nrows(); r++) {
	for (int c= 0; c < mPtr->ncols(); c++) {
	  (*octaveMatrix)(r,c) = (*mPtr)(r,c);
	}
      }

      //put input variables in symbol table
      symbol_record *record = top_level_sym_tab->lookup(mName,true,true);

      if (record) {	
	record->define(*octaveMatrix,symbol_record::USER_VARIABLE); 
      }
    }
 
    //parse octave file, verbose = m_verbose
    parse_and_execute (*file_name, m_verbose);

    //get all variables in the symbol table
    if (top_level_sym_tab) {
      
      //top_level_sym_tab->print_info(cerr);
      string_vector variableNames = top_level_sym_tab->variable_name_list();
      //variableNames.list_in_columns(cerr);

      for (int i = 0; i <variableNames.length(); i++) {
       
	symbol_record* symbol = top_level_sym_tab->lookup(variableNames[i]);

	if (symbol) {
	  octave_value &value = symbol->variable_value();

	  //conversion to matrix
	  ComplexMatrix matrix_value = value.complex_matrix_value(true); 	

	  int rows = matrix_value.rows();
	  int cols = matrix_value.cols();

	  //create an equivalent FD object
	  FD::Matrix<complex<double> > *FDMatrix = new FD::Matrix<complex<double> >(rows,cols);

	  //copy content
	  for (int r = 0; r < rows; r++) {
	    for (int c = 0; c < cols; c++) {
	      (*FDMatrix)(r,c) = matrix_value(r,c);
	    }
	  }
  
	  //add in the Composite
	  outComposite->addField(variableNames[i],ObjectRef(FDMatrix));
	}
      }
    }          

    //output them
    out[count] = outComposite;

  }//calculate
  
};
#endif
