#include "octave/oct.h"
#include "octave/parse.h"
#include "octave/variables.h"
#include "octave/sysdep.h"
#include "octave/pathsearch.h"
#include "octave/defaults.h"
#include "octave/file-io.h"
#include "octave/ops.h"
#include "octave/error.h"

#include <iostream>
#include <string>

using namespace std;
//using namespace FD;

extern void install_builtins (void);

static void initialize_error_handlers () {
  set_liboctave_error_handler (error);
  set_liboctave_warning_handler (warning);
}

int main(int argc, char* argv[]) {

//  octave_env::set_program_name (argv[0]);
//  dir_path::set_program_name (argv[0]);


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
  


  for (int i = 1; i <argc ; i++) {
    cerr<<"processing : "<<argv[i]<<endl;

    parse_and_execute (string(argv[i]), true);
  }

  return 0;
}
