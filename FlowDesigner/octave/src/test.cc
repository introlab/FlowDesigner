#include "octave/oct.h"
#include "octave/parse.h"
#include "octave/variables.h"
#include "octave/sysdep.h"
#include <iostream>
#include <string>

using namespace std;

int main(int argc, char* argv[]) {
  sysdep_init();
  initialize_symbol_tables();

  for (int i = 1; i <argc ; i++) {
    cerr<<"processing : "<<argv[i]<<endl;

    parse_and_execute (string(argv[i]), true);
  }

  return 0;
}
