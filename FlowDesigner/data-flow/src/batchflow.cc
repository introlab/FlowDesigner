#include "UIDocument.h"
#include <string>
#include "ParameterSet.h"
#include "ObjectRef.h"

int main(int argc, char **argv)
{
   try {
   UIDocument::scanDL();
   } catch (BaseException *e)
   {
      e->print();
      delete e;
      exit(1);
   }
   UIDocument::loadAllInfo();
   ParameterSet param;
   for (int arg = 2; arg<argc; arg++)
   {
      char arg_name[100];
      sprintf (arg_name, "ARG%d", arg-1);
      param.add(arg_name, ObjectRef (new String (argv[arg])));
   }
   UIDocument *doc = new UIDocument(argv[1]);
   doc->load();
   doc->run(param);
/*doc->export2net();
   string command = "xterm -e sh -c 'AudioNetwork " + doc->getPath() + doc->getName() + "et ; sleep 1000' &";
   cerr << command << endl;
   system (command.c_str());
   */
}
