// Copyright (C) 2001 Jean-Marc Valin & Dominic Letourneau

#include "UIDocument.h"
#include <string>
#include "ParameterSet.h"
#include "ObjectRef.h"
#include "path.h"
#include "Network.h"
#include "FlowException.h"
#include "iextensions.h"
#include "UINodeRepository.h"
#include "object_param.h"
#include <signal.h>
#include "UserException.h"

typedef Network *NetworkPtr;

class batchflowApp {

private:

  static batchflowApp* m_app;
  UIDocument *m_doc;
  volatile NetworkPtr m_net;
  string m_filename;

public:

  batchflowApp()
  : m_doc(NULL), m_net(NULL) {

    IExtensions::detect();
    scanDL();
    UINodeRepository::Scan();
  }

  ~batchflowApp() {
    if (m_doc) {
      delete m_doc;
    }
    if (m_net) {
       Network *net=m_net;
       m_net=NULL;
       net->cleanupNotify();
       delete net;
    }
  }

  void stop()
  {
     if (m_net)
        m_net->stop();
  }

  void initialize(int argc, char** argv) {
  
    ParameterSet param;
    for (int arg = 2; arg < argc; arg++) {

      char arg_name[100];
      sprintf (arg_name, "ARG%d", arg-1);
      param.add(arg_name, ObjectRef (new String (argv[arg])));
      sprintf (arg_name, "string:ARG%d", arg-1);
      param.add(arg_name, ObjectRef (new String (argv[arg])));
      sprintf (arg_name, "int:ARG%d", arg-1);
      param.add(arg_name, ObjectRef (Int::alloc (atoi(argv[arg]))));
      sprintf (arg_name, "float:ARG%d", arg-1);
      param.add(arg_name, ObjectRef (Float::alloc (atof(argv[arg]))));
      if (strlen(argv[arg]) > 2 && argv[arg][0]=='<' && argv[arg][strlen(argv[arg])-1]=='>') {
	sprintf (arg_name, "object:ARG%d", arg-1);
	try {
	  string val(argv[arg]);
	  ParameterSet p;
	  param.add(arg_name, ObjectParam::stringParam("object", val, p));
	} catch (...) {}
      }
    }

    m_filename = string(argv[1]);
    m_doc = new UIDocument(argv[1]);
    m_doc->load();
      
    m_net = m_doc->build("MAIN", param);
    if (m_net->getInputNode()) {
	 throw new GeneralException ("batchflowApp : main network has input node", __FILE__, __LINE__);
    }
    
    //cerr << "batchflowApp : initializing network "<<"("<<argv[1]<<")\n";
    for (int i = 0; ;i++) {
      if (!m_net->hasOutput(i)) {
	break;
      }
      ParameterSet req;
      m_net->request(i,req);
      }
    m_net->initialize();
    


  }//initialize
  
  void run() {

    //cerr << "batchflowApp : running "<<m_filename<<"\n";
    for (int i = 0; ;i++) {
      if (!m_net->hasOutput(i)) {
	break;
      }
      (m_net->getOutput(i,0))->printOn(cout); 
      cout<<endl;
    }
  }//run

  static batchflowApp* instance() {
    if (!m_app) {
      m_app = new batchflowApp;
    }
    return m_app;
  }
};

batchflowApp* batchflowApp::m_app = NULL;

static void sig_usr(int sig_no) {
 batchflowApp::instance()->stop();
}

int main(int argc, char **argv) {

  try {

    if (argc < 2) {
      cerr << "usage: batchflow <document> [arguments]" << endl;
      exit(1);
    }
    
    //signal function
    signal(SIGINT,sig_usr);
    batchflowApp::instance()->initialize(argc,argv);
    batchflowApp::instance()->run();
   }
   catch (BaseException *e) {
      e->print();
      cerr << endl;
      delete batchflowApp::instance();
      return 1;
   }  
   catch (RCPtr<FlowException> e) 
   {
      cerr << "Unhandled FlowException: " << endl;
      e->printOn(cerr);
      cerr << endl;
      delete batchflowApp::instance();
      return 1;
   }  
   catch (exception e) {
      cerr << e.what() << endl;
      delete batchflowApp::instance();
      return 2;
   }
   catch (UserException *e) {
      cerr << "User stop" << endl;
      delete batchflowApp::instance();
      return 0;
   }
   catch (...) {
      cerr<<"Unknown unhandled exception in "<<argv[1]<<endl;
      cerr<<"Exiting"<<endl;
      delete batchflowApp::instance();
      return 2;
   }

   delete batchflowApp::instance();
   return 0;
}
