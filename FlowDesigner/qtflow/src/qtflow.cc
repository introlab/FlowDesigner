#include <iostream>
#include "QtRunContext.h"
#include "QtDLManager.h"
#include "UIDocument.h"
#include <string>
#include "ParameterSet.h"
#include "ObjectRef.h"
#include "Network.h"
#include "Exception.h"
#include <sstream>
#include "iextensions.h"
#include <QApplication>
#include <QThread>
#include <QTimer>

using namespace FD;
using namespace std;


class QtFlowApp : public QApplication
{
	public:
	
	class QtFlowProcessingThread : public QThread
	{
		protected:
			QtFlowApp &m_app;
			
		virtual void run() 
		{
				cerr<<"QtFlowProcessingThread::run()"<<endl;
				m_app.run();
				exec();
		}	
		
		public:
	
		QtFlowProcessingThread(QtFlowApp &app)
			: m_app(app)
		{
		
		
		}
	};


	protected:
	
		UIDocument *doc;
		QTimer *m_timer;
		ParameterSet params;
		int m_timerId;
		QtFlowProcessingThread *m_thread;
		
	public:
		
	QtFlowApp(int argc, char* argv[])
		: QApplication(argc,argv)
	{
		if (argc < 2) 
		{
			cout<<"Usage : "<<argv[0]<<" <document> [arguments]"<<endl;
			//terminate application
			QApplication::exit(-1);
		}
		
		
		//Loading libraries
		try
		{

			//Scan for toolboxes (dll)
			QtDLManager::scanDL();
			//Scan for toolboxes (def)
			UINodeRepository::Scan();
			//Useful?
			IExtensions::detect();	
			
			//Loading document
			doc = NULL;
			
			//ARE WE RECEIVING RAW DATA FROM FLOWDESIGNER
			//IN XML FORMAT.
			if (string(argv[1]) == "/dev/stdin")
			{	
				
				stringstream inputStream;
				
				//we will read from stdin
				while(!cin.fail())
				{
					char data;
					cin.read(&data,1);
					inputStream.write(&data,1);
				}
				
				//Run the network
				doc = new UIDocument("untitled");
				
				doc->loadFromMemory(inputStream.str().c_str(),inputStream.str().size());
				
				
			}
			else
			{
				
				
				doc = new UIDocument(argv[1]);
				doc->load();
				
			}
			
			//Start Thread timer
			m_timerId = startTimer(1);
	
		}
		catch (BaseException *e)
		{
			e->print(cerr);
			delete e;
			//terminate application
			QApplication::exit(-1);
		}
		
	}
	void timerEvent(QTimerEvent *event)
	{
		killTimer(m_timerId);
		run();
	
	}
	
	void run()
	{
		if (doc)
		{
			try {
				//Running document
				QtRunContext *ctx = new QtRunContext(doc, params);		
				bool success = ctx->run();
				delete ctx;
				
				if (success)
					QApplication::exit(0);
				else
					QApplication::exit(-1);
			}	
			catch (BaseException *e)
			{
				e->print(cerr);
				delete e;
				QApplication::exit(-1);
			}
			catch (...)
			{
				QApplication::exit(-1);
			}
		}
	}

};


int main (int argc, char* argv[])
{

		
		//Parse command line parameters
		cerr<<"QtFlow starting..."<<endl;
		

		
		//TODO
		//PARSING ADDITIONAL ARGUMENTS
		
		/*
		for (int arg = 2; arg<argc; arg++)
		{
			char arg_name[100];
			sprintf (arg_name, "ARG%d", arg-1);
      
			params.add(arg_name, ObjectRef (new String (argv[arg])));
			sprintf (arg_name, "string:ARG%d", arg-1);
			params.add(arg_name, ObjectRef (new String (argv[arg])));
			
			sprintf (arg_name, "int:ARG%d", arg-1);			
			params.add(arg_name, ObjectRef (Int::alloc (atoi(argv[arg]))));
			sprintf (arg_name, "float:ARG%d", arg-1);			
			params.add(arg_name, ObjectRef (Float::alloc (atof(argv[arg]))));
			
			if (strlen(argv[arg]) > 2 && argv[arg][0]=='<' && argv[arg][strlen(argv[arg])-1]=='>') 
			{
				sprintf (arg_name, "object:ARG%d", arg-1);
				try 
				{
					string val(argv[arg]);
					ParameterSet p;
					ObjectRef obj = ObjectParam::stringParam("object", val, p);
					if (!obj.isNil())
					{
						params.add(arg_name, obj);
					}
				} catch (...) 
				{
					//WHAT DO WE DO ?
				}
			}
		}
		*/
		
	QtFlowApp app(argc,argv);

	app.exec();
	
	return 0;
}
