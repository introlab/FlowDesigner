/***********************************************************************************
** Copyright (C) 2006-2008 Laborius (http://www.gel.usherbrooke.ca/laborius/). 
** All rights reserved. 
**
** This program is free software; you can redistribute it and/or
** modify it under the terms of the GNU General Public License
** as published by the Free Software Foundation; either version 2
** of the License, or (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
***********************************************************************************/
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
#include "QtProbeManager.h"

using namespace FD;
using namespace std;


class QtFlowApp : public QApplication
{
	public:
	
	class QtFlowProcessingThread : public QThread
	{
		protected:
			QtFlowApp *m_app;
			QtRunContext *m_context;
			
		virtual void run() 
		{
				cerr<<"QtFlowProcessingThread::run()"<<endl;
				
				//Make sure the main event loop is launched 
				//before doing something. 
				// The problem is when a program ends before 
				// the main loop of Qt is running, a call to 
				// QApplication::exit() does nothing and the 
				// application never ends when it enters its 
				// event loop. (This thread is launched before 
				// app.exec() in the main method)
				msleep(100);
				
				if (m_context)
				{
					bool success = m_context->run();
					
					//TODO BETTER HANDLING OF THREADS & SERVERS
					if (success)
					{
						QApplication::exit(0);
					}
					else
					{	
						QApplication::exit(-1);
					}
				}
		}	
		
		public:
	
		QtFlowProcessingThread(QtFlowApp *app, UIDocument *doc, ParameterSet &params)
			: m_app(app)
		{
			if (doc)
			{	
				//Create run context
				//The context will "own" the document
				m_context = new QtRunContext(doc,params);
			}
		}
		
		~QtFlowProcessingThread()
		{
			//TODO STOP CONTEXT IF REQUIRED
			delete m_context;
		}
		
	};

	protected:

		QtFlowProcessingThread *m_thread;
		
		
	public:
		
	QtFlowApp(int argc, char* argv[])
		: QApplication(argc,argv, false)
	{	
		if (argc < 2) 
		{
			cout<<"Usage : "<<argv[0]<<" <document> [arguments] if you want to run a document"<<endl;
			cout<<"Will now run in server mode"<<endl;
			//terminate application
			//QApplication::exit(-1);
		}
		else
		{
			//Loading libraries
			try
			{
				//Scan for toolboxes (dll)
				QtDLManager::instance()->scanDL();
				//Scan for toolboxes (def)
				UINodeRepository::Scan();
				//Useful?
				IExtensions::detect();	
				
				//Loading document
				UIDocument *doc = NULL;
				
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
					
					//Load the network from RAM
					doc = new UIDocument("untitled");			
					doc->loadFromMemory(inputStream.str().c_str(),inputStream.str().size());			
				}
				else
				{	
					//Load the document from file
					doc = new UIDocument(argv[1]);
					doc->load();	
				}
				
				//Start the working thread
				ParameterSet params;
				m_thread = new  QtFlowProcessingThread(this,doc,params);
				m_thread->start();
		
			}
			catch (BaseException *e)
			{
				e->print(cerr);
				delete e;
				//terminate application
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
