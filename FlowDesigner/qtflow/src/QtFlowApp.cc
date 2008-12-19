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
 **************************************************************************************/
#include "qtflow.h"

#include "iextensions.h"
#include <QtDebug>


using namespace std;

namespace FD
{

	void QtFlowProcessingThread::run()
	{
		std::cerr<<"QtFlowProcessingThread::run()"<<std::endl;

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
			if(m_context->run() == true)
			{
				//Thread exit with return code 0 = OK
				exit(0);
			}
		}

		//RETURN ERROR CODE
		exit(-1);
	}



	QtFlowProcessingThread::QtFlowProcessingThread(QtFlowApp *app, UIDocument *doc, ParameterSet &params)
	: QThread(app), m_app(app)
	{
		if (doc)
		{
			//Create run context
			//The context will "own" the document
			m_context = new QtRunContext(doc,params);
		}
	}

	QtFlowProcessingThread::~QtFlowProcessingThread()
	{
		//TODO STOP CONTEXT IF REQUIRED
		delete m_context;
	}

	QtFlowApp::QtFlowApp(int argc, char* argv[])
	: QApplication(argc,argv, false)
	{
		if (argc < 2)
		{
			cout<<"Usage : "<<argv[0]<<" <document> [arguments] if you want to run a document"<<endl;
			//cout<<"Will now run in server mode"<<endl;
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

				for (int index = 1; index < argc; index++)
				{

					//ARE WE RECEIVING RAW DATA FROM FLOWDESIGNER
					//IN XML FORMAT FROM STDIN.
					if (string(argv[index]) == "/dev/stdin")
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
						doc = new UIDocument(argv[index]);
						doc->load();
					}

					//Start the working thread
					ParameterSet params;

					QtFlowProcessingThread *newThread = new QtFlowProcessingThread(this,doc,params);
					ThreadSignalHandler *handler = new ThreadSignalHandler(this,newThread);
					m_threadHandlers.push_back(handler);


				}

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

	QtFlowApp::~QtFlowApp()
	{


	}

	bool QtFlowApp::removeThreadHandler(ThreadSignalHandler *handler)
	{
		if (m_threadHandlers.contains(handler))
		{
			m_threadHandlers.removeAll(handler);
			delete handler;

			if (m_threadHandlers.empty())
			{
				//Application ending...
				QApplication::exit(0);
			}

			return true;
		}

		return false;
	}

	/*******************/

	ThreadSignalHandler::ThreadSignalHandler(QtFlowApp *app, QtFlowProcessingThread *thread)
		: QObject(app), m_app(app), m_thread(thread)
	{
		if (m_thread)
		{
			//Connect signals
			connect(thread,SIGNAL(finished()),this,SLOT(threadFinished()));
			connect(thread,SIGNAL(terminated()), this, SLOT(threadTerminated()));
			m_thread->start();
		}
	}

	ThreadSignalHandler::~ThreadSignalHandler()
	{
		//Make sure the handler is removed
		m_app->removeThreadHandler(this);

		if (m_thread)
		{
			if (m_thread->isRunning())
			{
				qDebug("ThreadSignalHandler::~ThreadSignalHandler() - killing thread (locked?)");
				m_thread->terminate();
			}
			delete m_thread;
		}
	}

	void ThreadSignalHandler::threadFinished()
	{
		qDebug("ThreadSignalHandler::threadFinished()");
		m_thread->wait();
		m_app->removeThreadHandler(this);
	}

	void ThreadSignalHandler::threadTerminated()
	{
		qDebug("ThreadSignalHandler::threadTerminated()");
		m_app->removeThreadHandler(this);
	}

	QtFlowProcessingThread* ThreadSignalHandler::getThread()
	{
		return m_thread;
	}


} //namespace FD

