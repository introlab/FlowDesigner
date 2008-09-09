/***********************************************************************************
** Copyright (C) 2006-2008 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca). 
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
#include <streambuf>
#include <QApplication>
#include <QThread>
#include <QObject>
#include <QMessageBox>
#include "QtFlowDesigner.h"
#include "QtDLManager.h"
#include "QtFlowDesignerSplash.h"
#include "QtIORedirector.h"
#include "path.h"
#include "BaseException.h"
#include "UINodeRepository.h"
#include "QtProbeRegistry.h"
//#include "iextensions.h"

using namespace FD;
using namespace std;




void FDMsgHandler(QtMsgType type, const char *msg)
{
	switch (type) {
	case QtDebugMsg:
		QMessageBox::information(0, "Debug message", msg, QMessageBox::Ok);
		break;
	case QtWarningMsg:
		QMessageBox::warning(0, "Warning", msg, QMessageBox::Ok);
		break;
	case QtCriticalMsg:
		QMessageBox::critical(0, "Critical error", msg, QMessageBox::Ok);
		break;
	case QtFatalMsg:
		QMessageBox::critical(0, "Fatal error", msg, QMessageBox::Ok);
		abort();
	}

}


int main(int argc, char* argv[])
{
	// Init resource
    Q_INIT_RESOURCE(flowdesigner);
    
	//This will popup messages if required
	qInstallMsgHandler(FDMsgHandler);
	
	//Let's redirect stdout and stderr to a new stream
	//QtIORedirector redirector_cerr;
	//QtIORedirector redirector_cout;

	//redirect cout
	//std::cout.rdbuf(&redirector_cout);
	
	//redirect cerr
	//std::cerr.rdbuf(&redirector_cerr);
	
	try 
	{
		//Main application
		QApplication app(argc, argv);
            
		//Show splash screen
		QtFlowDesignerSplash splash;
		
		//Connect signals
		QObject::connect(QtDLManager::instance(),SIGNAL(newLoadedLibrary(QString)), &splash, SLOT(displayMessage(QString)));
		
		splash.show();
		splash.showMessage("Starting FlowDesigner...");
				
		
		//IExtensions::detect();
		
		//Load dynamic libraries
		QtDLManager::instance()->scanDL();
		
		//This must be called after we have loaded libraries
		UINodeRepository::Scan();
       
		
		//Look for probes
		QtProbeRegistry reg;
 
		//Main window
		QtFlowDesigner fd;
		
		//QObject::connect(&redirector_cerr,SIGNAL(newOutput(const char*, std::streamsize)),&fd,SLOT(newStderrOutput(const char *, std::streamsize)));
		
        
        for (int i = 1; i < argc; i++)
        {
            fd.loadDocument(argv[i]);
        }

        fd.show();
		splash.finish(&fd);
		
		// Enter the main loop of the QApplication
		int result = app.exec();
		
		// Destroy the singleton
		QtDLManager::destroy();
		
		return result;      
   	} 
	catch (BaseException *e)
   	{
      		e->print();
      		delete e;
      		
      		// Destroy the singleton
      		QtDLManager::destroy();
      		
      		exit(-1);
   	}
    catch (...)
    {
        std::cerr<<"Unknown exception caught"<<std::endl;
        
        // Destroy the singleton
        QtDLManager::destroy();
        
        exit(-1);                     
    }      



    return 0;
}

