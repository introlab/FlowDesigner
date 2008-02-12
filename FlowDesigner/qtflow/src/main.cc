//Copyright (C) 2006 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca) 

#include "QtFlowDesigner.h"
#include "QtDLManager.h"
#include <QApplication>
#include "path.h"
#include "BaseException.h"
#include "UINodeRepository.h"
#include "QtFlowDesignerSplash.h"
#include <QObject>
#include <QMessageBox>
#include "QtIORedirector.h"
#include <streambuf>
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
        
		QtFlowDesigner fd;
		
		//QObject::connect(&redirector_cerr,SIGNAL(newOutput(const char*, std::streamsize)),&fd,SLOT(newStderrOutput(const char *, std::streamsize)));
		
        
        for (int i = 1; i < argc; i++)
        {
            fd.loadDocument(argv[i]);
        }
       
        fd.show();
		
        return app.exec();      
		
   	} 
	catch (BaseException *e)
   	{
      		e->print();
      		delete e;
      		exit(-1);
   	}
    catch (...)
    {
        std::cerr<<"Unknown exception caught"<<std::endl;
        exit(-1);                     
    }      



    return 0;
}
