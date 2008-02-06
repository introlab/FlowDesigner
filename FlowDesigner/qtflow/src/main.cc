//Copyright (C) 2006 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca) 

#include "QtFlowDesigner.h"
#include "QtDLManager.h"
#include <QApplication>
#include "path.h"
#include "BaseException.h"
#include "UINodeRepository.h"
#include "QtFlowDesignerSplash.h"
#include <QObject>
//#include "iextensions.h"

using namespace FD;
using namespace std;

int main(int argc, char* argv[])
{

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
