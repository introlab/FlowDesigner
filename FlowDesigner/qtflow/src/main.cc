//Copyright (C) 2006 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca) 

#include "QtFlowDesigner.h"
#include <QApplication>
#include "path.h"
#include "BaseException.h"
#include "UINodeRepository.h"
//#include "iextensions.h"

using namespace FD;
using namespace std;

int main(int argc, char* argv[])
{

	try 
	{
		//IExtensions::detect();
		scanDL();
		
		cerr<<"SCAN"<<endl;
		UINodeRepository::Scan();
        
        QApplication app(argc, argv);
        QtFlowDesigner fd;

        for (int i = 1; i < argc; i++)
        {
            fd.loadDocument(argv[i]);
        }
       
        fd.show();
		cerr<<"App.exec()"<<endl;
        return app.exec();      
		cerr<<"App.exec() done"<<endl;
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
