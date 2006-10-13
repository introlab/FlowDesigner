//Copyright (C) 2006 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca) 

#include "QtFlowDesigner.h"
#include <QApplication>
#include "path.h"
#include "BaseException.h"
#include "UINodeRepository.h"
#include "iextensions.h"

using namespace FD;

int main(int argc, char* argv[])
{

	try 
	{
		IExtensions::detect();
		scanDL();
		UINodeRepository::Scan();
   	} 
	catch (BaseException *e)
   	{
      		e->print();
      		delete e;
      		//exit(1);
   	}



	QApplication app(argc, argv);
	QtFlowDesigner fd;
	fd.show();
	return app.exec();
}
