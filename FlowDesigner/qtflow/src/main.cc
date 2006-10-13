//Copyright (C) 2006 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca) 

#include "QtFlowDesigner.h"
#include <QApplication>
#include "path.h"
#include "BaseException.h"

using namespace FD;

int main(int argc, char* argv[])
{

	try 
	{
      		scanDL();
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
