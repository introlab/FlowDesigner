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

using namespace std;
using namespace FD;

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

	FD::QtFlowApp app(argc,argv);

	app.exec();

	return 0;
}
