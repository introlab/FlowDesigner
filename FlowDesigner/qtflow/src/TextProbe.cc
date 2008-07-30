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
#ifndef _TEXTPROBE_CC_
#define _TEXTPROBE_CC_

#include "net_types.h"
#include "Object.h"
#include <sstream>
#include <iostream>
#include "BufferedNode.h"
#include <QMainWindow>

using namespace std;

namespace FD {

class TextProbe;

DECLARE_NODE(TextProbe)
/*Node
 *
 * @name TextProbe
 * @category Probe
 * @description Prints the data as text
 *
 * @input_name INPUT
 * @input_type any
 * @input_description Any data
 *
 * @output_name OUTPUT
 * @output_type any
 * @output_description Pass through
 *
 * @parameter_name BREAK_AT
 * @parameter_type int
 * @parameter_description If set, the probe runs until (count = BREAK_AT)
 *
 * @parameter_name SHOW
 * @parameter_type bool
 * @parameter_value true
 * @parameter_description Whether or not to show the the data by default
 *
 * @parameter_name SKIP
 * @parameter_type int
 * @parameter_description Count increment for each "Next"
 *
 * @parameter_name PROBE_NAME
 * @parameter_type string
 * @parameter_description Name (title) of the probe
 *
END*/


class TextProbe : public BufferedNode {
	
  protected:

	  int m_inputID;
	  int m_outputID;
	  
	  int m_breakAt;
	  std::string m_probeName;
	  int m_skip;
	  bool m_show;
	  
	  //Qt Interface
	  QMainWindow *m_mainWindow;
	  
  public:

   TextProbe(std::string nodeName, ParameterSet params)
    : BufferedNode(nodeName,params), m_mainWindow(NULL)
   {
	   m_inputID = addInput("INPUT");
	   m_outputID = addOutput("OUTPUT");
	   
	   //parameters
	   m_show = dereference_cast<bool>(parameters.get("SHOW"));
	   m_breakAt = dereference_cast<int>(parameters.get("BREAK_AT"));
	   m_skip = dereference_cast<int>(parameters.get("SKIP"));
	   
	   m_probeName = object_cast<String>(parameters.get("PROBE_NAME"));
	   
   }

   virtual ~TextProbe()
   {
	   if (m_mainWindow)
		   delete m_mainWindow;
   }

   /**Class specific initialization routine.
      Each class will call its subclass initialize() method*/
   virtual void initialize()
   {
	   cout<<"virtual void TextProbe::virtual void initialize()"<<endl;
	   BufferedNode::initialize();
	   
	   m_mainWindow = new QMainWindow(NULL);
   }

   virtual void calculate(int output_id, int count, Buffer &out)
   {
	   cout<<"virtual void TextProbe::calculate(int output_id, int count, Buffer &out)"<<endl;
	   ObjectRef inputValue = getInput(m_inputID,count);
	   
	   stringstream outStream;
	   
	   outStream << *inputValue;
	   
	   cout << *inputValue;
	   
	   //Do something!
	
	   out[count] = inputValue;
   }
};



}//namespace FD

#endif
