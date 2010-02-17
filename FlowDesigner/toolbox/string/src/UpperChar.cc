/* Copyright (C) 2010 Luc LEGER
 
 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.
 
 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.
 
 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
#ifndef _UPPER_CHAR_CC_
#define _UPPER_CHAR_CC_

#include "BufferedNode.h"
#include <QString>



/*Node
 *
 * @name UpperChar
 * @category string
 * @description none
 *
 * @input_name INPUT
 * @input_type string
 * @input_description String to upper case
 *
 * @output_name OUTPUT
 * @output_type string
 * @output_description upper case
 *
 END*/
namespace FD {
	
	
	
	class UpperChar;
	DECLARE_NODE(UpperChar);
	
	class UpperChar : public BufferedNode {
		
		
		int inputID;
		int outputID;
		
	public:
		UpperChar(std::string nodeName, ParameterSet params)
		: BufferedNode(nodeName, params)
		{
			//inputs
			inputID = addInput("INPUT");
			
			
			//outputs
			outputID = addOutput("OUTPUT");
			
			//parameters
		}
		
		
		~UpperChar()
		{
		}
		
		
		void calculate(int output_id, int count, Buffer &out)
		{
			try
			{
				ObjectRef inputValue = getInput(inputID, count);
			
				//Converting input to string will throw an exception if not the right type
				String &words = object_cast<String > (inputValue);
			
				//Making sure we get the full byte array by specifying the size
				QString str = QString(QByteArray(words.c_str(),words.size())).toUpper();

			
				//Construct FD::String from QString needs explicit toStdString() function
				out[count] = ObjectRef(new String(str.toStdString()));
			}
			catch (BaseException *e)
			{
				//Let's add the exception to the exception stack
				throw e->add(new GeneralException("Unable to convert to string",__FILE__,__LINE__));
				out[count] = nilObject;
			}
		}
		
	};
}//namespace FD
#endif //_UPPER_CHAR_CC_

