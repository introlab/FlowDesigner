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
#ifndef _STRING_SPLIT_CC_
#define _STRING_SPLIT_CC_

#include "BufferedNode.h"
#include <QString>
#include <QStringList>
#include <Vector.h>



/*Node
 *
 * @name StringSplit
 * @category string
 * @description Split a string into a vector using a character.
 *
 * @input_name INPUT
 * @input_type string
 * @input_description String to split
 *
 * @input_name INPUT_SEP
 * @input_type string
 * @input_description Separator
 *
 * @output_name OUTPUT
 * @output_type vector
 * @output_description vector of strings
 *
 END*/
namespace FD {
	
	
	
	class StringSplit;
	DECLARE_NODE(StringSplit);
	
	class StringSplit : public BufferedNode {
		
		
		int inputID;
		int inputIDsep;
		int outputID;
		
	public:
		StringSplit(std::string nodeName, ParameterSet params)
		: BufferedNode(nodeName, params)
		{
			//inputs
			inputID = addInput("INPUT");
			inputIDsep = addInput("INPUT_SEP");
			
			
			//outputs
			outputID = addOutput("OUTPUT");
			
			//parameters
		}
		
		
		~StringSplit()
		{
		}
		
		
		void calculate(int output_id, int count, Buffer &out)
		{
			try
			{
				ObjectRef inputValue = getInput(inputID, count);
				ObjectRef inputValueSep = getInput(inputIDsep, count);
			
				//Converting input to string will throw an exception if not the right type
				String &words = object_cast<String > (inputValue);
				String &wordsSep = object_cast<String > (inputValueSep);
			
				//Making sure we get the full byte array by specifying the size
				QString str = QString(QByteArray(words.c_str(),words.size()));
				QString sep = QString(QByteArray(wordsSep.c_str(),wordsSep.size()));
				
				//Split the string
				QStringList listString = str.split( sep );
				
				
				// Create a vector to split the string
				Vector<ObjectRef> *vect = new Vector<ObjectRef>;
				
				//Iterating over the list
				for (int i = 0; i < listString.size(); ++i)
				{
					vect->push_back( ObjectRef( new String( listString.at(i).toLocal8Bit().constData() ) ) );
				}
				out[count] = ObjectRef(vect);
			}
			catch (BaseException *e)
			{
				//Let's add the exception to the exception stack
				throw e->add(new GeneralException("Unable to split the string",__FILE__,__LINE__));
				out[count] = nilObject;
			}
		}
		
	};
}//namespace FD
#endif //_STRING_SPLIT_CC_

