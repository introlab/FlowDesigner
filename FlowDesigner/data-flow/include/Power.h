/* Copyright (C) 2003 Victor Bao Long Tran(v.tran@usherbrooke.ca)

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
#ifndef _POWER_H_
#define _POWER_H_

#include "BufferedNode.h"

namespace FD {

class Power : public BufferedNode
{
protected:
   //inputs
   int m_baseInID;
   int m_expID;

   //outputs
   int m_outputID;

   //parameters

public:

   Power(std::string nodeName, ParameterSet params);

   virtual ~Power();

   void calculate(int output_id, int count, Buffer &out);

};

}//namespace FD

#endif //_POWER_H_
