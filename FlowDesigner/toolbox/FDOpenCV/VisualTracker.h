/* Copyright (C) 2005 Pierre Moisan (Pierre.Moisan@USherbrooke.ca) 

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

#ifndef _VISUALTRACKER_H_
#define _VISUALTRACKER_H_

#include "BufferedNode.h"
#include <iostream>
#include "Image.h"
#include "cv.h"
#include "VisualFeatureDesc.h"
#include "VisualROI.h"
#include "VisualTarget.h"

namespace RobotFlow {

class VisualTracker : public FD::BufferedNode
{
public:
	VisualTracker()
	{
	
	}
	
	VisualTracker(std::string nodeName, FD::ParameterSet params)
	: BufferedNode(nodeName, params)
	{
	
	}

	virtual ~VisualTracker()
	{
	
	}

	// Default routine to print a VisualTracker object to an output stream
	virtual void printOn(std::ostream &out) const = 0;

	// Default routine to read a VisualTracker object from an input stream
	virtual void readFrom(std::istream &in) = 0;
	
	virtual void calculate(int output_id, int count, FD::Buffer &out) = 0;

	//virtual void TrackTarget() = 0;
};

}

#endif
