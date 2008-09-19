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

#ifndef _GETVISUALROIPARAM_CC_
#define _GETVISUALROIPARAM_CC_

#include "BufferedNode.h"
#include "VisualROI.h"
#include "Image.h"

using namespace std;
using namespace FD;

namespace RobotFlow {

class GetVisualROIParam;
DECLARE_NODE(GetVisualROIParam)

  /*Node
   *
   * @name GetVisualROIParam
   * @category RobotFlow:Vision:Tracking
   * @description Interface to get the member data of a VisualROI.
   *
   * @input_name IN_ROI
   * @input_type VisualROI
   * @input_description Current region of interest.
   *
   * @output_name CENTER_X
   * @output_type Int
   * @output_description Center position (X).
   *
   * @output_name CENTER_Y
   * @output_type Int
   * @output_description Center position (Y).
   *
   * @output_name HALF_WIDTH
   * @output_type Int
   * @output_description Region half width.
   *
   * @output_name HALF_HEIGHT
   * @output_type Int
   * @output_description Region half height.
   *
   * @output_name ANGLE
   * @output_type Int
   * @output_description Region angle (in degrees).
   *
   END*/

class GetVisualROIParam : public BufferedNode
{
public:
	GetVisualROIParam(string nodeName, ParameterSet params)
	: BufferedNode(nodeName, params)
	{
		m_roiInID = addInput("IN_ROI");
		
		m_xCenOutID = addOutput("CENTER_X");
		m_yCenOutID = addOutput("CENTER_Y");
		m_hsxOutID = addOutput("HALF_WIDTH");
		m_hsyOutID = addOutput("HALF_HEIGHT");
		m_angleOutID = addOutput("ANGLE");
	}
	
	virtual ~GetVisualROIParam()
	{
	}
	
	void calculate(int output_id, int count, Buffer &out)
	{
		try {
			// Get current region to convert
			ObjectRef roiObjRef = getInput(m_roiInID, count);
			
			if (!roiObjRef->isNil()) {
				RCPtr<VisualROI> roiRef = RCPtr<VisualROI>(roiObjRef);
				
				// Output region's data
				(*outputs[m_xCenOutID].buffer)[count] = ObjectRef(Int::alloc(roiRef->GetXCen()));
				(*outputs[m_yCenOutID].buffer)[count] = ObjectRef(Int::alloc(roiRef->GetYCen()));
				(*outputs[m_hsxOutID].buffer)[count] = ObjectRef(Int::alloc(roiRef->GetHSX()));
				(*outputs[m_hsyOutID].buffer)[count] = ObjectRef(Int::alloc(roiRef->GetHSY()));
				(*outputs[m_angleOutID].buffer)[count] = ObjectRef(Int::alloc(roiRef->GetAngle()));
			}
			else {
				// No region, output nilObjects
				(*outputs[m_xCenOutID].buffer)[count] = nilObject;
				(*outputs[m_yCenOutID].buffer)[count] = nilObject;
				(*outputs[m_hsxOutID].buffer)[count] = nilObject;
				(*outputs[m_hsyOutID].buffer)[count] = nilObject;
				(*outputs[m_angleOutID].buffer)[count] = nilObject;
			}
		}
		catch (BaseException *e) {
			throw e->add(new GeneralException("Exception in GetVisualROIParam::calculate:",__FILE__,__LINE__));
		}
	}

private:
	// Input IDs
	int m_imageInID;
	int m_roiInID;
	
	// Output IDs
	int m_xCenOutID;
	int m_yCenOutID;
	int m_hsxOutID;
	int m_hsyOutID;
	int m_angleOutID;
};

}

#endif
