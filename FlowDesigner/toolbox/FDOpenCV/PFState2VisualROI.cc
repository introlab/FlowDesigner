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

#ifndef _PFSTATE2VISUALROI_CC_
#define _PFSTATE2VISUALROI_CC_

#include "BufferedNode.h"
#include "VisualROI.h"
#include "PFGenericParticle.h"

using namespace std;
using namespace FD;

namespace RobotFlow {

class PFState2VisualROI;
DECLARE_NODE(PFState2VisualROI)

  /*Node
   *
   * @name PFState2VisualROI
   * @category RobotFlow:Vision:Tracking
   * @description Conversion from a particle filter sample (dynamic state) to a VisualROI object.
   *
   * @parameter_name USE_SCALE
   * @parameter_type bool
   * @parameter_value true
   * @parameter_description Flag indicating to use the ROI scale (width and height) in the particle's state.
   *
   * @parameter_name USE_ROTATION_ANGLE
   * @parameter_type bool
   * @parameter_value false
   * @parameter_description Flag indicating to use the ROI rotation angle in the particle's state.
   *
   * @parameter_name ROI_REGION_TYPE
   * @parameter_type int
   * @parameter_value 0
   * @parameter_description Geometric type for the ROI region (refer to enum e_VISUALROI_type in VisualROI.h).
   *
   * @input_name IN_PARTICLE
   * @input_type PFGenericParticle
   * @input_description Current ROI to convert.
   *
   * @output_name OUT_ROI
   * @output_type VisualROI
   * @output_description Resulting particle with appropriate state.
   *
   END*/

class PFState2VisualROI : public BufferedNode
{
public:
	PFState2VisualROI(string nodeName, ParameterSet params)
	: BufferedNode(nodeName, params)
	{
		m_particleInID = addInput("IN_PARTICLE");
		
		m_roiOutID = addOutput("OUT_ROI");
		
		m_useScale = dereference_cast<bool>(parameters.get("USE_SCALE"));
		m_useAngle = dereference_cast<bool>(parameters.get("USE_ROTATION_ANGLE"));
		m_roiType = dereference_cast<int>(parameters.get("ROI_REGION_TYPE"));
		
		// First 2 elements of state is center position x,y
		m_stateSize = 2;
		
		if (m_useScale) {
			// Add scale state which is described by the half-width and half-height of ROI
			m_stateSize += 2;
		}
		
		if (m_useAngle) {
			// Add rotation angle state
			m_stateSize += 1;
		}
		
		// Allocate ROI
		m_curROI = RCPtr<VisualROI>(new VisualROI());
		m_curROI->SetType(e_VISUALROI_type(m_roiType));
	}
	
	~PFState2VisualROI()
	{
		
	}
	
	void calculate(int output_id, int count, Buffer &out)
	{
		try {
			// Get current particle to convert
			ObjectRef particleObjRef = getInput(m_particleInID, count);
			
			if (!particleObjRef->isNil()) {
				RCPtr<PFGenericParticle> particleRef = RCPtr<PFGenericParticle>(particleObjRef);
				
				// Set ROI according to particle state
				const float *p_state = particleRef->GetCstState();
				// Set position
				m_curROI->SetXCen((int)(*p_state++));
				m_curROI->SetYCen((int)(*p_state++));
				
				if (m_useScale) {
					// Set scale
					m_curROI->SetHSX((int)(*p_state++));
					m_curROI->SetHSY((int)(*p_state++));
				}
				
				if (m_useAngle) {
					// Set rotation angle state
					m_curROI->SetAngle((int)(*p_state++));
				}
				
				// Output particle
				(*outputs[m_roiOutID].buffer)[count] = ObjectRef(m_curROI);
			}
			else {
				throw new GeneralException ("PFState2VisualROI::calculate : invalid (nilObject) IN_PARTICLE input.",__FILE__,__LINE__);
			}
		}
		catch (BaseException *e) {
			throw e->add(new GeneralException("Exception in PFState2VisualROI::calculate:",__FILE__,__LINE__));
		}
	}

private:
	// Input IDs
	int m_particleInID;
	
	// Output IDs
	int m_roiOutID;
	
	// Parameters
	bool m_useScale;
	bool m_useAngle;
	int m_roiType;
	
	unsigned int m_stateSize;
	
	RCPtr<VisualROI> m_curROI;
};

}

#endif
