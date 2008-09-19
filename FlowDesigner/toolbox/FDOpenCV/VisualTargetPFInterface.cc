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

#ifndef _VISUALROI2PFSTATE_CC_
#define _VISUALROI2PFSTATE_CC_

#include "BufferedNode.h"
#include "VisualTarget.h"
#include "PFGenericParticle.h"

using namespace std;
using namespace FD;

namespace RobotFlow {

class VisualTargetPFInterface;
DECLARE_NODE(VisualTargetPFInterface)

  /*Node
   *
   * @name VisualTargetPFInterface
   * @category RobotFlow:Vision:Tracking
   * @description Interface for tracking a VisualTarget using a particle filter framework.
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
   * @parameter_name LIKELIHOOD_SIGMA
   * @parameter_type float
   * @parameter_value 200.0
   * @parameter_description Exponential sigma value to discriminate likelihood.
   *
   * @input_name IN_TARGET
   * @input_type VisualTarget<double>
   * @input_description Current VisualTarget to convert.
   *
   * @input_name IN_FEATURES_VECTOR
   * @input_type Vector<VisualFeatureDesc<double> *>
   * @input_description Features descriptors vector (to compute a particle likelihood).
   *
   * @input_name IN_MEAN_STATE
   * @input_type PFGenericParticle
   * @input_description Mean state particle resulting from partcile filter tracking.
   *
   * @input_name PREPROCESS_COMPLETED
   * @input_type Vector<int>
   * @input_description Flags indicating the completion of the features extraction preprocessing stage.
   *
   * @input_name FILTERING_FINISHED
   * @input_type bool
   * @input_description Flags indicating the completion of the particle filtering stage.
   *
   * @output_name OUT_PARTICLE
   * @output_type PFGenericParticle
   * @output_description Resulting particle with appropriate state.
   *
   * @output_name OUT_LIKELIHOOD
   * @output_type float
   * @output_description Current particle likelihood.
   *
   * @output_name OUT_TARGET
   * @output_type VisualTarget<double>
   * @output_description Resulting tracked target.
   *
   * @output_name TRACKING_FINISHED
   * @output_type bool
   * @output_description Flag indicating that the tracking has completed.
   *
   END*/

class VisualTargetPFInterface : public BufferedNode
{
public:
	VisualTargetPFInterface(string nodeName, ParameterSet params)
	: BufferedNode(nodeName, params),
	m_needNewTarget(true)
	{
		m_targetInID = addInput("IN_TARGET");
		m_featuresInID = addInput("IN_FEATURES_VECTOR");
		m_meanParticleInID = addInput("IN_MEAN_STATE");
		m_ppCompletedInID = addInput("PREPROCESS_COMPLETED");
		m_filtFinishedInID = addInput("FILTERING_FINISHED");
		
		m_particleOutID = addOutput("OUT_PARTICLE");
		m_likelihoodOutID = addOutput("OUT_LIKELIHOOD");
		m_targetOutID = addOutput("OUT_TARGET");
		m_finishedOutID = addOutput("TRACKING_FINISHED");
		
		m_useScale = dereference_cast<bool>(parameters.get("USE_SCALE"));
		m_useAngle = dereference_cast<bool>(parameters.get("USE_ROTATION_ANGLE"));
		m_lSigma = dereference_cast<float>(parameters.get("LIKELIHOOD_SIGMA"));
		
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
		
		// Allocate particle
		m_curSample = RCPtr<PFGenericParticle>(new PFGenericParticle(m_stateSize));
	}
	
	~VisualTargetPFInterface()
	{
	
	}
	
	// Modified BufferedNode request method to support cyclic node connection
	void request(int output_id, const ParameterSet &req) 
	{
		if (req.exist("LOOKAHEAD")) {
			outputs[output_id].lookAhead = max(outputs[output_id].lookAhead,dereference_cast<int> (req.get("LOOKAHEAD")));
		}
		
		if (req.exist("LOOKBACK")) {
			outputs[output_id].lookBack = max(outputs[output_id].lookBack,dereference_cast<int> (req.get("LOOKBACK")));
		}
		
		if (req.exist("INORDER")) {
			inOrder = true;
		}
		
		int outputLookAhead=0, outputLookBack=0;
	
		outputLookAhead=max(outputLookAhead, outputs[output_id].lookAhead);
		outputLookBack =max(outputLookBack, outputs[output_id].lookBack);
		
		if (output_id == m_particleOutID) {
			// OUT_PARTICLE output does not required any inputs (if flow is respected)
			return;
			
		}
		else if (output_id == m_likelihoodOutID) {
			// Likelihood output requires features vector
			ParameterSet myReq;
			
			myReq.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_featuresInID].lookAhead+outputLookAhead)));
			myReq.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_featuresInID].lookBack+outputLookBack)));
			inputs[m_featuresInID].node->request(inputs[m_featuresInID].outputID,myReq);
			
			return;
		}
		else if (output_id == m_targetOutID) {
			ParameterSet myReq, myReq2, myReq3;
			/*
			myReq.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_targetInID].lookAhead+outputLookAhead)));
			myReq.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_targetInID].lookBack+outputLookBack)));
			inputs[m_targetInID].node->request(inputs[m_targetInID].outputID,myReq);
			*/
			myReq2.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_meanParticleInID].lookAhead+outputLookAhead)));
			myReq2.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_meanParticleInID].lookBack+outputLookBack)));
			inputs[m_meanParticleInID].node->request(inputs[m_meanParticleInID].outputID,myReq2);
			
			myReq3.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_ppCompletedInID].lookAhead+outputLookAhead)));
			myReq3.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_ppCompletedInID].lookBack+outputLookBack)));
			inputs[m_ppCompletedInID].node->request(inputs[m_ppCompletedInID].outputID,myReq3);
		}
		else if (output_id == m_finishedOutID) {
			ParameterSet myReq, myReq2;
			
			myReq.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_filtFinishedInID].lookAhead+outputLookAhead)));
			myReq.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_filtFinishedInID].lookBack+outputLookBack)));
			inputs[m_filtFinishedInID].node->request(inputs[m_filtFinishedInID].outputID,myReq);
			
			myReq2.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_targetInID].lookAhead+outputLookAhead)));
			myReq2.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_targetInID].lookBack+outputLookBack)));
			inputs[m_targetInID].node->request(inputs[m_targetInID].outputID,myReq2);
		}
		else {
			throw new GeneralException ("VisualTargetPFInterface::request : unknown output ID.",__FILE__,__LINE__);
		}
	}
	
	void calculate(int output_id, int count, Buffer &out)
	{
		try {
			if (output_id == m_finishedOutID) {
				if (m_needNewTarget) {
					// Get current target
					ObjectRef targetObjRef = getInput(m_targetInID, count);
					
					if (!targetObjRef->isNil()) {
						m_curTarget = RCPtr<VisualTarget<double> >(targetObjRef);
						
						if (!m_curTarget->GetCstROI()) {
							throw new GeneralException ("VisualTargetPFInterface::calculate : current VisualTarget has not a valid VisualROI member.",__FILE__,__LINE__);
						}
						
						// Force features preprocessing since we should be 
						// starting a tracking iteration
						RCPtr<Vector<int> > preprocessRef = getInput(m_ppCompletedInID, count);
						
						// Set current sample according to target's ROI
						const VisualROI *roiRef = m_curTarget->GetCstROI();
						float *p_state = m_curSample->GetState();
						
						// Set position
						*p_state++ = (float)(roiRef->GetXCen());
						*p_state++ = (float)(roiRef->GetYCen());
						
						if (m_useScale) {
							// Add scale state
							*p_state++ = (float)(roiRef->GetHSX());
							*p_state++ = (float)(roiRef->GetHSY());
						}
						
						if (m_useAngle) {
							// Add rotation angle state
							*p_state++ = (float)(roiRef->GetAngle());
						}
						
						m_needNewTarget = false;
					}
				}
				
				if (m_needNewTarget) {
					(*outputs[m_finishedOutID].buffer)[count] = ObjectRef(Bool::alloc(false));
				}
				else {
					// See if particle filtering has finished
					bool filtFinished = dereference_cast<bool>(getInput(m_filtFinishedInID, count));
					(*outputs[m_finishedOutID].buffer)[count] = ObjectRef(Bool::alloc(filtFinished));
				}
			}
			else if (output_id == m_likelihoodOutID) {
				// Get features vector
				ObjectRef featObjRef = getInput(m_featuresInID, count);
				
				if (!featObjRef->isNil()) {
					RCPtr<Vector<VisualFeatureDesc<double> *> > featRef = featObjRef;
					
					// Use current reference target to estimate the 
					// likelihood of the current features vector
					float sim = m_curTarget->Similarity(featRef.get());
					//cout << "Similarity = " << sim << endl;
					float likelihood = exp(m_lSigma*(sim-1.f));
					//cout << "Likelihood = " << likelihood << endl;
					
					// Output current likehood
					(*outputs[m_likelihoodOutID].buffer)[count] = ObjectRef(Float::alloc(likelihood));
				}
				else {
					throw new GeneralException ("VisualTargetPFInterface::calculate : invalid (nilObject) IN_FEATURES_VECTOR input.",__FILE__,__LINE__);
				}
			}
			else if (output_id == m_particleOutID) {
				// Simply output current particle reference that should be valid
				(*outputs[m_particleOutID].buffer)[count] = ObjectRef(m_curSample);
			}
			else if (output_id == m_targetOutID) {
				if (m_needNewTarget) {
					// Output nilObject since tracking is just starting
					(*outputs[m_targetOutID].buffer)[count] = nilObject;
				}
				else {
					// Get mean state particle
					ObjectRef particleObjRef = getInput(m_meanParticleInID, count);
				
					if (!particleObjRef->isNil()) {
						RCPtr<PFGenericParticle> meanParticleRef = particleObjRef;
						
						VisualROI *curROI = m_curTarget->GetROI();
						
						// Set current target's ROI according to mean state particle
						const float *p_state = meanParticleRef->GetCstState();
						// Set position
						curROI->SetXCen((int)(*p_state++));
						curROI->SetYCen((int)(*p_state++));
						
						if (m_useScale) {
							// Set scale
							curROI->SetHSX((int)(*p_state++));
							curROI->SetHSY((int)(*p_state++));
						}
						
						if (m_useAngle) {
							// Set rotation angle state
							curROI->SetAngle((int)(*p_state++));
						}
						
						// Set flag to indicate we need a new target
						m_needNewTarget = true;
						
						// Output resulting target
						(*outputs[m_targetOutID].buffer)[count] = ObjectRef(m_curTarget);
					}
					else {
						// Output nilObject since tracking is not finished
						(*outputs[m_targetOutID].buffer)[count] = nilObject;
					}
				}
			}
		}
		catch (BaseException *e) {
			throw e->add(new GeneralException("Exception in VisualTargetPFInterface::calculate:",__FILE__,__LINE__));
		}
	}

private:
	// Input IDs
	int m_targetInID;
	int m_featuresInID;
	int m_meanParticleInID;
	int m_ppCompletedInID;
	int m_filtFinishedInID;
	
	// Output IDs
	int m_particleOutID;
	int m_likelihoodOutID;
	int m_targetOutID;
	int m_finishedOutID;
	
	// Parameters
	bool m_useScale;
	bool m_useAngle;
	float m_lSigma;
	
	unsigned int m_stateSize;
	bool m_needNewTarget;
	RCPtr<VisualTarget<double> > m_curTarget;
	RCPtr<PFGenericParticle> m_curSample;
};

}

#endif
