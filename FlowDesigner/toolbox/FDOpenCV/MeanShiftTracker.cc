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
#include "MeanShiftTracker.h"

using namespace FD;
using namespace std;

namespace RobotFlow {

DECLARE_NODE(MeanShiftTracker)
DECLARE_TYPE(MeanShiftTracker)

  /*Node
   *
   * @name MeanShiftTracker
   * @category RobotFlow:Vision:Tracking
   * @description Mean shift region/object tracker.
   *
   * @parameter_name FRAME_WIDTH
   * @parameter_type int
   * @parameter_value 320
   * @parameter_description Video frame width.
   *
   * @parameter_name FRAME_HEIGHT
   * @parameter_type int
   * @parameter_value 240
   * @parameter_description Video frame height.
   *
   * @parameter_name NUM_CHANNELS
   * @parameter_type int
   * @parameter_value 3
   * @parameter_description Number of channels in video frame.
   *
   * @parameter_name MAX_NUM_MS_ITER
   * @parameter_type int
   * @parameter_value 20
   * @parameter_description Maximum number of mean shift iterations.
   *
   * @parameter_name MS_DIST_EPSILON
   * @parameter_type float
   * @parameter_value 2.0
   * @parameter_description Minimal distance to consider that the mean shift has found a maximum.
   *
   * @input_name IMAGE_IN
   * @input_type Image
   * @input_description Current video frame.
   *
   * @input_name PREPROCESS_COMPLETED
   * @input_type Vector<int>
   * @input_description Flags indicating the completion of the features extraction preprocessing stage.
   *
   * @input_name CURRENT_TARGET
   * @input_type VisualTarget<double>
   * @input_description Current target to track.
   *
   * @input_name MS_LOCATION
   * @input_type Vector<double>
   * @input_description Mean shift vector of location.
   *
   * @output_name TRACKING_FINISHED
   * @output_type bool
   * @output_description Flag indicating that the tracking has completed.
   *
   * @output_name IMAGE_OUT
   * @output_type Image
   * @output_description Current image to process.
   *
   * @output_name CURRENT_TARGET
   * @output_type VisualTarget<double>
   * @output_description Current target being tracked (to compute mean shift location).
   *
   * @output_name TRACKED_TARGET
   * @output_type Image
   * @output_description Target updated according to the tracking algorithm.
   *
   END*/

//
// Mean shift object/region tracker for RobotFlow 
//
// Implementation is based on the following publication:
//
// D. Comaniciu, V. Ramesh, P. Meer. "Kernel-based object tracking", 
// IEEE Transactions on Pattern Analysis Machine Intelligence, vol 25, 
// pp. 564-577, 2003.
//
 
//
// Default constructor for Object 
//
MeanShiftTracker::MeanShiftTracker()
: VisualTracker(),
m_maxNumMSIter(0),
m_minMSDistEpsilon(0.0),
m_curTarget(NULL)
{

}

MeanShiftTracker::MeanShiftTracker(int i_maxNumMSIter, double i_minMSDistEpsilon)
: VisualTracker(),
m_maxNumMSIter(i_maxNumMSIter),
m_minMSDistEpsilon(i_minMSDistEpsilon),
m_curTarget(NULL)
{
	m_curTarget = new VisualTarget<double>();
}

//
// BufferedNode constructor
//
MeanShiftTracker::MeanShiftTracker(string nodeName, ParameterSet params)
: VisualTracker(nodeName, params),
m_curTarget(NULL)
{
	m_imageInID = addInput("IMAGE_IN");
	m_ppCompletedInID = addInput("PREPROCESS_COMPLETED");
	m_targetInID = addInput("CURRENT_TARGET");
	m_msLocVecInID = addInput("MS_LOCATION");

	m_finishedOutID = addOutput("TRACKING_FINISHED");
	m_imageOutID = addOutput("IMAGE_OUT");
	m_curTargetOutID = addOutput("CURRENT_TARGET");
	m_targetOutID = addOutput("TRACKED_TARGET");

	m_width = dereference_cast<int>(parameters.get("FRAME_WIDTH"));
	m_height = dereference_cast<int>(parameters.get("FRAME_HEIGHT"));
	m_numChannels = dereference_cast<int>(parameters.get("NUM_CHANNELS"));
	m_maxNumMSIter = dereference_cast<int>(parameters.get("MAX_NUM_MS_ITER"));
	m_minMSDistEpsilon = dereference_cast<float>(parameters.get("MS_DIST_EPSILON"));
	
	m_numPixels = m_width*m_height;
	m_numBytesInFrame = m_numPixels*m_numChannels;
	
	CvSize imgSize;
	imgSize.width = m_width;
	imgSize.height = m_height;
	
	m_curImage = cvCreateImage(imgSize, IPL_DEPTH_8U, m_numChannels);
	
	m_curTarget = new VisualTarget<double>();
	m_finished = false;
	m_initMS = false;
}
	
MeanShiftTracker::~MeanShiftTracker()
{
	delete m_curTarget;
	cvReleaseImage(&m_curImage);
}

// Modified BufferedNode request method to support cyclic node connection
void MeanShiftTracker::request(int output_id, const ParameterSet &req) 
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
	
	if (output_id == m_finishedOutID) {
		// TRACKING_FINISHED output does not required any inputs
		return;
		
	}
	else if (output_id == m_imageOutID) {
		// IMAGE_OUT output does not required any inputs
		return;
	}
	else if (output_id == m_curTargetOutID) {
		// CURRENT_TARGET output does not required any inputs
		return;
	}
	else if (output_id == m_targetOutID) {
		ParameterSet myReq, myReq2, myReq3, myReq4;
		
		myReq.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_imageInID].lookAhead+outputLookAhead)));
		myReq.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_imageInID].lookBack+outputLookBack)));
		inputs[m_imageInID].node->request(inputs[m_imageInID].outputID,myReq);
		
		myReq2.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_ppCompletedInID].lookAhead+outputLookAhead)));
		myReq2.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_ppCompletedInID].lookBack+outputLookBack)));
		inputs[m_ppCompletedInID].node->request(inputs[m_ppCompletedInID].outputID,myReq2);
		
		myReq3.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_targetInID].lookAhead+outputLookAhead)));
		myReq3.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_targetInID].lookBack+outputLookBack)));
		inputs[m_targetInID].node->request(inputs[m_targetInID].outputID,myReq3);
		
		myReq4.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_msLocVecInID].lookAhead+outputLookAhead)));
		myReq4.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_msLocVecInID].lookBack+outputLookBack)));
		inputs[m_msLocVecInID].node->request(inputs[m_msLocVecInID].outputID,myReq4);
	}
	else {
		throw new GeneralException ("MeanShiftTracker::request : unknown output ID.",__FILE__,__LINE__);
	}
}

void MeanShiftTracker::calculate(int output_id, int count, Buffer &out)
{
	try {
		if (output_id == m_finishedOutID) {
			(*outputs[m_finishedOutID].buffer)[count] = ObjectRef(Bool::alloc(!m_finished));
			if (m_finished) {
				m_initMS = false;
				m_finished = false;
			}
		}
		else if (output_id == m_imageOutID) {
			// Copy current image for output
			Image *outImage = Image::alloc(m_width, m_height, m_numChannels);
			memcpy(outImage->get_data(), m_curImage->imageData, m_numBytesInFrame);
			
			(*outputs[m_imageOutID].buffer)[count] = ObjectRef(outImage);
		}
		else if (output_id == m_curTargetOutID) {
			// Output current target
			if (m_curTarget->GetCstROI()) {
				//VisualTarget<double> *outTarget = new VisualTarget<double>(*m_curTarget);
				static RCPtr<VisualTarget<double> > refTarget = RCPtr<VisualTarget<double> >(m_curTarget);
				(*outputs[m_curTargetOutID].buffer)[count] = ObjectRef(refTarget);
			}
			else {
				(*outputs[m_curTargetOutID].buffer)[count] = ObjectRef(nilObject);
			}
		}
		else if (output_id == m_targetOutID){
			if (!m_initMS) {
				// First get current target to track
				ObjectRef targetRef = getInput(m_targetInID, count);
				
				if (targetRef->isNil()) {
					// Invalid target, output nilObject
					(*outputs[m_targetOutID].buffer)[count] = ObjectRef(nilObject);
					return;
				}
				
				// Set current target reference to tracker's target model
				*m_curTarget = object_cast<VisualTarget<double> >(targetRef);
				
				// Get current image
				RCPtr<Image> imageRef = getInput(m_imageInID, count);
				
				// Verify input image sanity
				if (imageRef->get_width() != m_width ||
					imageRef->get_height() != m_height ||
					imageRef->get_pixelsize() != m_numChannels) {
					throw new GeneralException ("MeanShiftTracker::calculate : image parameters do not correspond to given input.",__FILE__,__LINE__);
				}
				
				memcpy(m_curImage->imageData, imageRef->get_data(), m_numBytesInFrame);
				
				// Force features extraction preprocessing
				RCPtr<Vector<int> > preprocessRef = getInput(m_ppCompletedInID, count);
				
				m_numIter = 0;
				
				m_initMS = true;
			}
			
			
			// Apply mean shift tracking
			int numDesc = m_curTarget->GetNumDescriptors();
			double distanceMoved = 0.0;

			int xLoc = m_curTarget->GetCstROI()->GetXCen();
			int yLoc = m_curTarget->GetCstROI()->GetYCen();
			double newXLoc = 0.0;
			double newYLoc = 0.0;
			
			// Get mean shift location
			RCPtr<Vector<double> > msLocVecRef = getInput(m_msLocVecInID, count);
			
			// For each visual cue
			for (int i=0; i<numDesc; i++) {
				// Location of target is a weighted sum of all the cues locations
				newXLoc += (*msLocVecRef)[2*i];
				newYLoc += (*msLocVecRef)[2*i+1];
			}
			
			// Compute distance moved during this iteration
			double deltaX = newXLoc - (double)xLoc;
			double deltaY = newYLoc - (double)yLoc;
			distanceMoved = sqrt(deltaX*deltaX + deltaY*deltaY);
			
			// Set new location
			xLoc = (int)(round(newXLoc));
			yLoc = (int)(round(newYLoc));
			m_curTarget->GetROI()->SetXCen(xLoc);
			m_curTarget->GetROI()->SetYCen(yLoc);

			m_numIter++;

			if ( (distanceMoved <= m_minMSDistEpsilon) || (m_numIter >= m_maxNumMSIter) ) {
				m_finished = true;
			}
				
			if (!m_finished) {
				(*outputs[m_targetOutID].buffer)[count] = ObjectRef(nilObject);
			}
			else {
				// Output current target with ROI position updated
				// A copy must be done since it will be probably be modified
				//VisualTarget<double> *outTarget = new VisualTarget<double>(*m_curTarget);
				static RCPtr<VisualTarget<double> > outTarget = RCPtr<VisualTarget<double> >(m_curTarget);
				(*outputs[m_targetOutID].buffer)[count] = ObjectRef(outTarget);
			}
		}
	}
	catch (BaseException *e) {
		throw e->add(new GeneralException("Exception in MeanShiftTracker::calculate:",__FILE__,__LINE__));
	}
}

	// Make a mean shift iteration
	
	// Make a weighted sum of the features and adapt the weights
	// rate*w_new = quality_mesurement - w_old
	// where quality_mesurement = normDist(prob(estimate), avg(prob(all pos for current cue)))
	// or
	// w_new = rateInv*quality_mesurement - (1-rateInv)*w_old
	// quality_mesurement = likelihood(estimate from last frame)/sum(likelihood(all pos for current cue from last frame))
	// or quality_mesurement => {exp(-error)|current_cue}/sum(exp(-error)|for all cues)
	//
	// For mean shift:
	//	For each cue:
	//			For each pixel in ROI
	//				probRef = m_curTarget.GetCstDescriptorsVec.getProb(pixel(x,y))
	//				probTmp = i_descVec.getProb(pixel(x,y))
	//				if (probTmp > 0)
	//					wi=cueWeight*sqrt(probRef/probTmp);
	//					float kern = derivateKernel(window.getCenter(),position,radius);
	//					numerator.x += wi*kern*position.x;
	//					numerator.y += wi*kern*position.y;
	//					denominator += wi*kern;
	//				end if
	//			end for
	//		end for
	//		estimate new location
	//	end for
	// end mean shift
	//
	// double sumLikelihhods = 0.0;
	// For each cue:
	//	likelihood = EstimateTarget
	//	sumLikelihhods += likelihood
	// end for
	// For each cue:
	//	cueWeight = rateInv*likelihood/sumLikelihhods + (1-rateInv)*cueWeight;
	// end for

}//namespace RobotFlow
