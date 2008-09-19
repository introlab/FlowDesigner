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
#include "VisualTargetManager.h"

using namespace FD;
using namespace std;

namespace RobotFlow {

DECLARE_NODE(VisualTargetManager)
DECLARE_TYPE(VisualTargetManager)

  /*Node
   *
   * @name VisualTargetManager
   * @category RobotFlow:Vision:Tracking
   * @description Target manager to handle target initialization, adaptation and deletion.
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
   * @parameter_name MAX_NUM_TARGET
   * @parameter_type int
   * @parameter_value 10
   * @parameter_description Maximum number of target to keep track of.
   *
   * @parameter_name TARGET_MATCH_THRES
   * @parameter_type float
   * @parameter_value 0.6
   * @parameter_description Minimum confidence to assume a target match.
   *
   * @parameter_name TARGET_ADAPT_THRES
   * @parameter_type float
   * @parameter_value 0.9
   * @parameter_description Adaptation treshold for each target cue model.
   *
   * @parameter_name TARGET_ADAPT_RATE
   * @parameter_type float
   * @parameter_value 0.1
   * @parameter_description Adaptation rate for each target cue model.
   *
   * @parameter_name CUE_ADAPT_RATE
   * @parameter_type float
   * @parameter_value 0.2
   * @parameter_description Adaptation rate for each cue weight.
   *
   * @parameter_name LIKELIHOOD_SIGMA
   * @parameter_type float
   * @parameter_value 200.0
   * @parameter_description Exponential sigma value to discriminate likelihood.
   *
   * @input_name IMAGE_IN
   * @input_type Image
   * @input_description Current video frame.
   *
   * @input_name PREPROCESS_COMPLETED
   * @input_type Vector<int>
   * @input_description Flags indicating the completion of the features extraction preprocessing stage.
   *
   * @input_name DETECTED_ROI
   * @input_type VisualROI
   * @input_description A region of interest detected as a potential target region.
   *
   * @input_name FEATURES_VECTOR
   * @input_type Vector<VisualFeatureDesc<double> *>
   * @input_description Features descriptors vector (probably requested to adapt or init a target)
   *
   * @input_name TRACKED_TARGET
   * @input_type VisualTarget<double>
   * @input_description An updated target by a tracking routine.
   *
   * @output_name IMAGE_OUT
   * @output_type Image
   * @output_description Current image with identified target.
   *
   * @output_name CURRENT_ROI
   * @output_type VisualROI
   * @output_description Current region of interest to extract features.
   *
   * @output_name CURRENT_TARGET
   * @output_type VisualTarget<double>
   * @output_description Current target to track.
   *
   * @output_name TARGET_PROB
   * @output_type double
   * @output_description Current target likelihood at current tracked position.
   *
   * @output_name TARGET_DELTA_X
   * @output_type float
   * @output_description Current target position relative to the image x center.
   *
   * @output_name TARGET_DELTA_Y
   * @output_type float
   * @output_description Current target position relative to the image y center.
   *
   * @output_name FRAME_NAME
   * @output_type string
   * @output_description Current frame name for saving purposes.
   *
   END*/

//
// Default constructor for Object 
//
VisualTargetManager::VisualTargetManager()
{

}

//
// BufferedNode constructor
//
VisualTargetManager::VisualTargetManager(std::string nodeName, ParameterSet params)
: BufferedNode(nodeName, params)
{
	m_imageInID = addInput("IMAGE_IN");
	m_ppCompletedInID = addInput("PREPROCESS_COMPLETED");
	m_roiInID = addInput("DETECTED_ROI");
	m_featVecInID = addInput("FEATURES_VECTOR");
	m_targetInID = addInput("TRACKED_TARGET");

	m_imageOutID = addOutput("IMAGE_OUT");
	m_roiOutID = addOutput("CURRENT_ROI");
	m_targetOutID = addOutput("CURRENT_TARGET");
	m_targetProbOutID = addOutput("TARGET_PROB");
	m_targetDXOutID = addOutput("TARGET_DELTA_X");
	m_targetDYOutID = addOutput("TARGET_DELTA_Y");
	m_nameOutID = addOutput("FRAME_NAME");

	m_width = dereference_cast<int>(parameters.get("FRAME_WIDTH"));
	m_height = dereference_cast<int>(parameters.get("FRAME_HEIGHT"));
	m_numChannels = dereference_cast<int>(parameters.get("NUM_CHANNELS"));
	m_maxNumTargets = dereference_cast<int>(parameters.get("MAX_NUM_TARGET"));
	m_targetMatchThres = dereference_cast<float>(parameters.get("TARGET_MATCH_THRES"));
	m_targetAdaptThres = dereference_cast<float>(parameters.get("TARGET_ADAPT_THRES"));
	m_targetAdaptRate = dereference_cast<float>(parameters.get("TARGET_ADAPT_RATE"));
	m_cueAdaptRate = dereference_cast<float>(parameters.get("CUE_ADAPT_RATE"));
	m_lSigma = dereference_cast<float>(parameters.get("LIKELIHOOD_SIGMA"));
	
	m_numPixels = m_width*m_height;
	m_numBytesInFrame = m_numPixels*m_numChannels;
	m_imgXCen = (float)(m_width)*0.5f;
	m_imgYCen = (float)(m_height)*0.25f;
	
	CvSize imgSize;
	imgSize.width = m_width;
	imgSize.height = m_height;
	
	m_curImage = cvCreateImage(imgSize, IPL_DEPTH_8U, m_numChannels);
	
	m_refTarget = RCPtr<VisualTarget<double> >(new VisualTarget<double>());
	
	m_roiColor[0] = 255;
	m_roiColor[1] = 0;
	m_roiColor[2] = 0;
}
	
VisualTargetManager::~VisualTargetManager()
{
	cvReleaseImage(&m_curImage);
}

// Modified BufferedNode request method to support cyclic node connection
void VisualTargetManager::request(int output_id, const ParameterSet &req) 
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
	
	if (output_id == m_targetProbOutID) {
		ParameterSet myReq, myReq2, myReq3, myReq4;
		
		myReq.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_roiInID].lookAhead+outputLookAhead)));
		myReq.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_roiInID].lookBack+outputLookBack)));
		inputs[m_roiInID].node->request(inputs[m_roiInID].outputID,myReq);
		
		myReq2.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_ppCompletedInID].lookAhead+outputLookAhead)));
		myReq2.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_ppCompletedInID].lookBack+outputLookBack)));
		inputs[m_ppCompletedInID].node->request(inputs[m_ppCompletedInID].outputID,myReq2);
		
		myReq3.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_targetInID].lookAhead+outputLookAhead)));
		myReq3.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_targetInID].lookBack+outputLookBack)));
		inputs[m_targetInID].node->request(inputs[m_targetInID].outputID,myReq3);
		
		myReq4.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_featVecInID].lookAhead+outputLookAhead)));
		myReq4.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_featVecInID].lookBack+outputLookBack)));
		inputs[m_featVecInID].node->request(inputs[m_featVecInID].outputID,myReq4);
	}
	else if (output_id == m_targetOutID) {
		// CURRENT_TARGET output does not required any inputs
		return;
	}
	else if (output_id == m_roiOutID) {
		// CURRENT_ROI output does not required any inputs
		return;
	}
	else if (output_id == m_imageOutID) {
		ParameterSet myReq;
		
		myReq.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_imageInID].lookAhead+outputLookAhead)));
		myReq.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_imageInID].lookBack+outputLookBack)));
		inputs[m_imageInID].node->request(inputs[m_imageInID].outputID, myReq);
	}
	else if (output_id == m_targetDXOutID) {
		// TARGET_DELTA_X output does not required any inputs
		return;
	}
	else if (output_id == m_targetDYOutID) {
		// TARGET_DELTA_Y output does not required any inputs
		return;
	}
	else if (output_id == m_nameOutID) {
		// FRAME_NAME output does not required any inputs
		return;
	}
	else {
		throw new GeneralException ("VisualTargetManager::request : unknown output ID.",__FILE__,__LINE__);
	}

}

void VisualTargetManager::calculate(int output_id, int count, Buffer &out)
{
	try {
		if (output_id == m_imageOutID) {
			// Get current image
			RCPtr<Image> imageRef = getInput(m_imageInID, count);
			
			// Verify input image sanity
			if (imageRef->get_width() != m_width ||
				imageRef->get_height() != m_height ||
				imageRef->get_pixelsize() != m_numChannels) {
				throw new GeneralException ("VisualTargetManager::calculate : image parameters do not correspond to given input.",__FILE__,__LINE__);
			}
			
			memcpy(m_curImage->imageData, imageRef->get_data(), m_numBytesInFrame);
			
			if (m_refTarget->IsValid()) {
				// Draw current target's ROI in current image
				m_refTarget->GetCstROI()->DrawROI(m_curImage, (const unsigned char *)m_roiColor);
				
				// Copy current image for output
				Image *outImage = Image::alloc(m_width, m_height, m_numChannels);

				unsigned char *p_src = (unsigned char *)m_curImage->imageData;
				unsigned char *p_dst = (unsigned char *)outImage->get_data();
				for (int p=0; p<m_numPixels; p++) {
					p_dst[0] = p_src[2];
					p_dst[1] = p_src[1];
					p_dst[2] = p_src[0];
					p_dst += 3;
					p_src += 3;
				}
				//memcpy(outImage->get_data(), m_curImage->imageData, m_numBytesInFrame);
				
				(*outputs[m_imageOutID].buffer)[count] = ObjectRef(outImage);
			}
			else {
				// If no current target, output unmodified image
				(*outputs[m_imageOutID].buffer)[count] = ObjectRef(imageRef);
			}
		}
		else if (output_id == m_roiOutID) {
			if (m_refTarget->IsValid()) {
				// Output directly the reference since it should
				// only be used as a constant reference
				(*outputs[m_roiOutID].buffer)[count] = m_refTarget->GetROIRCPtr();
			}
			else {
				// No ROI initialized, so output nilObject
				(*outputs[m_roiOutID].buffer)[count] = ObjectRef(nilObject);
			}
		}
		else if (output_id == m_targetOutID) {
			if (m_refTarget->IsValid()) {
				// Output directly the reference since it should
				// only be used as a constant reference
				(*outputs[m_targetOutID].buffer)[count] = m_refTarget;
			}
			else {
				// No ROI initialized, so output nilObject
				(*outputs[m_targetOutID].buffer)[count] = ObjectRef(nilObject);
			}
		}
		else if (output_id == m_targetProbOutID) {
			if (m_refTarget->GetCurrentAge() < -10) {
				// Target is too old to track
				// Invalidate it
				m_refTarget->SetValidity(false);
			}
				
			if (!m_refTarget->IsValid()) {
				// Try to initialize a new target
				// Get detected ROI
				ObjectRef roiRef = getInput(m_roiInID, count);
				m_ppCompleted = false;
				
				if (!roiRef->isNil()) {
					// Initialize target at current ROI
					RCPtr<VisualROI> roiRefPtr = roiRef;
					m_refTarget->SetROI(roiRefPtr.get());
					m_refTarget->SetValidity(true);
					
					if (!m_ppCompleted) {
						// Force features extraction preprocessing
						RCPtr<Vector<int> > preprocessRef = getInput(m_ppCompletedInID, count);
						m_ppCompleted = true;
					}
					
					// Get features for current ROI and use them
					// as the target feature model
					ObjectRef featVecObjRef = getInput(m_featVecInID, count);
					if (featVecObjRef->isNil()) {
						throw new GeneralException ("VisualTargetManager::calculate : cannot initialize target with null input FEATURES_VECTOR.",__FILE__,__LINE__);
					}
					RCPtr<Vector<VisualFeatureDesc<double> *> > featVecRef = featVecObjRef;
					m_refTarget->SetDescriptorsVec(&(*featVecRef));
					m_refTarget->InitCueWeights();
					m_refTarget->InitAges();
				}
			}
			
			if (m_refTarget->IsValid()) {
				double sim = 0.0;
				bool targetMatch = false;
				
				// Force features extraction preprocessing only once per frame
				if (!m_ppCompleted) {
					RCPtr<Vector<int> > preprocessRef = getInput(m_ppCompletedInID, count);
					m_ppCompleted = true;
				}
				
				// Start timer
				ftime(&m_t1);
				
				// Track current target
				ObjectRef targetRef = getInput(m_targetInID, count);
				
				if (!targetRef->isNil()) {
					*m_refTarget = object_cast<VisualTarget<double> >(targetRef);
				
					// Get features to evalute the likelihood of the current
					// target at the given tracked position
					ObjectRef featVecRef = getInput(m_featVecInID, count);
					if (featVecRef->isNil()) {
						throw new GeneralException ("VisualTargetManager::calculate : cannot evaluate the target's likelihood with null input FEATURES_VECTOR.",__FILE__,__LINE__);
					}
					RCPtr<Vector<VisualFeatureDesc<double> *> > featVecPtr = featVecRef;
					sim = m_refTarget->SimilarityWCueAdapt(featVecPtr.get(), m_cueAdaptRate);
					//sim = m_refTarget->Similarity(featVecPtr.get());
					cout << "Similarity = " << sim << endl;
					//float likelihood = exp(m_lSigma*(sim-1.f));
					//cout << "Likelihood = " << likelihood << endl;
					targetMatch = (sim >= m_targetMatchThres);
					
					m_refTarget->AgeTarget(targetMatch);
					if (sim > m_targetAdaptThres) {
						m_refTarget->Adapt(featVecPtr.get(), m_targetAdaptRate);
					}
				}
				
				// End timer
				ftime(&m_t2);
				
				// Display time used
				double timeDiff=(m_t2.time-m_t1.time)+((m_t2.millitm-m_t1.millitm)/1000.0);
				cout << "Total run time (sec): " << timeDiff << endl;
				
				(*outputs[m_targetProbOutID].buffer)[count] = ObjectRef(Double::alloc(sim));
			}
			else {
				// If no current target, output 0.0
				(*outputs[m_targetProbOutID].buffer)[count] = ObjectRef(Double::alloc(0.0));
			}
		}
		else if (output_id == m_targetDXOutID) {
			if (m_refTarget->IsValid()) {
				float deltaX = (float)(m_refTarget->GetCstROI()->GetXCen()) - m_imgXCen;
				(*outputs[m_targetDXOutID].buffer)[count] = ObjectRef(Float::alloc(deltaX));
			}
			else {
				(*outputs[m_targetDXOutID].buffer)[count] = nilObject;
			}
		}
		else if (output_id == m_targetDYOutID) {
			if (m_refTarget->IsValid()) {
				float deltaY = m_imgYCen - (float)(m_refTarget->GetCstROI()->GetYCen());
				(*outputs[m_targetDYOutID].buffer)[count] = ObjectRef(Float::alloc(deltaY));
			}
			else {
				(*outputs[m_targetDYOutID].buffer)[count] = nilObject;
			}
		}
		else if (output_id == m_nameOutID) {
			String *outName = new String("/home/moip1501/images/FaceTracking");
			char idx[12];
			sprintf(idx, "%05d", count);
			(*outName) += idx;
			(*outName) += ".jpg";

			(*outputs[m_nameOutID].buffer)[count] = ObjectRef(outName);
		}
	}
	catch (BaseException *e) {
		throw e->add(new GeneralException("Exception in VisualTargetManager::calculate:",__FILE__,__LINE__));
	}
}

}//namespace RobotFlow
