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

#include "BufferedNode.h"
#include <iostream>
#include "Image.h"
#include "cv.h"
#include "VisualTarget.h"
#include "PFGenericParticleFilter.h"
#include "PFPMRandomWalk.h"
#include "IntegralColorExtraction.h"
#include "IntegralEdgesOriExtraction.h"
#include "IntegralLBPExtraction.h"
#include <stdlib.h>
#include <sys/timeb.h>

using namespace FD;
using namespace std;

namespace RobotFlow {

class MultiIntegralCuesPFTracker;

DECLARE_NODE(MultiIntegralCuesPFTracker)

  /*Node
   *
   * @name MultiIntegralCuesPFTracker
   * @category RobotFlow:Vision:Tracking
   * @description Full tracker implementation using different types of integral features.
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
   * @parameter_name NUM_HORI_RECT
   * @parameter_type int
   * @parameter_value 2
   * @parameter_description Number of horizontal rectangle to use for integral features.
   *
   * @parameter_name NUM_VERT_RECT
   * @parameter_type int
   * @parameter_value 2
   * @parameter_description Number of vertical rectangle to use for integral features.
   *
   * @parameter_name USE_RECT_DIFF
   * @parameter_type bool
   * @parameter_value true
   * @parameter_description Features are differences between rectangle regions.
   *
   * @parameter_name USE_COLOR_BOUNDARY
   * @parameter_type bool
   * @parameter_value true
   * @parameter_description Flag indicating to detect a color boundary for a better scale adaptation.
   *
   * @parameter_name BOUNDARY_DIFF_THRESH
   * @parameter_type float
   * @parameter_value 20.0
   * @parameter_description Minimum difference between boundary and ROI rectangle mean values.
   *
   * @parameter_name NUM_ORIENTATIONS
   * @parameter_type int
   * @parameter_value 8
   * @parameter_description Number of edge orientations.
   *
   * @parameter_name MIN_EDGES_STRENGTH
   * @parameter_type float
   * @parameter_value 100.0
   * @parameter_description Threshold to remove noisy/weak edges.
   *
   * @parameter_name MAX_EDGES_STRENGTH
   * @parameter_type float
   * @parameter_value 1000.0
   * @parameter_description Limit on the edge strength.
   *
   * @parameter_name NUM_PARTICLES
   * @parameter_type int
   * @parameter_value 1000
   * @parameter_description Number of particles (samples) to use.
   *
   * @parameter_name PARTICLE_STATE_SIZE
   * @parameter_type int
   * @parameter_value 4
   * @parameter_description Particle state size.
   *
   * @parameter_name COLOR_CUE_WEIGHT
   * @parameter_type float
   * @parameter_value 0.4
   * @parameter_description Color features weight.
   *
   * @parameter_name EDGES_CUE_WEIGHT
   * @parameter_type float
   * @parameter_value 0.3
   * @parameter_description Color features weight.
   *
   * @parameter_name LBP_CUE_WEIGHT
   * @parameter_type float
   * @parameter_value 0.3
   * @parameter_description Color features weight.
   *
   * @parameter_name REDETECTION_FREQUENCY
   * @parameter_type int
   * @parameter_value 30
   * @parameter_description Frequency of application of redetection of current tracked region (in number of frames).
   *
   * @input_name INIT_VARIANCE
   * @input_type Vector<float>
   * @input_description Variance for initialization of each of the particle state.
   *
   * @input_name NOISE_VARIANCE
   * @input_type Vector<float>
   * @input_description Noise variance for each of the particle state.
   *
   * @input_name IMAGE_IN
   * @input_type Image
   * @input_description Current video frame.
   *
   * @input_name DETECTED_ROI
   * @input_type VisualROI
   * @input_description A region of interest detected as a potential target region.
   *
   * @input_name EDGES_ORI_SUM_IMG
   * @input_type Vector<double *>
   * @input_description Reference to edges orientation integral images. While reference is not null, no edges preprocessing will be done.
   *
   * @input_name SHOW_TRACKED_ROI
   * @input_type bool
   * @input_description Flag indicating to show the current tracked region.
   *
   * @input_name FRAME_BASENAME
   * @input_type string
   * @input_description Path and basename for the frames filename that will be produced as an output.
   *
   * @input_name ACTIVATION
   * @input_type bool
   * @input_description Node activation flag.
   *
   * @output_name IMAGE_OUT
   * @output_type Image
   * @output_description Current image with identified target.
   *
   * @output_name CURRENT_ROI
   * @output_type VisualROI
   * @output_description Current region of interest corresponding to the tracked target.
   *
   * @output_name CURRENT_TARGET
   * @output_type VisualTarget<double>
   * @output_description Current target to track.
   *
   * @output_name TARGET_PROB
   * @output_type float
   * @output_description Current target likelihood at current tracked position.
   *
   * @output_name ROI_FOR_DETECTION
   * @output_type VisualROI
   * @output_description Current region of interest corresponding to the tracked target. IMPORTANT NOTE:  Do not pull ROI_FOR_DETECTION output first since this will result in the output to be not synchoronized with the frames and the tracking process.
   *
   * @output_name FRAME_FILENAME
   * @output_type string
   * @output_description Filename for the current frame to save.
   *
   END*/

class MultiIntegralCuesPFTracker : public BufferedNode
{
public:
	//
	// BufferedNode constructor
	//
	MultiIntegralCuesPFTracker(string nodeName, ParameterSet params)
	: BufferedNode(nodeName, params),
	m_initDone(false),
	m_needInitTargetFeat(false),
	m_tmpSample(NULL),
	m_particleFilter(NULL),
	m_predModel(NULL),
	m_intClrExtract(NULL),
	m_intEdgExtract(NULL),
	m_intLBPExtract(NULL)
	{
		try {
			m_initVarianceInID = addInput("INIT_VARIANCE");
			m_noiseVarianceInID = addInput("NOISE_VARIANCE");
			m_imageInID = addInput("IMAGE_IN");
			m_roiInID = addInput("DETECTED_ROI");
			m_edgesOriInID = addInput("EDGES_ORI_SUM_IMG");
			m_showROIInID = addInput("SHOW_TRACKED_ROI");
			m_baseNameInID = addInput("FRAME_BASENAME");
			m_activatedInID = addInput("ACTIVATION");
		
			m_imageOutID = addOutput("IMAGE_OUT");
			m_roiOutID = addOutput("CURRENT_ROI");
			m_targetOutID = addOutput("CURRENT_TARGET");
			m_targetProbOutID = addOutput("TARGET_PROB");
			m_detectROIOutID = addOutput("ROI_FOR_DETECTION");
			m_frameNameOutID = addOutput("FRAME_FILENAME");
		
			m_width = dereference_cast<int>(parameters.get("FRAME_WIDTH"));
			m_height = dereference_cast<int>(parameters.get("FRAME_HEIGHT"));
			m_numChannels = dereference_cast<int>(parameters.get("NUM_CHANNELS"));
			m_targetMatchThres = dereference_cast<float>(parameters.get("TARGET_MATCH_THRES"));
			m_targetAdaptThres = dereference_cast<float>(parameters.get("TARGET_ADAPT_THRES"));
			m_targetAdaptRate = dereference_cast<float>(parameters.get("TARGET_ADAPT_RATE"));
			m_cueAdaptRate = dereference_cast<float>(parameters.get("CUE_ADAPT_RATE"));
			m_lSigma = dereference_cast<float>(parameters.get("LIKELIHOOD_SIGMA"));
			m_numHoriIntRect = dereference_cast<int>(parameters.get("NUM_HORI_RECT"));
			m_numVertIntRect = dereference_cast<int>(parameters.get("NUM_VERT_RECT"));
			m_useRectDiff = dereference_cast<bool>(parameters.get("USE_RECT_DIFF"));
			m_useBoundary = dereference_cast<bool>(parameters.get("USE_COLOR_BOUNDARY"));
			m_boundaryMeanDiffThresh = dereference_cast<float>(parameters.get("BOUNDARY_DIFF_THRESH"));
			m_numOriBins = dereference_cast<int>(parameters.get("NUM_ORIENTATIONS"));
			m_edgesStrTresh = dereference_cast<float>(parameters.get("MIN_EDGES_STRENGTH"));
			m_maxStrengthValue = dereference_cast<float>(parameters.get("MAX_EDGES_STRENGTH"));
			m_numSamples = dereference_cast<int>(parameters.get("NUM_PARTICLES"));
			m_sampleStateSize = dereference_cast<int>(parameters.get("PARTICLE_STATE_SIZE"));
			m_colorCueWeight = dereference_cast<float>(parameters.get("COLOR_CUE_WEIGHT"));
			m_edgesCueWeight = dereference_cast<float>(parameters.get("EDGES_CUE_WEIGHT"));
			m_LBPCueWeight = dereference_cast<float>(parameters.get("LBP_CUE_WEIGHT"));
			m_redetectFreq = dereference_cast<int>(parameters.get("REDETECTION_FREQUENCY"));
	
			m_numPixels = m_width*m_height;
			m_numBytesInFrame = m_numPixels*m_numChannels;
			
			CvSize imgSize;
			imgSize.width = m_width;
			imgSize.height = m_height;
			
			m_curImage = cvCreateImage(imgSize, IPL_DEPTH_8U, m_numChannels);
			
			m_tmpSample = new PFGenericParticle(m_sampleStateSize);
			
			m_refTarget = RCPtr<VisualTarget<double> >(new VisualTarget<double>());
			m_featVecRef = RCPtr<Vector<VisualFeatureDesc<double> *> >(Vector<VisualFeatureDesc<double> *>::alloc(3));
		
			m_intClrExtract = new IntegralColorExtraction(m_width, m_height, 
				m_numChannels, m_numHoriIntRect, m_numVertIntRect,
				255.0, m_useRectDiff, m_useBoundary, m_boundaryMeanDiffThresh);
			m_intEdgExtract = new IntegralEdgesOriExtraction(m_width, m_height, 
				m_numChannels, m_numHoriIntRect, m_numVertIntRect, m_numOriBins, 
				m_edgesStrTresh, m_maxStrengthValue, m_useRectDiff);
			m_intLBPExtract = new IntegralLBPExtraction(m_width, m_height, 
				m_numChannels, m_numHoriIntRect, m_numVertIntRect,
				8, 1, false, true, 0, 255.0, m_useRectDiff);
			
			m_curSampleROIRef = RCPtr<VisualROI>(new VisualROI());
			m_curSampleROIRef->SetType(e_VISUALROI_rectangular);
			
			m_roiColor[0] = 255;
			m_roiColor[1] = 0;
			m_roiColor[2] = 0;
			
			m_numSkipForRedetect = 0;
		}
		catch (BaseException *e) {
			throw e->add(new GeneralException("Exception in MultiIntegralCuesPFTracker::MultiIntegralCuesPFTracker:",__FILE__,__LINE__));
		}
	}
		
	virtual ~MultiIntegralCuesPFTracker()
	{
		delete m_predModel;
		delete m_particleFilter;
		
		delete m_intLBPExtract;
		delete m_intEdgExtract;
		delete m_intClrExtract;
		
		// This should be deleted by the particle filter
		//delete m_tmpSample;
		
		cvReleaseImage(&m_curImage);
	}
	
	// Modified BufferedNode request method to support cyclic node connection
	virtual void request(int output_id, const ParameterSet &req) 
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
		
		// Always request every inputs except CURRENT_ROI
		// for feedback between tracking and detection.
		ParameterSet actReq;
		actReq.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_activatedInID].lookAhead+outputLookAhead)));
		actReq.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_activatedInID].lookBack+outputLookBack)));
		inputs[m_activatedInID].node->request(inputs[m_activatedInID].outputID,actReq);
		
		if (output_id == m_detectROIOutID) {
			// ROI_FOR_DETECTION output does not required any inputs
			// for feedback between tracking and detection.
			
			// IMPORTANT NOTE:
			// Do not pull CURRENT_ROI output first since this
			// will result in the output to be not synchoronized
			// with the frames and the tracking process.
			return;
		}
		else if (output_id == m_frameNameOutID) {
			ParameterSet myReq;
			myReq.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_baseNameInID].lookAhead+outputLookAhead)));
			myReq.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_baseNameInID].lookBack+outputLookBack)));
			inputs[m_baseNameInID].node->request(inputs[m_baseNameInID].outputID,myReq);
		}
		else if (output_id == m_imageOutID ||
			output_id == m_roiOutID ||
			output_id == m_targetOutID ||
			output_id == m_targetProbOutID) {
			ParameterSet myReq, myReq2, myReq3, myReq4, myReq5;
			myReq.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_initVarianceInID].lookAhead+outputLookAhead)));
			myReq.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_initVarianceInID].lookBack+outputLookBack)));
			inputs[m_initVarianceInID].node->request(inputs[m_initVarianceInID].outputID,myReq);
			
			myReq2.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_imageInID].lookAhead+outputLookAhead)));
			myReq2.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_imageInID].lookBack+outputLookBack)));
			inputs[m_imageInID].node->request(inputs[m_imageInID].outputID,myReq2);
			
			myReq3.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_roiInID].lookAhead+outputLookAhead)));
			myReq3.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_roiInID].lookBack+outputLookBack)));
			inputs[m_roiInID].node->request(inputs[m_roiInID].outputID,myReq3);
			
			myReq4.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_edgesOriInID].lookAhead+outputLookAhead)));
			myReq4.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_edgesOriInID].lookBack+outputLookBack)));
			inputs[m_edgesOriInID].node->request(inputs[m_edgesOriInID].outputID,myReq4);
			
			myReq5.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_showROIInID].lookAhead+outputLookAhead)));
			myReq5.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_showROIInID].lookBack+outputLookBack)));
			inputs[m_showROIInID].node->request(inputs[m_showROIInID].outputID,myReq5);
			
		}
		else {
			throw new GeneralException ("MultiIntegralCuesPFTracker::request : unknown output ID.",__FILE__,__LINE__);
		}
	
	}
	
	void calculate(int output_id, int count, Buffer &out)
	{
		try {
			// Get activation flag 
			m_activated = getInput(m_activatedInID, count);
			
			if (!(*m_activated)) {
				// Deactivate current tracked region
				m_refTarget->SetValidity(false);
				
				// Output nilObjects and return
				(*outputs[m_imageOutID].buffer)[count] = nilObject;
				(*outputs[m_roiOutID].buffer)[count] = nilObject;
				(*outputs[m_targetOutID].buffer)[count] = nilObject;
				(*outputs[m_targetProbOutID].buffer)[count] = ObjectRef(Float::alloc(0.0));
				(*outputs[m_detectROIOutID].buffer)[count] = nilObject;
				(*outputs[m_frameNameOutID].buffer)[count] = nilObject;
				return;
			}
			
			if (output_id == m_frameNameOutID) {
				// Get frame basename
				RCPtr<String> baseName = getInput(m_baseNameInID, count);
				String *frameName = new String();
				(*frameName) += (*baseName);
				const int strSize = 12;
				char idx[strSize];
				snprintf(idx, strSize, "%011d", count);
				(*frameName) += idx;
				(*frameName) += ".jpg";
	
				(*outputs[m_frameNameOutID].buffer)[count] = ObjectRef(frameName);
			}
			
			if (output_id == m_detectROIOutID) {
				if (m_refTarget->IsValid() && m_numSkipForRedetect < m_redetectFreq) {
					// Output current tracked ROI
					(*outputs[m_detectROIOutID].buffer)[count] = m_refTarget->GetROIRCPtr();;
				}
				else {
					// Not a valid target, so output nilObject
					(*outputs[m_detectROIOutID].buffer)[count] = nilObject;
				}
				return;
			}
			
			if (!m_initDone) {
				// Get variances only once for intialization
				ObjectRef initObjRef = getInput(m_initVarianceInID, count);
				ObjectRef noiseObjRef = getInput(m_noiseVarianceInID, count);
				
				if (!noiseObjRef->isNil() && !initObjRef->isNil()) {
					RCPtr<Vector<float> > initRef = initObjRef;
					RCPtr<Vector<float> > noiseRef = noiseObjRef;
					
					Initialize(initRef.get(), noiseRef.get());
				}
				else {
					throw new GeneralException ("MultiIntegralCuesPFTracker::calculate : invalid (nilObject) INIT_VARIANCE or NOISE_VARIANCE input.",__FILE__,__LINE__);
				}
			}
			
			// Try to get edges orientation integral images
			ObjectRef edgesIntObjRef = getInput(m_edgesOriInID, count);
			
			if (!edgesIntObjRef->isNil()) {
				// Set new reference
				m_intEdgExtract->SetEdgesOriSumRef(edgesIntObjRef);
			}
			else {
				m_intEdgExtract->SetEdgesOriSumRef(nilObject);
			}
			
			// Get current image
			RCPtr<Image> imageRef = getInput(m_imageInID, count);
			
			// Verify input image sanity
			if (imageRef->get_width() != m_width ||
				imageRef->get_height() != m_height ||
				imageRef->get_pixelsize() != m_numChannels) {
				throw new GeneralException ("MultiIntegralCuesPFTracker::calculate : image parameters do not correspond to given input.",__FILE__,__LINE__);
			}
			
			memcpy(m_curImage->imageData, imageRef->get_data(), m_numBytesInFrame);
			
			Image *outImage = Image::alloc(m_width, m_height, m_numChannels);
			
			// Start timer
			//ftime(&m_t1);
			
			if (!m_refTarget->IsValid()) {
				// Tracking has not started
				// Try to initialize a new target
				// Get detected ROI
				ObjectRef roiRef = getInput(m_roiInID, count);
				
				if (!roiRef->isNil()) {
					// Initialize target at current ROI
					RCPtr<VisualROI> roiRefPtr = roiRef;
					m_refTarget->SetROI(roiRefPtr.get());
					m_refTarget->SetValidity(true);
					
					// Initialize PF samples
					float *p_state = m_tmpSample->GetState();
					// Set position
					*p_state++ = (float)(roiRefPtr->GetXCen());
					*p_state++ = (float)(roiRefPtr->GetYCen());
					// Add scale state
					*p_state++ = (float)(roiRefPtr->GetHSX());
					*p_state++ = (float)(roiRefPtr->GetHSY());
					m_particleFilter->SetRefMeanSample(m_tmpSample);
					m_particleFilter->InitSamples();
					m_needInitTargetFeat = true;
					m_numInitFrames = 0;
				}
				else {
					(*outputs[m_imageOutID].buffer)[count] = ObjectRef(outImage);
					(*outputs[m_roiOutID].buffer)[count] = ObjectRef(nilObject);
					(*outputs[m_targetOutID].buffer)[count] = ObjectRef(nilObject);
					(*outputs[m_targetProbOutID].buffer)[count] = ObjectRef(Float::alloc(0.0));
					return;
				}
			}
			
			//
			// Preprocess image for features extraction
			//
			m_intClrExtract->Preprocess(m_curImage);
			m_intEdgExtract->Preprocess(m_curImage);
			m_intLBPExtract->Preprocess(m_curImage);
			
			if (m_needInitTargetFeat) {
				// Get features for current ROI and use them
				// as the target feature model
				m_curSampleROIRef = m_refTarget->GetROIRCPtr();
				m_intClrExtract->ExtractFeatures(m_curSampleROIRef.get());
				m_intEdgExtract->ExtractFeatures(m_curSampleROIRef.get());
				m_intLBPExtract->ExtractFeatures(m_curSampleROIRef.get());
				(*m_featVecRef)[0] = m_intClrExtract->GetDescriptor();
				(*m_featVecRef)[1] = m_intEdgExtract->GetDescriptor();
				(*m_featVecRef)[2] = m_intLBPExtract->GetDescriptor();
				m_refTarget->SetDescriptorsVec(m_featVecRef.get());
				//m_refTarget->InitCueWeights();
				m_refTarget->SetCueWeight(m_colorCueWeight, 0);
				m_refTarget->SetCueWeight(m_edgesCueWeight, 1);
				m_refTarget->SetCueWeight(m_LBPCueWeight, 2);
				m_refTarget->InitAges();
				m_needInitTargetFeat = false;
				m_numSkipForRedetect = 0;
			}
			
			//
			// Apply tracking
			//
			m_particleFilter->InitLikelihoodsSum();
			
			for (int p=0; p<m_numSamples; p++) {
				// Get current sample
				m_curSampleRef = m_particleFilter->GetSampleByIdx(p);
				
				//
				// Prediction step
				// Apply prediction to current particle
				//
				m_predModel->Predict(m_curSampleRef.get());
				
				//
				// Measurement step
				// Compute likelihood of the current particle
				//
				ConvertCurSample2ROI();
				m_intClrExtract->ExtractFeatures(m_curSampleROIRef.get());
				m_intEdgExtract->ExtractFeatures(m_curSampleROIRef.get());
				m_intLBPExtract->ExtractFeatures(m_curSampleROIRef.get());
				(*m_featVecRef)[0] = m_intClrExtract->GetDescriptor();
				(*m_featVecRef)[1] = m_intEdgExtract->GetDescriptor();
				(*m_featVecRef)[2] = m_intLBPExtract->GetDescriptor();
				
				float sim = m_refTarget->Similarity(m_featVecRef.get());
				float likelihood = exp(m_lSigma*(sim-1.f));
				
				m_curSampleRef->SetWeight(likelihood);
				m_particleFilter->IncLikelihoodsSum(likelihood);
			}
			
			//
			// Update step
			// Propagate densities and update samples
			//
			m_particleFilter->Update();
			
			// Compute mean state
			m_particleFilter->ComputeMeanState();
			m_curSampleRef = m_particleFilter->GetMeanState();
			
			//
			// Resampling step
			//
			m_particleFilter->Resample();
			
			// Compute likelihood for mean state
			ConvertCurSample2ROI();
			*(m_refTarget->GetROI()) = *(m_curSampleROIRef.get());
			m_intClrExtract->ExtractFeatures(m_curSampleROIRef.get());
			m_intEdgExtract->ExtractFeatures(m_curSampleROIRef.get());
			m_intLBPExtract->ExtractFeatures(m_curSampleROIRef.get());
			(*m_featVecRef)[0] = m_intClrExtract->GetDescriptor();
			(*m_featVecRef)[1] = m_intEdgExtract->GetDescriptor();
			(*m_featVecRef)[2] = m_intLBPExtract->GetDescriptor();
			float sim = m_refTarget->Similarity(m_featVecRef.get());
			//float likelihood = exp(m_lSigma*(sim-1.f));
			
			//cout << "Similarity = " << sim << endl;
			bool targetMatch = (sim >= m_targetMatchThres);
			
			if (targetMatch) {
				m_numSkipForRedetect++;
				if (m_numSkipForRedetect >= m_redetectFreq) {
					// Ask for redetection in current ROI
					ObjectRef roiRef = getInput(m_roiInID, count);
				
					if (!roiRef->isNil()) {
						// Detection found target
						// Fully adapt target to given detection
						RCPtr<VisualROI> roiRefPtr = roiRef;
						m_refTarget->SetROI(roiRefPtr.get());
						
						// Initialize PF samples
						float *p_state = m_tmpSample->GetState();
						// Set position
						*p_state++ = (float)(roiRefPtr->GetXCen());
						*p_state++ = (float)(roiRefPtr->GetYCen());
						// Add scale state
						*p_state++ = (float)(roiRefPtr->GetHSX());
						*p_state++ = (float)(roiRefPtr->GetHSY());
						m_particleFilter->SetRefMeanSample(m_tmpSample);
						m_particleFilter->InitSamples();
						
						// Set features to given ROI
						m_curSampleROIRef = m_refTarget->GetROIRCPtr();
						m_intClrExtract->ExtractFeatures(m_curSampleROIRef.get());
						m_intEdgExtract->ExtractFeatures(m_curSampleROIRef.get());
						m_intLBPExtract->ExtractFeatures(m_curSampleROIRef.get());
						(*m_featVecRef)[0] = m_intClrExtract->GetDescriptor();
						(*m_featVecRef)[1] = m_intEdgExtract->GetDescriptor();
						(*m_featVecRef)[2] = m_intLBPExtract->GetDescriptor();
						m_refTarget->SetDescriptorsVec(m_featVecRef.get());
						
						m_numSkipForRedetect = 0;
					}
				}
			}
			
			m_refTarget->AgeTarget(targetMatch);
			if (sim > m_targetAdaptThres) {
				m_refTarget->Adapt(m_featVecRef.get(), m_targetAdaptRate);
			}
			
			//
			// Produce outputs
			//
			// See if we need to draw ROI in image
			RCPtr<Bool> showROI = getInput(m_showROIInID, count);
			
			if (m_numInitFrames < 10) {
				m_numSkipForRedetect = 0;
				if (*showROI) {
					m_refTarget->GetCstROI()->DrawROI(m_curImage, (const unsigned char *)m_roiColor);
				}
				memcpy(outImage->get_data(), m_curImage->imageData, m_numBytesInFrame);
				
				(*outputs[m_imageOutID].buffer)[count] = ObjectRef(outImage);
				(*outputs[m_roiOutID].buffer)[count] = ObjectRef(nilObject);
				(*outputs[m_targetOutID].buffer)[count] = ObjectRef(nilObject);
				(*outputs[m_targetProbOutID].buffer)[count] = ObjectRef(Float::alloc(0.0));
				m_numInitFrames++;
				return;
			}
			
			// Check if target is still valid
			if (m_refTarget->GetCurrentAge() > -10) {
				if (*showROI) {
					m_refTarget->GetCstROI()->DrawROI(m_curImage, (const unsigned char *)m_roiColor);
				}
				
				memcpy(outImage->get_data(), m_curImage->imageData, m_numBytesInFrame);
				
				(*outputs[m_imageOutID].buffer)[count] = ObjectRef(outImage);
				(*outputs[m_roiOutID].buffer)[count] = m_refTarget->GetROIRCPtr();
				(*outputs[m_targetOutID].buffer)[count] = m_refTarget;
				(*outputs[m_targetProbOutID].buffer)[count] = ObjectRef(Float::alloc(sim));
			}
			else {
				// Target is too old to track
				// Invalidate it
				m_refTarget->SetValidity(false);
				m_numSkipForRedetect = 0;
				memcpy(outImage->get_data(), m_curImage->imageData, m_numBytesInFrame);
				
				(*outputs[m_imageOutID].buffer)[count] = ObjectRef(outImage);
				(*outputs[m_roiOutID].buffer)[count] = ObjectRef(nilObject);
				(*outputs[m_targetOutID].buffer)[count] = ObjectRef(nilObject);
				(*outputs[m_targetProbOutID].buffer)[count] = ObjectRef(Float::alloc(0.0));
			}
			
			// End timer
			//ftime(&m_t2);
			
			// Display time used
			//double timeDiff=(m_t2.time-m_t1.time)+((m_t2.millitm-m_t1.millitm)/1000.0);
			//cout << "Total run time (sec): " << timeDiff << endl;
		}
		catch (BaseException *e) {
			throw e->add(new GeneralException("Exception in MultiIntegralCuesPFTracker::calculate:",__FILE__,__LINE__));
		}
	}

private:
	void Initialize(const Vector<float> *i_initVar, const Vector<float> *i_noiseVar)
	{
		if (i_initVar->size() != m_sampleStateSize) {
			throw new GeneralException ("MultiIntegralCuesPFTracker::Initialize : initial variance vector size differs from sample state size.",__FILE__,__LINE__);
		}
		
		if (i_noiseVar->size() != m_sampleStateSize) {
			throw new GeneralException ("MultiIntegralCuesPFTracker::Initialize : noise variance vector size differs from sample state size.",__FILE__,__LINE__);
		}
		
		m_particleFilter = new PFGenericParticleFilter(m_numSamples, 
			m_sampleStateSize, i_initVar) ;
		m_predModel = new PFPMRandomWalk(m_sampleStateSize, i_noiseVar);
		
		m_initDone = true;
	}
	
	void ConvertCurSample2ROI()
	{
		// Set ROI according to particle state
		const float *p_state = m_curSampleRef->GetCstState();
		
		// Set position
		m_curSampleROIRef->SetXCen((int)(*p_state++));
		m_curSampleROIRef->SetYCen((int)(*p_state++));
		// Set scale
		m_curSampleROIRef->SetHSX((int)(*p_state++));
		m_curSampleROIRef->SetHSY((int)(*p_state++));
	}
	
private:
	// Input IDs 
	int m_initVarianceInID;
	int m_noiseVarianceInID;
	int m_imageInID;
	int m_roiInID;
	int m_edgesOriInID;
	int m_showROIInID;
	int m_baseNameInID;
	int m_activatedInID;
	
	// Output IDs 
	int m_imageOutID;
	int m_roiOutID;
	int m_targetOutID;
	int m_targetProbOutID;
	int m_detectROIOutID;
	int m_frameNameOutID;
	
	RCPtr<Bool> m_activated;
	
	int m_width;
	int m_height;
	int m_numChannels;
	int m_numPixels;
	int m_numBytesInFrame;
	unsigned char m_roiColor[3];
	
	struct timeb m_t1, m_t2;
	
	double m_targetMatchThres;
	double m_targetAdaptThres;
	double m_targetAdaptRate;
	double m_cueAdaptRate;
	float m_lSigma;
	
	int m_numHoriIntRect;
	int m_numVertIntRect;
	bool m_useRectDiff;
	bool m_useBoundary;
	double m_boundaryMeanDiffThresh;
	int m_numOriBins;
	double m_edgesStrTresh;
	double m_maxStrengthValue;
	
	double m_colorCueWeight;
	double m_edgesCueWeight;
	double m_LBPCueWeight;
	
	int m_numInitFrames;
	int m_numSkipForRedetect;
	int m_redetectFreq;
	
	int m_numSamples;
	int m_sampleStateSize;
	
	bool m_initDone;
	bool m_needInitTargetFeat;
	
	PFGenericParticle *m_tmpSample;
	
	RCPtr<VisualTarget<double> > m_refTarget;
	RCPtr<Vector<VisualFeatureDesc<double> *> > m_featVecRef;
	RCPtr<PFGenericParticle> m_curSampleRef;
	RCPtr<VisualROI> m_curSampleROIRef;
	
	// Particle filter related members
	PFGenericParticleFilter *m_particleFilter;
	PFPMRandomWalk *m_predModel;
	
	// Features extraction related members
	IntegralColorExtraction *m_intClrExtract;
	IntegralEdgesOriExtraction *m_intEdgExtract;
	IntegralLBPExtraction *m_intLBPExtract;
	
	// Temporary image copy
	IplImage *m_curImage;
};

}//namespace RobotFlow
