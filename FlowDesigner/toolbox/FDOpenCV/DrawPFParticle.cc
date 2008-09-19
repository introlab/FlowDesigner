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

#ifndef _DRAWPFPARTICLE_CC_
#define _DRAWPFPARTICLE_CC_

#include "BufferedNode.h"
#include "VisualROI.h"
#include "Image.h"

using namespace std;
using namespace FD;

namespace RobotFlow {

class DrawPFParticle;
DECLARE_NODE(DrawPFParticle)

  /*Node
   *
   * @name DrawPFParticle
   * @category RobotFlow:Vision:Tracking
   * @description Draws particle state as a visual ROI in current image.
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
   * @input_name IN_IMAGE
   * @input_type Image
   * @input_description Current image to use.
   *
   * @input_name IN_ROI
   * @input_type VisualROI
   * @input_description Current particle to draw.
   *
   * @output_name OUT_IMAGE
   * @output_type Image
   * @output_description Resulting image with particles drawn.
   *
   * @output_name OUT_ROI
   * @output_type VisualROI
   * @output_description Current ROI.
   *
   * @output_name PREPROCESS_COMPLETED
   * @output_type int
   * @output_description Output to force preprocessing.
   *
   END*/

class DrawPFParticle : public BufferedNode
{
public:
	DrawPFParticle(string nodeName, ParameterSet params)
	: BufferedNode(nodeName, params)
	{
		m_imageInID = addInput("IN_IMAGE");
		m_roiInID = addInput("IN_ROI");
		
		m_imageOutID = addOutput("OUT_IMAGE");
		m_roiOutID = addOutput("OUT_ROI");
		m_ppCompletedOutID = addOutput("PREPROCESS_COMPLETED");
		
		m_width = dereference_cast<int>(parameters.get("FRAME_WIDTH"));
		m_height = dereference_cast<int>(parameters.get("FRAME_HEIGHT"));
		m_numChannels = dereference_cast<int>(parameters.get("NUM_CHANNELS"));
		
		m_numPixels = m_width*m_height;
		m_numBytesInFrame = m_numPixels*m_numChannels;
		
		m_curImage = RCPtr<Image>(Image::alloc(m_width, m_height, m_numChannels));
		
		m_color = new unsigned char[m_numChannels];
		for (int c=0; c<m_numChannels; c++) {
			m_color[c] = 128;
		}
	}
	
	~DrawPFParticle()
	{
		delete [] m_color;
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
		
		if (output_id == m_imageOutID) {
			return;
		}
		else if (output_id == m_roiOutID) {
			ParameterSet myReq;
			myReq.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_roiInID].lookAhead+outputLookAhead)));
			myReq.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_roiInID].lookBack+outputLookBack)));
			inputs[m_roiInID].node->request(inputs[m_roiInID].outputID,myReq);
		}
		else if (output_id == m_ppCompletedOutID) {
			ParameterSet myReq;
			myReq.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_imageInID].lookAhead+outputLookAhead)));
			myReq.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_imageInID].lookBack+outputLookBack)));
			inputs[m_imageInID].node->request(inputs[m_imageInID].outputID, myReq);
		}
		else {
			throw new GeneralException ("DrawPFParticle::request : unknown output ID.",__FILE__,__LINE__);
		}
	
	}
	
	void calculate(int output_id, int count, Buffer &out)
	{
		try {
			if (output_id == m_roiOutID) {
				// Get current particle to convert
				ObjectRef roiObjRef = getInput(m_roiInID, count);
				
				if (!roiObjRef->isNil()) {
					RCPtr<VisualROI> roiRef = RCPtr<VisualROI>(roiObjRef);
					
					// Draw particle
					roiRef->DrawROI(m_width, m_height, m_numChannels, m_curImage->get_data(), m_color);
				}
				
				// Output the input ROI
				(*outputs[m_roiOutID].buffer)[count] = roiObjRef;
			}
			else if (output_id == m_ppCompletedOutID) {
				// Get a new copy of current frame
				RCPtr<Image> imageRef = getInput(m_imageInID, count);
		
				// Verify input image sanity
				if (imageRef->get_width() != m_width ||
					imageRef->get_height() != m_height ||
					imageRef->get_pixelsize() != m_numChannels) {
					throw new GeneralException ("DrawPFParticle::calculate : image parameters do not correspond to given input.",__FILE__,__LINE__);
				}
				
				// Copy input image
				memcpy(m_curImage->get_data(), imageRef->get_data(), m_numBytesInFrame);
				
				(*outputs[m_ppCompletedOutID].buffer)[count] = ObjectRef(Int::alloc(1));
			}
			else if (output_id == m_imageOutID) {
				// Allocate output image
				Image *imageOut = Image::alloc(m_width, m_height, m_numChannels);
			
				// Copy image
				memcpy(imageOut->get_data(), m_curImage->get_data(), m_numBytesInFrame);
				(*outputs[m_imageOutID].buffer)[count] = ObjectRef(imageOut);
			}
		}
		catch (BaseException *e) {
			throw e->add(new GeneralException("Exception in DrawPFParticle::calculate:",__FILE__,__LINE__));
		}
	}

private:
	// Input IDs
	int m_imageInID;
	int m_roiInID;
	
	// Output IDs
	int m_imageOutID;
	int m_ppCompletedOutID;
	int m_roiOutID;
	
	// Parameters
	int m_width;
	int m_height;
	int m_numChannels;
	int m_numPixels;
	int m_numBytesInFrame;
	
	unsigned char *m_color;
	
	RCPtr<Image> m_curImage;
};

}

#endif
