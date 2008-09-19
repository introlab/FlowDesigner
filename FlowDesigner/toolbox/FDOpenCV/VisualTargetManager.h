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
#include "VisualTarget.h"
#include <stdlib.h>
#include <sys/timeb.h>

namespace RobotFlow {

class VisualTargetManager : public FD::BufferedNode
{
	friend class FD::BufferedNode;
public:
	VisualTargetManager();
	
	VisualTargetManager(std::string nodeName, FD::ParameterSet params);

	virtual ~VisualTargetManager();

	// Default routine to print a VisualTargetManager object to an output stream
	void printOn(std::ostream &out) const
	{
		throw new FD::GeneralException("Exception in VisualTargetManager::printOn: method not yet implemented.",__FILE__,__LINE__);
	}

	// Default routine to read a VisualTargetManager object from an input stream
	void readFrom(std::istream &in)
	{
		throw new FD::GeneralException("Exception in VisualTargetManager::printOn: method not yet implemented.",__FILE__,__LINE__);
	}
	
	virtual void request(int output_id, const FD::ParameterSet &req);
	
	void calculate(int output_id, int count, FD::Buffer &out);

private:
	// Input IDs (for BufferedNode)
	int m_imageInID;
	int m_roiInID;
	int m_featVecInID;
	int m_targetInID;
	int m_ppCompletedInID;
	
	// Output IDs (for BufferedNode)
	int m_imageOutID;
	int m_roiOutID;
	int m_targetOutID;
	int m_targetProbOutID;
	int m_targetDXOutID;
	int m_targetDYOutID;
	int m_nameOutID;
	
	int m_width;
	int m_height;
	int m_numChannels;
	int m_numPixels;
	int m_numBytesInFrame;
	float m_imgXCen;
	float m_imgYCen;
	unsigned char m_roiColor[3];
	float m_lSigma;
	
	struct timeb m_t1, m_t2;
	
	int m_maxNumTargets;
	double m_targetMatchThres;
	double m_targetAdaptThres;
	double m_targetAdaptRate;
	double m_cueAdaptRate;
	bool m_ppCompleted;
	
	FD::RCPtr<VisualTarget<double> > m_refTarget;
	//RCPtr<VisualROI> m_refROI;
	
	// Temporary image copy
	IplImage *m_curImage;
};

}

#endif
