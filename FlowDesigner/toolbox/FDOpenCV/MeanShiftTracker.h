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

#ifndef _MEANSHIFTTRACKER_H_
#define _MEANSHIFTTRACKER_H_

#include "VisualTracker.h"

namespace RobotFlow {

class MeanShiftTracker : public VisualTracker
{
public:
	MeanShiftTracker();
	
	MeanShiftTracker(int i_maxNumMSIter, double i_minMSDistEpsilon);
	
	MeanShiftTracker(std::string nodeName, FD::ParameterSet params);

	virtual ~MeanShiftTracker();

	// Default routine to print a MeanShiftTracker object to an output stream
	void printOn(std::ostream &out) const
	{
		throw new FD::GeneralException("Exception in MeanShiftTracker::printOn: method not yet implemented.",__FILE__,__LINE__);
	}

	// Default routine to read a MeanShiftTracker object from an input stream
	void readFrom(std::istream &in)
	{
		throw new FD::GeneralException("Exception in MeanShiftTracker::printOn: method not yet implemented.",__FILE__,__LINE__);
	}
	
	virtual void request(int output_id, const FD::ParameterSet &req);
	
	void calculate(int output_id, int count, FD::Buffer &out);

private:
	// Input IDs (for BufferedNode)
	int m_imageInID;
	int m_targetInID;
	int m_msLocVecInID;
	int m_ppCompletedInID;
	
	// Output IDs (for BufferedNode)
	int m_finishedOutID;
	int m_imageOutID;
	int m_curTargetOutID;
	int m_targetOutID;
	
	int m_width;
	int m_height;
	int m_numChannels;
	int m_numPixels;
	int m_numBytesInFrame;
	
	int m_numIter;
	int m_maxNumMSIter;
	double m_minMSDistEpsilon;
	
	bool m_finished;
	bool m_initMS;
	
	VisualTarget<double> *m_curTarget;
	
	// Temporary image copy
	IplImage *m_curImage;
};

}

#endif
