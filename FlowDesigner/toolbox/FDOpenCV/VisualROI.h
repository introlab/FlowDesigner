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

#ifndef _VISUALROI_H_
#define _VISUALROI_H_

#include "Object.h"
#include <iostream>
#include <math.h>
#include "cv.h"

namespace RobotFlow {

typedef enum
{
	e_VISUALROI_rectangular = 0,
	e_VISUALROI_elliptical,
	e_VISUALROI_unknown
} e_VISUALROI_type;

class VisualROI : public FD::Object
{
public:
	VisualROI();
	
	VisualROI(e_VISUALROI_type i_type, int i_xCen, int i_yCen,
		int i_hsX, int i_hsY, int i_angle);
	
	VisualROI(const VisualROI& i_ref);

	virtual ~VisualROI();
	
	VisualROI & operator =(const VisualROI &i_ref);

	// Default routine to print a VisualROI object to an output stream
	void printOn(std::ostream &out) const;

	// Default routine to read a VisualROI object from an input stream
	void readFrom(std::istream &in);
	
	void DrawROI(IplImage *io_frame, const unsigned char *i_color) const;
	
	void DrawROI(int i_width, int i_height, int i_numChannels, 
		unsigned char *io_pixels, const unsigned char *i_color) const;

	void Reset(int i_hsX, int i_hsY, int i_angle);

	e_VISUALROI_type GetType() const;

	int GetPerimLength() const;

	short *GetPerim();
	
	const short *GetCstPerim() const;

	float *GetNormVects();
	
	const float *GetCstNormVects() const;
	
	int GetXCen() const;
	
	int GetYCen() const;

	int GetHSX() const;

	int GetHSY() const;
	
	int GetAngle() const;

	int GetArea() const;

	unsigned char *GetMask();
	
	const unsigned char *GetCstMask() const;
	
	void SetType(e_VISUALROI_type i_type);
	
	void SetXCen(int i_xCen);
	
	void SetYCen(int i_yCen);

	void SetHSX(int i_hsX);

	void SetHSY(int i_hsY);
	
	void SetAngle(int i_angle);

private:
	void DrawRectangularRegion(IplImage *io_frame, const unsigned char *i_color) const;
	
	void DrawEllipticalRegion(IplImage *io_frame, const unsigned char *i_color) const;
	
	void DrawRectangularRegion(int i_width, int i_height, int i_numChannels, 
		unsigned char *io_pixels, const unsigned char *i_color) const;
	
	void DrawEllipticalRegion(int i_width, int i_height, int i_numChannels, 
		unsigned char *io_pixels, const unsigned char *i_color) const;
	
	void MakeRectangularRegion();
	
	void MakeEllipticalRegion();

	int MakeRegionMask();

private:
	// Region geometric type
	e_VISUALROI_type m_type;
	// Number of pixels along perimeter
	int m_perimLength;
	// Perimeter pixels
	short *m_perim;
	// Contour normal vectors
	float *m_normVects;
	// X value for the region center position
	int m_xCen;
	// Y value for the region center position
	int m_yCen;
	// Halfsize for x axis
	int m_hsX;
	// Halfsize for y axis
	int m_hsY;
	// Orientation of the region in degrees
	int m_angle;
	// Number of pixels in mask
	int m_area;
	// (2*m_hsX+1) X (2*m_hsY+1) region
	unsigned char *m_mask;
};

}

#endif
