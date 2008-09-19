#include "VisualROI.h"
#include "vmethod.h"
#include "Vector.h"

using namespace FD;
using namespace std;

namespace RobotFlow {

DECLARE_TYPE(VisualROI)

VisualROI::VisualROI()
: m_type(), m_xCen(0), m_yCen(0), 
m_hsX(0), m_hsY(0), m_angle(0), 
m_perim(NULL), m_normVects(NULL), m_mask(NULL)
{

}

VisualROI::VisualROI(e_VISUALROI_type i_type, int i_xCen, int i_yCen,
	int i_hsX, int i_hsY, int i_angle)
:m_type(i_type), m_xCen(i_xCen), m_yCen(i_yCen),
m_hsX(i_hsX), m_hsY(i_hsY), m_angle(i_angle),
m_perim(NULL), m_normVects(NULL), m_mask(NULL)
{
	switch(m_type) {
	case e_VISUALROI_rectangular:
		MakeRectangularRegion();
		break;
	case e_VISUALROI_elliptical:
		MakeEllipticalRegion();
		break;
	case e_VISUALROI_unknown:
	default:
		throw new GeneralException ("VisualROI::VisualROI : unknown region geometric type",__FILE__,__LINE__);
	}
}

VisualROI::VisualROI(const VisualROI& i_ref)
:m_perim(NULL), m_normVects(NULL), m_mask(NULL)
{
	try {
		m_type = i_ref.m_type;
		m_xCen = i_ref.m_xCen;
		m_yCen = i_ref.m_yCen;
		m_hsX = i_ref.m_hsX;
		m_hsY = i_ref.m_hsY;
		m_angle = i_ref.m_angle;
		
		Reset(m_hsX, m_hsY, m_angle);
	}
	catch (BaseException *e) {
		throw e->add(new GeneralException("Exception caught in VisualROI::VisualROI:",__FILE__,__LINE__));
	}
}

VisualROI::~VisualROI()
{
	delete [] m_perim;
	delete [] m_normVects;
	delete [] m_mask;
}

VisualROI & VisualROI::operator =(const VisualROI &i_ref)
{
	// Avoid self assignment
	if (&i_ref == this) {
		return *this;
	}
	
	try {
		m_type = i_ref.m_type;
		m_xCen = i_ref.m_xCen;
		m_yCen = i_ref.m_yCen;
		
		if (m_hsX == i_ref.m_hsX &&
			m_hsY == i_ref.m_hsY &&
			m_angle == i_ref.m_angle) {
			// Nothing else to do
			return *this;	
		}
		
		m_hsX = i_ref.m_hsX;
		m_hsY = i_ref.m_hsY;
		m_angle = i_ref.m_angle;
		
		Reset(m_hsX, m_hsY, m_angle);
		
		return *this;
	}
	catch (BaseException *e) {
		throw e->add(new GeneralException("Exception caught in VisualROI::VisualROI:",__FILE__,__LINE__));
	}
}

// Default routine to print a VisualROI object to an output stream
void VisualROI::printOn(ostream &out) const
{
	//throw new GeneralException ("VisualROI::printOn : routine not implemented yet",__FILE__,__LINE__);
	out << "VisualROI: centerX=" << m_xCen << " centerY=" << m_yCen << " half width=" << m_hsX << " half height=" << m_hsY << endl;
}

// Default routine to read a VisualROI object from an input stream
void VisualROI::readFrom(istream &in)
{
	throw new GeneralException ("VisualROI::readFrom : routine not implemented yet",__FILE__,__LINE__);
}

void VisualROI::DrawROI(IplImage *io_frame, const unsigned char *i_color) const
{
	try {
		switch(m_type) {
		case e_VISUALROI_rectangular:
			DrawRectangularRegion(io_frame, i_color);
			break;
		case e_VISUALROI_elliptical:
			DrawEllipticalRegion(io_frame, i_color);
			break;
		case e_VISUALROI_unknown:
		default:
			throw new GeneralException ("VisualROI::DrawROI : unknown region geometric type",__FILE__,__LINE__);
		}
	}
	catch (BaseException *e) {
		throw e->add(new GeneralException("Exception caught in VisualROI::VisualROI:",__FILE__,__LINE__));
	}
}

void VisualROI::DrawROI(int i_width, int i_height, int i_numChannels, 
	unsigned char *io_pixels, const unsigned char *i_color) const
{
	try {
		switch(m_type) {
		case e_VISUALROI_rectangular:
			DrawRectangularRegion(i_width, i_height, i_numChannels, io_pixels, i_color);
			break;
		case e_VISUALROI_elliptical:
			DrawEllipticalRegion(i_width, i_height, i_numChannels, io_pixels, i_color);
			break;
		case e_VISUALROI_unknown:
		default:
			throw new GeneralException ("VisualROI::DrawROI : unknown region geometric type",__FILE__,__LINE__);
		}
	}
	catch (BaseException *e) {
		throw e->add(new GeneralException("Exception caught in VisualROI::VisualROI:",__FILE__,__LINE__));
	}
}

void VisualROI::Reset(int i_hsX, int i_hsY, int i_angle)
{
	m_hsX = i_hsX;
	m_hsY = i_hsY;
	m_angle = i_angle;

	switch(m_type) {
	case e_VISUALROI_rectangular:
		MakeRectangularRegion();
		break;
	case e_VISUALROI_elliptical:
		MakeEllipticalRegion();
		break;
	case e_VISUALROI_unknown:
	default:
		throw new GeneralException ("VisualROI::VisualROI : unknown region geometric type",__FILE__,__LINE__);
	}
}

//
// Public accessor methods
//
e_VISUALROI_type VisualROI::GetType() const
{
	return m_type;
}

int VisualROI::GetPerimLength() const
{
	return m_perimLength;
}

short *VisualROI::GetPerim()
{
	return m_perim;
}

const short *VisualROI::GetCstPerim() const
{
	return (const short *)m_perim;
}

float *VisualROI::GetNormVects()
{
	return m_normVects;
}

const float *VisualROI::GetCstNormVects() const
{
	return (const float *)m_normVects;
}

int VisualROI::GetXCen() const
{
	return m_xCen;
}

int VisualROI::GetYCen() const
{
	return m_yCen;
}

int VisualROI::GetHSX() const
{
	return m_hsX;
}

int VisualROI::GetHSY() const
{
	return m_hsY;
}

int VisualROI::GetAngle() const
{
	return m_angle;
}

int VisualROI::GetArea() const
{
	return m_area;
}

unsigned char *VisualROI::GetMask()
{
	return m_mask;
}

const unsigned char *VisualROI::GetCstMask() const
{
	return (const unsigned char *)m_mask;
}

void VisualROI::SetType(e_VISUALROI_type i_type)
{
	m_type = i_type;
}

void VisualROI::SetXCen(int i_xCen)
{
	m_xCen = i_xCen;
}
	
void VisualROI::SetYCen(int i_yCen)
{
	m_yCen = i_yCen;
}
	
void VisualROI::SetHSX(int i_hsX)
{
	if (i_hsX == m_hsX) {
		// Nothing to do
		return;
	}
	
	try {
		m_hsX = i_hsX;
		Reset(m_hsX, m_hsY, m_angle);
	}
	catch (BaseException *e) {
		throw e->add(new GeneralException("Exception caught in VisualROI::SetHSX:",__FILE__,__LINE__));
	}
}

void VisualROI::SetHSY(int i_hsY)
{
	if (i_hsY == m_hsY) {
		// Nothing to do
		return;
	}
	
	try {
		m_hsY = i_hsY;
		Reset(m_hsX, m_hsY, m_angle);
	}
	catch (BaseException *e) {
		throw e->add(new GeneralException("Exception caught in VisualROI::SetHSY:",__FILE__,__LINE__));
	}
}

void VisualROI::SetAngle(int i_angle)
{
	if (i_angle == m_angle) {
		// Nothing to do
		return;
	}
	
	try {
		m_angle = i_angle;
		Reset(m_hsX, m_hsY, m_angle);
	}
	catch (BaseException *e) {
		throw e->add(new GeneralException("Exception caught in VisualROI::SetAngle:",__FILE__,__LINE__));
	}
}

//
// Private routines
//
void VisualROI::DrawRectangularRegion(IplImage *io_frame, const unsigned char *i_color) const
{
	CvPoint ulc, lrc;
	CvScalar color;
	
	if (io_frame->nChannels == 3) {
		color = CV_RGB(i_color[0], i_color[1], i_color[2]);
	}
	else if (io_frame->nChannels == 1) {
		color = cvRealScalar((double)(i_color[0]));
	}
	else {
		throw new GeneralException ("VisualROI::DrawRectangularRegion : can only draw region with an image with 1 or 3 channel(s).",__FILE__,__LINE__);
	}

	ulc.x = m_xCen - m_hsX;
	ulc.y = m_yCen - m_hsY;
	lrc.x = m_xCen + m_hsX;
	lrc.y = m_yCen + m_hsY;
	cvRectangle(io_frame, ulc, lrc, color, 1); 
}

void VisualROI::DrawEllipticalRegion(IplImage *io_frame, const unsigned char *i_color) const
{
	if (!io_frame) {
		throw new GeneralException ("VisualROI::DrawROI : invalid image reference.",__FILE__,__LINE__);
	}
	
	if (!i_color) {
		throw new GeneralException ("VisualROI::DrawROI : invalid color reference.",__FILE__,__LINE__);
	}
	
	if (!m_perim) {
		throw new GeneralException ("VisualROI::DrawROI : cannot draw ROI with uninitialized region data.",__FILE__,__LINE__);
	}
	
	unsigned char *p_pixels = (unsigned char *)io_frame->imageData;
	const short *p_perim = this->GetCstPerim();
	int imgWidth = io_frame->width;
	int imgHeight = io_frame->height;
	int numChannels = io_frame->nChannels;
	int i, c, x, y;
	short deltaX, deltaY;
	bool broken = true;

	// Start at the top center of the region
	x = m_xCen;
	y = m_yCen;
	p_pixels += numChannels*(y*imgWidth + x);

	// Overlay region of interest
	for (i=m_perimLength; i>0; i--)  {
		deltaX = *p_perim++;
		deltaY = *p_perim++;
		x += deltaX;
		y += deltaY;

		// Draw only if region is visible
		if (y>0 && y<imgHeight && x>0 && x<imgWidth) {
			if (!broken) {
				// Relative position
				p_pixels += numChannels*(deltaY*imgWidth + deltaX);
			}
			else {
				// Absolute position
				p_pixels = (unsigned char *)(io_frame->imageData + numChannels*(y*imgWidth + x));
				broken = false;
			}

			for (c=0; c<numChannels; c++) {
				*p_pixels++ = i_color[c];
			}

			p_pixels -= numChannels;
		}
		else {
			broken = true;
		}
	}
}

void VisualROI::DrawRectangularRegion(int i_width, int i_height, int i_numChannels, 
	unsigned char *io_pixels, const unsigned char *i_color) const
{
	throw new GeneralException ("VisualROI::DrawRectangularRegion : method not yet implemented.",__FILE__,__LINE__);
}

void VisualROI::DrawEllipticalRegion(int i_width, int i_height, int i_numChannels, 
	unsigned char *io_pixels, const unsigned char *i_color) const
{
	if (!io_pixels) {
		throw new GeneralException ("VisualROI::DrawROI : invalid pixels reference.",__FILE__,__LINE__);
	}
	
	if (!i_color) {
		throw new GeneralException ("VisualROI::DrawROI : invalid color reference.",__FILE__,__LINE__);
	}
	
	if (!m_perim) {
		throw new GeneralException ("VisualROI::DrawROI : cannot draw ROI with uninitialized region data.",__FILE__,__LINE__);
	}
	
	unsigned char *p_pixels = io_pixels;
	const short *p_perim = this->GetCstPerim();
	int imgWidth = i_width;
	int imgHeight = i_height;
	int numChannels = i_numChannels;
	int i, c, x, y;
	short deltaX, deltaY;
	bool broken = true;

	// Start at the top center of the region
	x = m_xCen;
	y = m_yCen;
	p_pixels += numChannels*(y*imgWidth + x);

	// Overlay region of interest
	for (i=m_perimLength; i>0; i--)  {
		deltaX = *p_perim++;
		deltaY = *p_perim++;
		x += deltaX;
		y += deltaY;

		// Draw only if region is visible
		if (y>0 && y<imgHeight && x>0 && x<imgWidth) {
			if (!broken) {
				// Relative position
				p_pixels += numChannels*(deltaY*imgWidth + deltaX);
			}
			else {
				// Absolute position
				p_pixels = (unsigned char *)(io_pixels + numChannels*(y*imgWidth + x));
				broken = false;
			}

			for (c=0; c<numChannels; c++) {
				*p_pixels++ = i_color[c];
			}

			p_pixels -= numChannels;
		}
		else {
			broken = true;
		}
	}
}

void VisualROI::MakeEllipticalRegion()
{
	short *ptr;
	float *ptrn;
	int xrad2 = m_hsX * m_hsX;
	int yrad2 = m_hsY * m_hsY;
	double ratio = ((double) xrad2) / ((double) yrad2);
	int hsXbkpt = (int) (xrad2 / sqrt((double) xrad2 + yrad2));
	int hsYbkpt = (int) (yrad2 / sqrt((double) xrad2 + yrad2));
	int x, y, xOff, yOff, xAbs, yAbs;
	int curX, curY;
	
	delete [] m_perim;
	delete [] m_normVects;
	delete [] m_mask;

	// Number of iterations in loops below
	m_perimLength = 4 * hsXbkpt + 4 * hsYbkpt + 4;
	m_perim = new short[2*m_perimLength];
	m_normVects = new float[2*m_perimLength];
	ptr = m_perim;
	ptrn = m_normVects;

	// Perimeter starts at top and proceeds clockwise
	*(ptr)++ = curX = 0;
	*(ptr)++ = curY = - m_hsY;
	*ptrn++ = 0.0f;
	*ptrn++ = -1.0f;

	// From 0 to 45 degrees, starting from top clockwise of an unoriented ellipse
	for (x = 1; x <= hsXbkpt ; x++)  {
		y = (int) (-m_hsY * sqrt(1 - ((double) x / m_hsX)*((double) x / m_hsX)));
		*(ptr)++ = x - curX;
		*(ptr)++ = y - curY;
		*ptrn++ = (float) (       x  / sqrt(x*x + ratio*y*y));
		*ptrn++ = (float) ((ratio*y) / sqrt(x*x + ratio*y*y));
		curX = x;
		curY = y;
	}
	
	// From 45 to 135 degrees (including right axis)
	for (y = - hsYbkpt ; y <= hsYbkpt ; y++)  {
		x = (int) (m_hsX * sqrt(1 - ((double) y / m_hsY)*((double) y / m_hsY)));
		*(ptr)++ = x - curX;
		*(ptr)++ = y - curY;
		*ptrn++ = (float) (       x  / sqrt(x*x + ratio*y*y));
		*ptrn++ = (float) ((ratio*y) / sqrt(x*x + ratio*y*y));
		curX = x;
		curY = y;
	}
	// From 135 to 225 degrees (including down axis)
	for (x = hsXbkpt  ; x >= - hsXbkpt ; x--)  {
		y = (int) (m_hsY * sqrt(1 - ((double) x / m_hsX)*((double) x / m_hsX)));
		*(ptr)++ = x - curX;
		*(ptr)++ = y - curY;
		*ptrn++ = (float) (       x  / sqrt(x*x + ratio*y*y));
		*ptrn++ = (float) ((ratio*y) / sqrt(x*x + ratio*y*y));
		curX = x;
		curY = y;
	}
	// From 225 to 315 degrees (including left axis)
	for (y = hsYbkpt ; y >= - hsYbkpt ; y--)  {
		x = (int) (-m_hsX * sqrt(1 - ((double) y / m_hsY)*((double) y / m_hsY)));
		*(ptr)++ = x - curX;
		*(ptr)++ = y - curY;
		*ptrn++ = (float) (       x  / sqrt(x*x + ratio*y*y));
		*ptrn++ = (float) ((ratio*y) / sqrt(x*x + ratio*y*y));
		curX = x;
		curY = y;
	}
	// From 315 to 360 degrees
	for (x = - hsXbkpt ; x < 0 ; x++)  {
		y = (int) (-m_hsY * sqrt(1 - ((double) x / m_hsX)*((double) x / m_hsX)));
		*(ptr)++ = x - curX;
		*(ptr)++ = y - curY;
		*ptrn++ = (float) (       x  / sqrt(x*x + ratio*y*y));
		*ptrn++ = (float) ((ratio*y) / sqrt(x*x + ratio*y*y));
		curX = x;
		curY = y;
	}

	m_area = MakeRegionMask();
}

void VisualROI::MakeRectangularRegion()
{
	if (m_angle != 0) { 
		throw new GeneralException ("VisualROI::MakeRectangularRegion : method not yet implemented for angle different from 0.",__FILE__,__LINE__);
	}
}

int VisualROI::MakeRegionMask()
{
	unsigned char *p_mask;
	short *p_perim;
	int *mins, *maxs;
	int *p_min, *p_max;
	int x, y;
	int i, j;
	int sizeX = 2*(m_hsX)+1;
	int sizeY = 2*(m_hsY)+1;
	int numPixels = 0;

	mins = new int[sizeY];
	maxs = new int[sizeY];
	m_mask = new unsigned char[sizeX*sizeY];
	
	// Init mins and maxs
	p_min = mins;
	p_max = maxs;
	for (i = 0 ; i < sizeY ; i++)  {
		*p_min++ =  999;
		*p_max++ = -999;
	}

	// Init mask
	memset(m_mask, 0, sizeX*sizeY);

	// Fill in max and min vectors
	p_perim = m_perim;
	x = m_hsX + *p_perim++;
	y = m_hsY + *p_perim++;
	p_max = maxs + y;
	p_min = mins + y;
	*p_max = x;
	*p_min = x;
	for (i=1; i<m_perimLength; ++i)  {
		x += *p_perim++;
		p_max += *p_perim;
		p_min += *p_perim++;
		if (x > *p_max) {
			*p_max = x;
		}
		if (x < *p_min) {
			*p_min = x;
		}
	}
	
	// Use max and min vectors to fill in mask.
	// The interior of the object is filled in
	// and the perimeter is excluded.
	p_min = mins;
	p_max = maxs;
	for (i = 0 ; i < sizeY ; i++)  {
		p_mask = (m_mask) + i * sizeX;
		if (*p_min < *p_max)  {
			p_mask += *p_min + 1;
			for (j = *p_min + 1 ; j < *p_max ; j++) {
				*p_mask++ = 255;
				numPixels++;
			}
		}
		p_min++;
		p_max++;
	}

	// Clean memory
	delete [] mins;
	delete [] maxs;

	return numPixels;
}

ObjectRef VisualROIToVect(ObjectRef VisualROIValue) {
  try {
    RCPtr<VisualROI> ROI = VisualROIValue;
    Vector<float> *vect = Vector<float>::alloc(6);
    (*vect)[0] = ROI->GetXCen();
    (*vect)[1] = ROI->GetYCen();
    (*vect)[2] = ROI->GetHSX();
    (*vect)[3] = ROI->GetHSY();
    (*vect)[4] = ROI->GetAngle();
    (*vect)[5] = ROI->GetArea();
    return ObjectRef(vect);
  }
  catch(BaseException *e) {
    throw e->add(new GeneralException("Unable to convert VisualROI into Vector",__FILE__,__LINE__));
  }
}
REGISTER_VTABLE0(toVect, VisualROI, VisualROIToVect, 1);


}//namespace RobotFlow
