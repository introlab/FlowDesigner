#include "CvStructuringElement.h"
#include <iostream>

namespace FD {
   
	using namespace std;
   
	DECLARE_TYPE(CvStructuringElement);
   
	CvStructuringElement::CvStructuringElement()
   : m_structuringElement(NULL)
	{
	}
   
   CvStructuringElement::CvStructuringElement(int cols, int rows, int anchor_x, int anchor_y, int shape)
   : m_structuringElement(NULL)
	{      
      int status = cvGetErrMode();
      cvSetErrMode( CV_ErrModeSilent );
      __BEGIN__; 
      OPENCV_CALL(m_structuringElement = cvCreateStructuringElementEx( cols, rows, anchor_x, anchor_y, shape, NULL));         
      __END__;
      cvSetErrMode( status );
      if( cvGetErrStatus() != CV_StsOk  )
      {
         throw new GeneralException("OPENCV - Error to create the structuring element: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
      } 
	}
   
	CvStructuringElement::~CvStructuringElement()
	{
      cvReleaseStructuringElement( &m_structuringElement );
	}
   
	/**Serialize (binary) the object to a stream*/
	void CvStructuringElement::serialize(std::ostream &out) const
	{
      
	}
   
	/**Unserialize (binary) the object from a stream*/
	void CvStructuringElement::unserialize(std::istream &in)
	{
      
	}
   
	/**Generic print function*/
	void CvStructuringElement::printOn(std::ostream &out) const
	{
		out << "CvStructuringElement ";
		out << m_structuringElement;		
	}
   
	/**Generic read function*/
	void CvStructuringElement::readFrom(std::istream &in)
	{
      
	}
   
   IplConvKernel* CvStructuringElement::getStructuringElement()
   {
      return m_structuringElement;
   }
   
}//namespace FD
