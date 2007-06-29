#include "CvColor.h"
#include <iostream>

namespace FD {
   
	using namespace std;
   
	DECLARE_TYPE(CvColor);
   
	CvColor::CvColor()
	{
      cvSetErrMode( CV_ErrModeSilent );
      __BEGIN__; 
      OPENCV_CALL(m_Color = CV_RGB(0,0,0));         
      __END__;
      cvSetErrMode( CV_ErrModeLeaf );
      if( cvGetErrStatus() != CV_StsOk  )
      {
         throw new GeneralException("OPENCV - Error to construct the RGB color: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
      }       
	}
   
   CvColor::CvColor(int channel1, int channel2, int channel3)
	{
      cvSetErrMode( CV_ErrModeSilent );
      __BEGIN__; 
      OPENCV_CALL(m_Color = cvScalar(channel1,channel2,channel3));         
      __END__;
      cvSetErrMode( CV_ErrModeLeaf );
      if( cvGetErrStatus() != CV_StsOk  )
      {
         throw new GeneralException("OPENCV - Error to construct the RGB color: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
      } 
	}
   double CvColor::channel3()
   {
      return m_Color.val[2];
   }
   double CvColor::channel2()
   {
      return m_Color.val[1];
   }
   double CvColor::channel1()
   {  
      return m_Color.val[0];
   }
	CvColor::~CvColor()
	{
      
	}
   
	/**Serialize (binary) the object to a stream*/
	void CvColor::serialize(std::ostream &out) const
	{
      
	}
   
	/**Unserialize (binary) the object from a stream*/
	void CvColor::unserialize(std::istream &in)
	{
      
	}
   
	/**Generic print function*/
	void CvColor::printOn(std::ostream &out) const
	{
		out << "CvColor ";
		out << &m_Color;		
	}
   
	/**Generic read function*/
	void CvColor::readFrom(std::istream &in)
	{
      
	}
   
   CvScalar CvColor::getColor()
   {
      return m_Color;
   }
   
}//namespace FD
