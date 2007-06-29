#include "CvImage.h"

namespace FD {
   
	using namespace std;
   
	DECLARE_TYPE(CvImage);
   
	CvImage::CvImage()
   : m_image(NULL)
	{
	}
   
   CvImage::CvImage(const IplImage* image)
   {
      cvSetErrMode( CV_ErrModeSilent ); 
      __BEGIN__;
      OPENCV_CALL(m_image = cvCloneImage(image));
      __END__;
      cvSetErrMode( CV_ErrModeLeaf );
      if( cvGetErrStatus() != CV_StsOk  )
      {
         throw new GeneralException("OPENCV - Error to clone the image " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
      }
   } 
   
   CvImage::CvImage(const CvImage* image)
   {
      cvSetErrMode( CV_ErrModeSilent ); 
      __BEGIN__;
      OPENCV_CALL(m_image = cvCloneImage(image->getImage()));
      __END__;
      cvSetErrMode( CV_ErrModeLeaf );
      if( cvGetErrStatus() != CV_StsOk  )
      {
         throw new GeneralException("OPENCV - Error to clone the image " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
      }
   }
   
	CvImage::~CvImage()
	{
      cvReleaseImage( &m_image );
	}
   
	CvImage::CvImage(const std::string &path, const int &iscolor)
	{
      cvSetErrMode( CV_ErrModeSilent );
      __BEGIN__;
      OPENCV_CALL(m_image = cvLoadImage(path.c_str(), iscolor));
      __END__;
      cvSetErrMode( CV_ErrModeLeaf );
      if( cvGetErrStatus() != CV_StsOk  )
      {
         throw new GeneralException("OPENCV - Error to load the image: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
      }
	}
   
	/**Serialize (binary) the object to a stream*/
	void CvImage::serialize(std::ostream &out) const
	{
      
	}
   
	/**Unserialize (binary) the object from a stream*/
	void CvImage::unserialize(std::istream &in)
	{
      
	}
   
	/**Generic print function*/
	void CvImage::printOn(std::ostream &out) const
	{
		out << "CvImage: ";
      out << m_image;
	}
   
	/**Generic read function*/
	void CvImage::readFrom(std::istream &in)
	{
      
	}
   
   IplImage* CvImage::getImage() const
   {
      return m_image;
   }
   
   CvImage* CvImage::gray() const
   {
      CvImage* image;
      if(m_image->nChannels != 1)
      {         
         cvSetErrMode( CV_ErrModeSilent );
         __BEGIN__;         
         OPENCV_CALL(image = new CvImage(cvCreateImage( cvGetSize(m_image), m_image->depth, 1)));
         OPENCV_CALL(cvCvtColor(m_image, image->getImage(), CV_BGR2GRAY));
         __END__;
         cvSetErrMode( CV_ErrModeLeaf );
         if( cvGetErrStatus() != CV_StsOk  )
         {            
            throw new GeneralException("OPENCV - Error to convert in gray the image: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
         }           
      }
      else
      {
         image = new CvImage(m_image);
      }    
      return image;
   }   
   
   CvImage* CvImage::rgb() const
   {
      CvImage* image;    
      if(m_image->nChannels == 1)
      {
         cvSetErrMode( CV_ErrModeSilent );
         __BEGIN__; 
         OPENCV_CALL(image = new CvImage(cvCreateImage( cvGetSize(m_image), m_image->depth, 3)));
         OPENCV_CALL(cvCvtColor(m_image, image->getImage(), CV_GRAY2BGR));
         __END__;
         cvSetErrMode( CV_ErrModeLeaf );
         if( cvGetErrStatus() != CV_StsOk  )
         {
            throw new GeneralException("OPENCV - Error to convert in rgb the image: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
         } 
      }
      else
      {
         image = new CvImage(m_image);
      }
      return image;
   } 
   
   CvImage* CvImage::zero() const
   {
      CvImage* image = new CvImage(*this);  
      cvSetErrMode( CV_ErrModeSilent );
      __BEGIN__;       
      OPENCV_CALL(cvSetZero(image->getImage()));
      __END__;      
      cvSetErrMode( CV_ErrModeLeaf );
      if( cvGetErrStatus() != CV_StsOk  )
      {
         throw new GeneralException("OPENCV - Error to clear the image: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
      }       
      return image;
   }
   
}//namespace FD
