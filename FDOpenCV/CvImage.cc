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
      if(image != NULL)
      {
         int status = cvGetErrMode();
         cvSetErrMode( CV_ErrModeSilent ); 
         __BEGIN__;
         OPENCV_CALL(m_image = cvCloneImage(image));
         __END__;
         cvSetErrMode( status );
         if( cvGetErrStatus() != CV_StsOk  )
         {
            throw new GeneralException("OPENCV - Error to clone the image " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
         }
      }
      else
      {
         m_image = 0;
      }
   } 
   
   CvImage::CvImage(const CvImage* image)
   {
      int status = cvGetErrMode();
      cvSetErrMode( CV_ErrModeSilent ); 
      __BEGIN__;
      OPENCV_CALL(m_image = cvCloneImage(image->getImage()));
      __END__;
      cvSetErrMode( status );
      if( cvGetErrStatus() != CV_StsOk  )
      {
         throw new GeneralException("OPENCV - Error to clone the image " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
      }
   }
   
	CvImage::~CvImage()
	{
      if(m_image != NULL)
      {
         cvReleaseImage( &m_image );
      }
	}
   
	CvImage::CvImage(const std::string &path, const int &iscolor)
	{
      int status = cvGetErrMode();
      cvSetErrMode( CV_ErrModeSilent );
      __BEGIN__;
      OPENCV_CALL(m_image = cvLoadImage(path.c_str(), iscolor));
      __END__;
      cvSetErrMode( status );
      if( cvGetErrStatus() != CV_StsOk  )
      {
         throw new GeneralException("OPENCV - Error to load the image: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
      }
      if(m_image == NULL)
      {
         throw new GeneralException("OPENCV - Error to load the image: it's NULL check your path" ,__FILE__,__LINE__);
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
         int status = cvGetErrMode();
         cvSetErrMode( CV_ErrModeSilent );
         __BEGIN__;         
         OPENCV_CALL(image = new CvImage(cvCreateImage( cvGetSize(m_image), m_image->depth, 1)));
         OPENCV_CALL(cvCvtColor(m_image, image->getImage(), CV_BGR2GRAY));
         __END__;
         cvSetErrMode( status );
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
   
   CvImage* CvImage::bgr() const
   {
      CvImage* image;    
      if(m_image->nChannels == 1)
      {
         int status = cvGetErrMode();
         cvSetErrMode( CV_ErrModeSilent );
         __BEGIN__; 
         OPENCV_CALL(image = new CvImage(cvCreateImage( cvGetSize(m_image), m_image->depth, 3)));
         OPENCV_CALL(cvCvtColor(m_image, image->getImage(), CV_GRAY2BGR));
         __END__;
         cvSetErrMode( status );
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
      
      int status = cvGetErrMode();
      cvSetErrMode( CV_ErrModeSilent );
      __BEGIN__;       
      OPENCV_CALL(cvSetZero(image->getImage()));
      __END__;      
      cvSetErrMode( status );
      if( cvGetErrStatus() != CV_StsOk  )
      {
         throw new GeneralException("OPENCV - Error to clear the image: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
      }       
      return image;
   }
   
}//namespace FD
