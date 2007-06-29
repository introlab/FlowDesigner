#include "CvVideo.h"

namespace FD {
   
	using namespace std;
   
	DECLARE_TYPE(CvVideo);
   
	CvVideo::CvVideo()
   : m_video(NULL)
	{
	}
   
   CvVideo::CvVideo(CvCapture* video)
   {      
      m_video = video;      
   }   
   
	CvVideo::~CvVideo()
	{
      cvReleaseCapture( &m_video );
	}
   
	CvVideo::CvVideo(const std::string &path)
	{
      cvSetErrMode( CV_ErrModeSilent );
      __BEGIN__;
      OPENCV_CALL(m_video = cvCaptureFromFile(path.c_str()));
      __END__;
      cvSetErrMode( CV_ErrModeLeaf );
      if( cvGetErrStatus() != CV_StsOk  )
      {
         throw new GeneralException("OPENCV - Error to load the video: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
      }
	}
   
	/**Serialize (binary) the object to a stream*/
	void CvVideo::serialize(std::ostream &out) const
	{
      
	}
   
	/**Unserialize (binary) the object from a stream*/
	void CvVideo::unserialize(std::istream &in)
	{
      
	}
   
	/**Generic print function*/
	void CvVideo::printOn(std::ostream &out) const
	{
		out << "CvVideo: ";
      out << m_video;
	}
   
	/**Generic read function*/
	void CvVideo::readFrom(std::istream &in)
	{
      
	}
   
   CvCapture* CvVideo::getVideo()
   {
      return m_video;
   }
}//namespace FD
