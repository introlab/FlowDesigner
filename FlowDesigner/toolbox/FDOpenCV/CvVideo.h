#ifndef _CVVIDEO_H_
#define _CVVIDEO_H_

#include "Object.h"
#include "CvImage.h"
#include <cv.h>
#include <highgui.h>
#include <string>
#include <iostream>
#include <sstream>

namespace FD
{

	class CvVideo : public Object
	{
      
		public:
      
		CvVideo();
      CvVideo(CvCapture* image);
		~CvVideo();
		CvVideo(const std::string &path);
		CvVideo(const int index);
      
		/**Serialize (binary) the object to a stream*/
		virtual void serialize(std::ostream &out) const;
		
		/**Unserialize (binary) the object from a stream*/
		virtual void unserialize(std::istream &in);
		
		/**Generic print function*/
		virtual void printOn(std::ostream &out=std::cout) const;
      
		/**Generic read function*/
		virtual void readFrom(std::istream &in=std::cin);
      
		CvCapture* getVideo();
      
		protected:
      
		CvCapture* m_video;
      
	};
}

#endif
