#ifndef _CVIMAGE_H_
#define _CVIMAGE_H_

#include "Object.h"
#include <cv.h>
#include <highgui.h>
#include <string>
#include <iostream>
#include <sstream>

namespace FD
{
   inline std::string CCHAR(const char* value)
   {
      std::stringstream sstr;
      sstr << value;
      return(sstr.str());
   } 
   
	class CvImage : public Object
	{
      
		public:
      
        CvImage();
      
      CvImage(const IplImage* image);
      CvImage(const CvImage* image);

		~CvImage();
		CvImage(const std::string &path, const int &iscolor);
		
		/**Serialize (binary) the object to a stream*/
		virtual void serialize(std::ostream &out) const;
		
		/**Unserialize (binary) the object from a stream*/
		virtual void unserialize(std::istream &in);
		
		/**Generic print function*/
		virtual void printOn(std::ostream &out=std::cout) const;
      
		/**Generic read function*/
		virtual void readFrom(std::istream &in=std::cin);
      
		IplImage* getImage() const;
		void releaseImage();
      
      CvImage* gray() const;
      
      CvImage* bgr() const;
      
      CvImage* zero() const;
      
		protected:
      
		IplImage* m_image;
      
	};
}

#endif
