#ifndef _CVCOLOR_H_
#define _CVCOLOR_H_

#include "Object.h"
#include "CvImage.h"
#include <cv.h>
#include <highgui.h>
#include <string>

namespace FD
{
   
	class CvColor : public Object
	{
      
		public:
      
		CvColor(); 
		~CvColor();
      
      CvColor(int channel1, int channel2, int channel3);
		
		/**Serialize (binary) the object to a stream*/
		virtual void serialize(std::ostream &out) const;
		
		/**Unserialize (binary) the object from a stream*/
		virtual void unserialize(std::istream &in);
		
		/**Generic print function*/
		virtual void printOn(std::ostream &out=std::cout) const;
      
		/**Generic read function*/
		virtual void readFrom(std::istream &in=std::cin);
      
		CvScalar getColor();
      double channel1();
      double channel2();
      double channel3();
      
		protected:
      
		CvScalar m_Color;           
	};
}

#endif
