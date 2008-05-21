#ifndef _CVSTRUCTURINGELEMENT_H_
#define _CVSTRUCTURINGELEMENT_H_

#include "Object.h"
#include "CvImage.h"
#include <cv.h>
#include <highgui.h>
#include <string>

namespace FD
{
   
	class CvStructuringElement : public Object
	{
      
		public:
      
		CvStructuringElement(); 
		~CvStructuringElement();
      
      CvStructuringElement(int cols, int rows, int anchor_x, int anchor_y, int shape);
		
		/**Serialize (binary) the object to a stream*/
		virtual void serialize(std::ostream &out) const;
		
		/**Unserialize (binary) the object from a stream*/
		virtual void unserialize(std::istream &in);
		
		/**Generic print function*/
		virtual void printOn(std::ostream &out=std::cout) const;
      
		/**Generic read function*/
		virtual void readFrom(std::istream &in=std::cin);
      
		IplConvKernel* getStructuringElement();
      
		protected:
      
		IplConvKernel* m_structuringElement;           
	};
}

#endif
