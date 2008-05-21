#ifndef _CVCONTOURS_H_
#define _CVCONTOURS_H_

#include "Object.h"
#include <cv.h>
#include <highgui.h>
#include <string>
#include "CvImage.h"

namespace FD
{
	class CvContours : public Object
	{
      
		public:
      
		CvContours(); 
		~CvContours();
      
	   CvContours(const CvContours* contours);
		
		/**Serialize (binary) the object to a stream*/
		virtual void serialize(std::ostream &out) const;
		
		/**Unserialize (binary) the object from a stream*/
		virtual void unserialize(std::istream &in);
		
		/**Generic print function*/
		virtual void printOn(std::ostream &out=std::cout) const;
      
		/**Generic read function*/
		virtual void readFrom(std::istream &in=std::cin);
      
      CvSeq* getFirstContours() const;
      CvSeq* getContours() const; 
      int getNbContours() const;      
      
      void setFirstContours(CvSeq* firstContours);
      void setContours(CvSeq* contours);
      void setNbContours(int nbContours);
      
      //CvSeq* cloneMemory(CvSeq* contour,CvMemStorage* storage, int h,int* v);
      CvSeq* m_firstContours;   
      CvSeq* m_contours;      
      int m_nbContours;      
	};
}

#endif
