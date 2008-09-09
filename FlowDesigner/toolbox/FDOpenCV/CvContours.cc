#include "CvContours.h"
#include <iostream>

namespace FD {
   
	using namespace std;
   
	DECLARE_TYPE(CvContours);
   
	CvContours::CvContours() :
   
    m_firstContours(0) , m_nbContours(0)
	{
      
	}
   
	CvContours::CvContours(const CvContours* contours)
   {
      m_nbContours = contours->getNbContours();
      m_firstContours = contours->getFirstContours();
      m_contours = contours->getContours();	      
	}   
   
	CvContours::~CvContours()
	{
      
	}
   
   
	/**Serialize (binary) the object to a stream*/
	void CvContours::serialize(std::ostream &out) const
	{
      
	}
   
	/**Unserialize (binary) the object from a stream*/
	void CvContours::unserialize(std::istream &in)
	{
      
	}
   
	/**Generic print function*/
	void CvContours::printOn(std::ostream &out) const
	{
		out << "CvContours ";
		out << &m_contours;		
	}
   
	/**Generic read function*/
	void CvContours::readFrom(std::istream &in)
	{
      
	}
   
   CvSeq*  CvContours::getContours() const
   {
      return m_contours;
   }
   
   CvSeq*  CvContours::getFirstContours() const
   {
      return m_firstContours;
   }

   int CvContours::getNbContours() const
   {
      return m_nbContours;
   }
   
   void CvContours::setContours(CvSeq* contours)
   {
      m_contours = contours;
   }
   
   void CvContours::setFirstContours(CvSeq* firstContours)
   {
      m_firstContours = firstContours;
   }
   
   void CvContours::setNbContours(int nbContours)
   {
      m_nbContours = nbContours;
   }   
 
   /*
   CvSeq* CvContours::cloneMemory(CvSeq* contour,CvMemStorage* storage, int v,int* h)
   {
      cout << "V = ";
      cout << v << endl;
      cout << "H = ";
      cout << h[v] << endl;
      CvSeq* CvSeqTemp =  contour->v_next;
      if (CvSeqTemp != 0)
      {
         v++;
         cloneMemory(CvSeqTemp,storage, v, h);
      }
      else
      {
         CvSeqTemp =  contour->h_next;
         if (CvSeqTemp != 0)
         {
            h[v] = h[v]++;
            cloneMemory(CvSeqTemp,storage, v, h);
         }
         else
         {
            int i;
            cout <<"SAVE" << endl;
            CvSeq* CvSeqFinal = cvCloneSeq(contour,storage);
            for (i=0;i<h[v] ;i++)
            {
               cout <<"SAVE" << endl;
               contour = contour->h_prev;
               CvSeqFinal = cvCloneSeq(contour,storage);
            }
            
            cout <<"trace 1" << endl;
            
            CvSeqTemp =  contour->v_prev;     
            cout <<"trace 2" << endl;
            
            if (CvSeqTemp != 0)
            {
               v--;
               cloneMemory(CvSeqTemp,storage, v, h);
            }
            cout <<"trace 3" << endl;
            
            return CvSeqFinal;           
         }
      }
      
   }
   */
}//namespace FD
