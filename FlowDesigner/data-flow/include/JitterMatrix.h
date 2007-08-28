#ifndef JITTERMATRIX_H
#define JITTERMATRIX_H

#include "Object.h"
#include <iostream>
#include <sstream>
#include <string>

using namespace std;


namespace FD
{
   
   typedef struct _jit_net_packet_header
   {   
      string ID;
      long size; //size of packet to come   
   } t_jit_net_packet_header;
   
   typedef struct _jit_net_packet_matrix 
   {
      string   ID;
      long   size;
      
      long   planeCount;
      long   type;         //0=char,1=long,2=float32,3=float64
      long   dimCount;
      long   dim[32];
      long   dimStride[32];
      long   dataSize;
      double   time;      
   } t_jit_net_packet_matrix;
   
   char* reverse(const char* data, const int size);
   
	class JitterMatrix : public Object
	{
      
		public:
      
      JitterMatrix();
      ~JitterMatrix();
      JitterMatrix(const JitterMatrix& jitterMatrix);
      
      JitterMatrix(const long& width
                              , const long& height
                              , const long& planeCount
                              , const double& time
                              , const long& type
                              , const char* buffer);
		
		/**Serialize (binary) the object to a stream*/
		virtual void serialize(std::ostream &out) const;
		
		/**Unserialize (binary) the object from a stream*/
		virtual void unserialize(std::istream &in);
		
		/**Generic print function*/
		virtual void printOn(std::ostream &out=std::cout) const;
      
		/**Generic read function*/
		virtual void readFrom(std::istream &in=std::cin);
      
      void setIDHeader(char* IDHeader);
      void setSizeHeader(char* sizeHeader);
      
      void setIDMatrix(char* IDMatrix);
      void setSizeMatrix(char* SizeMatrix); 
      
      void setPlaneCount(char* planeCount);
      void setType(char* type); 
      void setDimCount(char* dimCount);
      void setDim(char* dim);
      void setDimStride(char* dimStride); 
      void setDataSize(char* dataSize);
      void setTime(char* time);
      void setBuffer(const char* buffer);
      
      const t_jit_net_packet_header getHeader();
      const t_jit_net_packet_matrix getMatrix();  
      char* getBuffer();
      long getWidth();
      long getHeight();
      long getPlaneCount();
      
      protected:
      
      t_jit_net_packet_header m_packetHeader;
      t_jit_net_packet_matrix m_packetMatrix; 
      char* m_buffer;  
      
      
	};
}

#endif
