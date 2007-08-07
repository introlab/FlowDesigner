#include "JitterMatrix.h"
namespace FD {
   
	using namespace std;
   
   char* reverse(const char* data, const int size)
   {
      char* reverse = new char[size];
      int i=0;
      for(i=0;i<size;i++)
      {
         reverse[i] = data[size-1-i];
      }
      return reverse;
   }
   
	DECLARE_TYPE(JitterMatrix);
   
	JitterMatrix::JitterMatrix() : m_buffer(NULL)
	{
      m_packetMatrix.ID = "NULL";
      m_packetHeader.ID = "NULL";
      m_packetMatrix.dataSize = 0;
	} 
	JitterMatrix::~JitterMatrix()
	{
       if (m_buffer!=NULL) delete[] m_buffer;
	} 
   
	JitterMatrix::JitterMatrix(const JitterMatrix& jitterMatrix)
	{
      
      m_packetHeader = jitterMatrix.m_packetHeader;
      m_packetMatrix = jitterMatrix.m_packetMatrix;
      if(jitterMatrix.m_packetMatrix.dataSize != 0)
      {
         m_buffer = new char[jitterMatrix.m_packetMatrix.dataSize];
         memcpy(m_buffer, jitterMatrix.m_buffer, jitterMatrix.m_packetMatrix.dataSize);
      }
      else
      {         
         m_buffer = 0;
      }
	}   
   
   JitterMatrix::JitterMatrix(const long& width
                              , const long& height
                              , const long& planeCount
                              , const double& time
                              , const long& type
                              , const char* buffer)
   {
      m_packetHeader.ID = "JMTX";
      m_packetHeader.size = 288;
      m_packetMatrix.ID = "JMTX";
      m_packetMatrix.size = 288;
      m_packetMatrix.planeCount = planeCount;
      m_packetMatrix.type = type;
      m_packetMatrix.dimCount = 2;
      m_packetMatrix.dim[0] = width;
      m_packetMatrix.dim[1] = height;
      m_packetMatrix.dimStride[0] = planeCount;
      m_packetMatrix.dimStride[1] = planeCount*width;
      m_packetMatrix.dataSize = planeCount*width*height;
      m_packetMatrix.time = time;
      m_packetMatrix.type = type;
      if( type == 1 || type == 2) // 1 -> long; 2 -> float32
      {
         m_packetMatrix.dimStride[1] *= 4;
         m_packetMatrix.dataSize *= 4;         
      }
      else if( type==3 ) // 3 -> float64
      {
         m_packetMatrix.dimStride[1] *= 8;
         m_packetMatrix.dataSize *= 8;
      }
      if(m_buffer!=NULL)
      {
            delete[] m_buffer;            
      }
      m_buffer = new char[m_packetMatrix.dataSize];
      memcpy(m_buffer, buffer, m_packetMatrix.dataSize);
   }

   
	/**Serialize (binary) the object to a stream*/
	void JitterMatrix::serialize(std::ostream &out) const
	{      
	}
   
	/**Unserialize (binary) the object from a stream*/
	void JitterMatrix::unserialize(std::istream &in)
	{      
	}
   
	/**Generic print function*/
	void JitterMatrix::printOn(std::ostream &out) const
	{
		out << "JitterMatrix" << endl;
      if(m_packetMatrix.ID == "JMTX")
      {
         out << "ID Header: "    << m_packetHeader.ID << endl; 
         out << "Size Header : " << m_packetHeader.size << endl;
         out << "ID Matrix : "   << m_packetMatrix.ID << endl;
         out << "Size Matrix : " << m_packetMatrix.size << endl; 
         out << "PlanetCount : " << m_packetMatrix.planeCount << endl;
         out << "Type : "        << m_packetMatrix.type << endl; 
         out << "DimCount : "    << m_packetMatrix.dimCount << endl; 
         out << "Dim : ["        << m_packetMatrix.dim[0] << ":" 
         << m_packetMatrix.dim[1] << "]" << endl;
         out << "DimStride : ["  << m_packetMatrix.dimStride[0] << ":" 
         << m_packetMatrix.dimStride[1] << "]" << endl;
         out << "DataSize : "    << m_packetMatrix.dataSize << endl;
         out << "Time : "        << m_packetMatrix.time << endl;
         
      }
      else
      {
         out << "NULL"; 
      }      
	}
   
	/**Generic read function*/
	void JitterMatrix::readFrom(std::istream &in)
	{      
	}
   
   /*******************************/
   /**   Set Functions           **/
   /******************************/
   
   void JitterMatrix::setIDHeader(char* IDPacketHeader)
   {
      string temp(IDPacketHeader,4);
      m_packetHeader.ID = temp;
   }
   
   void JitterMatrix::setSizeHeader(char* sizeHeader)
   {
      m_packetHeader.size = *(reinterpret_cast<long*>(sizeHeader));      
   }
   
   void JitterMatrix::setIDMatrix(char* IDMatrix)
   {            
      string temp(IDMatrix,4);
      m_packetMatrix.ID = temp;
   }
   
   void JitterMatrix::setSizeMatrix(char* sizeMatrix)
   {
      m_packetMatrix.size = *(reinterpret_cast<long*>(sizeMatrix));      
   } 
   
   void JitterMatrix::setPlaneCount(char* planeCount)
   {
      m_packetMatrix.planeCount = *(reinterpret_cast<long*>(planeCount)); 
   }
   
   void JitterMatrix::setType(char* type)
   {
      m_packetMatrix.type = *(reinterpret_cast<long*>(type)); 
   }
   
   void JitterMatrix::setDimCount(char* dimCount)
   {
      m_packetMatrix.dimCount = *(reinterpret_cast<long*>(dimCount)); 
   }
   
   void JitterMatrix::setDim(char* dim)
   {
      int i=0;
      char* temp;
      for(i=0;i<32;i++)
      {
         temp = reverse(dim+i*4,4);
         m_packetMatrix.dim[i] = *(reinterpret_cast<long*>(temp));          
      }  
   }
   
   void JitterMatrix::setDimStride(char* dimStride)
   {
      int i=0;
      char* temp ;
      for(i=0;i<32;i++)
      {
         temp = reverse(dimStride+i*4,4);
         m_packetMatrix.dimStride[i] = *(reinterpret_cast<long*>(temp));          
      }  
   }
   
   void JitterMatrix::setDataSize(char* dataSize)
   {
      m_packetMatrix.dataSize = *(reinterpret_cast<long*>(dataSize)); 
   }
   
   void JitterMatrix::setTime(char* time)
   {
      m_packetMatrix.time = *(reinterpret_cast<double*>(time)); 
   }  
   
   void JitterMatrix::setBuffer(const char* buffer)
   {
      if(m_buffer!=NULL)
      {
            delete[] m_buffer;            
      }
      m_buffer = new char[m_packetMatrix.dataSize];
      memcpy(m_buffer, buffer, m_packetMatrix.dataSize);
   }
   
   /*******************************/
   /**   get Functions           **/
   /******************************/
   
   const t_jit_net_packet_header JitterMatrix::getHeader()
   {
      return m_packetHeader;
   }
   const t_jit_net_packet_matrix JitterMatrix::getMatrix()
   {
      return m_packetMatrix;
   } 
   
   char* JitterMatrix::getBuffer()
   {
      return m_buffer;
   }    
   
   long JitterMatrix::getWidth()
   {
      return m_packetMatrix.dim[0];
   }
   
   long JitterMatrix::getHeight()
   {
      return m_packetMatrix.dim[1];
   }
   long JitterMatrix::getPlaneCount()
   {
      return m_packetMatrix.planeCount;
   }   
   
}//namespace FD
