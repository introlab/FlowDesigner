#include "BufferedNode.h"
#include "operators.h"
#include <QtNetwork/QTcpServer>
#include <QtNetwork/QTcpSocket>
#include <string>
#include "JitterMatrix.h"

using namespace std;
namespace FD {
   
   class QtListenJitterMatrix;
   
   DECLARE_NODE(QtListenJitterMatrix)
   
   /*Node
   *
   * @name QtListenJitterMatrix
   * @category FDQtTCP
   * @description Create a tcp server
   *
   * @parameter_name HOSTADDRESS
   * @parameter_type string
   * @parameter_value 127.0.0.1
   * @parameter_description Host address
   *
   * @parameter_name PORT
   * @parameter_type int
   * @parameter_value 7000
   * @parameter_description Port
   *
   * @output_name JITTERMATRIX
   * @output_description The server to be used
   * @output_type JitterMatrix
   *
   END*/
   
   class QtListenJitterMatrix : public BufferedNode {
      
      const static int bufferLimit = 1000;
      
      public:
      //Output ID
      int m_jitterMatrixID;
      //Parameters
      QString m_hostAddress;
      quint16 m_port;
      
      QTcpServer m_tcpServer;
      QTcpSocket* m_tcpServerConnection;
      QByteArray m_bytesRead;
      JitterMatrix m_matrix;
      
      QtListenJitterMatrix(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      , m_tcpServerConnection(0)
      {
         //add outputs
         m_jitterMatrixID = addOutput("JITTERMATRIX");
         
         //Initialize parameters
         m_hostAddress =  QString( ( object_cast<String>( parameters.get("HOSTADDRESS") ) ).c_str() );   
         m_port = dereference_cast<int>(parameters.get("PORT"));
         tryToListen();        
      }
      
      void calculate(int output_id, int count, Buffer &out)   
      {
         if(!m_tcpServer.isListening())
         {
            if(!tryToListen())
            {
               (*(outputs[m_jitterMatrixID].buffer))[count] = ObjectRef(new JitterMatrix());
               return;               
            }
         }         
         if(m_tcpServer.waitForNewConnection(1))
         {
            cout<<"QtListenJitterMatrix -> Accept new connection"<<endl; 
            m_tcpServerConnection = m_tcpServer.nextPendingConnection();            
            m_tcpServerConnection->setReadBufferSize(bufferLimit);
         }
        
         if(m_tcpServerConnection!=0)
         {                  
            if(m_tcpServerConnection->waitForReadyRead(1))
            {                                                          
               read();
            }
         }
         (*(outputs[m_jitterMatrixID].buffer))[count] = ObjectRef(new JitterMatrix(m_matrix));
      } 
      bool tryToListen()
      {
         if (m_tcpServer.listen(QHostAddress(m_hostAddress), m_port)) 
         {
            cout << "QtListenJitterMatrix -> OK to listen" << endl;
            return true;
         }
         else
         {
            cout << "QtListenJitterMatrix -> ERROR to listen: ";
            cout<< QStringToString(m_tcpServer.errorString()) << endl;
         }
         return false;
      }
      
      void read()  
      {
         m_bytesRead = m_tcpServerConnection->read(4);          
         if ( m_bytesRead == "JMTX")
         {         
            //cout << "QtListenJitterMatrix -> New JitterMatrix " << endl; 
            readJitterMatrix();
         }
         else
         {         
            cout << "QtListenJitterMatrix -> unkmowData - Erase buffer" << endl;
            m_tcpServerConnection->readAll();
         }
      }
      
      void readJitterMatrix()
      {

         // id 	 'JMTX'
         m_matrix.setIDHeader(m_bytesRead.data());
         
         // size next packet (288)
         m_bytesRead = m_tcpServerConnection->read(4);
         m_matrix.setSizeHeader(m_bytesRead.data());

         //Try to read the size header (4 bytes) and the matrix header
                     
         
         
         m_bytesRead.clear();
         int bytesAvailable = m_tcpServerConnection->bytesAvailable();
         while(bytesAvailable<((m_matrix.getHeader().size)-(m_bytesRead.size())))
         {
            if(bytesAvailable >= bufferLimit)
            {               
               m_bytesRead += m_tcpServerConnection->readAll();                    
            }
            if(!m_tcpServerConnection->waitForReadyRead(100))
            {
               cout << "QtListenJitterMatrix -> ERROR : ReadBuffer TimeOut - Erase buffer" << endl;               
               m_tcpServerConnection->readAll();
               m_matrix = JitterMatrix();
               return;
            }
            bytesAvailable = m_tcpServerConnection->bytesAvailable();
         }
         m_bytesRead += m_tcpServerConnection->read(m_matrix.getHeader().size-m_bytesRead.size());
         
         
         
         
         /*m_bytesRead = m_tcpServerConnection->read(m_matrix.getHeader().size-m_bytesRead.size());
         while(m_bytesRead.size()<m_matrix.getHeader().size)
         {           
            cout << "trace 2" << endl;                 
            if(!m_tcpServerConnection->waitForReadyRead(100))
            {
               cout << "QtListenJitterMatrix -> ERROR : ReadHeader TimeOut - Erase buffer"<< endl;
               m_tcpServerConnection->readAll();
               m_matrix = JitterMatrix();
               return;
            }    
            cout << "trace 3" << endl;
            m_bytesRead += m_tcpServerConnection->read(m_matrix.getHeader().size-m_bytesRead.size());
            cout << "trace 4" << endl;
         }*/
         
         
         
         
         
         
         
         //data pointer to the next data to manage
         char* data = m_bytesRead.data();
         
         // id 	 'JMTX'     
         m_matrix.setIDMatrix(reverse(data,4));       
         data += 4;      
         // Size :	 288 (32-bit int, size of this header)
         m_matrix.setSizeMatrix(reverse(data,4));
         data += 4;      
         //Planecount 	 32-bit int      
         m_matrix.setPlaneCount(reverse(data,4));
         data += 4;      
         // Type 	 32-bit int, 0 for char, 1 for long, 2 for float32, 3 for float64      
         m_matrix.setType(reverse(data,4));
         data += 4;      
         // DimCount 32-bit int      
         m_matrix.setDimCount(reverse(data,4));
         data += 4;      
         //  Dim 	 Array of 32 32-bit ints
         m_matrix.setDim(data);
         data += 4*32;      
         //Dimstride  Array of 32 32-bit ints
         m_matrix.setDimStride(data);
         data += 4*32;      
         //DataSize 	 32-bit int, size of the data buffer to come      
         m_matrix.setDataSize(reverse(data,4));
         data += 4;      
         // Time 	 64-bit double precision float      
         m_matrix.setTime(reverse(data,8));

         m_bytesRead.clear();
         bytesAvailable = m_tcpServerConnection->bytesAvailable();

         while(bytesAvailable<((m_matrix.getMatrix().dataSize)-(m_bytesRead.size())))
         {
            if(bytesAvailable >= bufferLimit)
            {               
               m_bytesRead += m_tcpServerConnection->readAll();                    
            }
            if(!m_tcpServerConnection->waitForReadyRead(100))
            {
               cout << "QtListenJitterMatrix -> ERROR : ReadBuffer TimeOut - Erase buffer" << endl;               
               m_tcpServerConnection->readAll();
               m_matrix = JitterMatrix();
               return;
            }
            bytesAvailable = m_tcpServerConnection->bytesAvailable();
         }

         m_bytesRead += m_tcpServerConnection->read(m_matrix.getMatrix().dataSize-m_bytesRead.size());

         /*
         m_bytesRead = m_tcpServerConnection->read(m_matrix.getMatrix().dataSize);
         while(m_bytesRead.size()<m_matrix.getMatrix().dataSize)
         { 
            if(!m_tcpServerConnection->waitForReadyRead(100))
            {
               cout << "QtListenJitterMatrix -> ERROR : ReadBuffer TimeOut - Erase buffer" << endl;               
               m_tcpServerConnection->readAll();
               m_matrix = JitterMatrix();
               return;
            } 
            //cout << "QtListenJitterMatrix -> " << m_tcpServerConnection->bytesAvailable();
            m_bytesRead += m_tcpServerConnection->read(m_matrix.getMatrix().dataSize-m_bytesRead.size());
         }*/
         
         m_matrix.setBuffer(m_bytesRead.data()); 

         //cout << "QtListenJitterMatrix -> Read JMTX SUCCESS" << endl;
         
      }
      
      const string QStringToString(const QString qstring)
      {            
         return std::string( (char*)(qstring.data()), qstring.size() * sizeof(QChar));   
      }
      
   };
}//namespace FD

