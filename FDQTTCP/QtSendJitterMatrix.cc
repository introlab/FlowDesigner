// Copyright (C) 2001 Dominic Letourneau

#include "BufferedNode.h"
#include "operators.h"
#include <QtNetwork/QTcpSocket>
#include <string>
#include "JitterMatrix.h"

using namespace std;

namespace FD {
   
   class QtSendJitterMatrix;
   
   DECLARE_NODE(QtSendJitterMatrix)
   
   /*Node
   *
   * @name QtSendJitterMatrix
   * @category FDQtTCP
   * @description Create a tcp socket
   *
   * @input_name JITTERMATRIX
   * @input_description JitterMatrix to send
   * @input_type JitterMatrix
   *
   * @parameter_name HOSTNAME
   * @parameter_type string
   * @parameter_value 127.0.0.1
   * @parameter_description Hostname
   *
   * @parameter_name PORT
   * @parameter_type int
   * @parameter_value 7000
   * @parameter_description Port
   *
   * @parameter_name WAITFORCONNECTED
   * @parameter_type int
   * @parameter_value 1000
   * @parameter_description How ms that the node wait for connected
   *
   * @output_name BOOLWRITE
   * @output_description A new matrix was send
   * @output_type bool
   *
   END*/
   
   class QtSendJitterMatrix : public BufferedNode {
      
      public:
      //Input ID
      int m_JitterMatrixID;
      //Output ID
      int m_boolWriteID;
      //Parameters
      QString m_hostname;
      quint16 m_port;
      int m_waitForConnected;
      //socket
      QTcpSocket m_tcpClient;      
      
      QtSendJitterMatrix(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_JitterMatrixID = addInput("JITTERMATRIX");
         
         //add outputs
         m_boolWriteID = addOutput("BOOLWRITE");
         
         //Initialize parameters
         m_hostname =  QString( ( object_cast<String>( parameters.get("HOSTNAME") ) ).c_str() );   
         m_port = dereference_cast<int>(parameters.get("PORT"));
         m_waitForConnected = dereference_cast<int>(parameters.get("WAITFORCONNECTED"));
      }  
      
      void calculate(int output_id, int count, Buffer &out)
      {
         RCPtr<JitterMatrix> matrix = getInput(m_JitterMatrixID,count);
         
         if (m_tcpClient.state() != 3)
         {
            m_tcpClient.connectToHost(m_hostname, m_port, QIODevice::WriteOnly);
            if(m_tcpClient.waitForConnected(m_waitForConnected))
            {
               cout << "QtSendJitterMatrix -> Connect" << endl;
            } 
            else
            {
               cout << "QtSendJitterMatrix -> Error of connection : ";
               cout << m_tcpClient.errorString().toStdString() << endl;
               (*(outputs[m_boolWriteID].buffer))[count] = ObjectRef(Bool::alloc(false));
               return;
            }
         }
         
         if((matrix->getHeader()).ID!="JMTX")
         {
            //cout << "WriteJitterMatrix -> not a JMTX valid" << endl;
            (*(outputs[m_boolWriteID].buffer))[count] = ObjectRef(Bool::alloc(false)); 
            return;                  
         }   
         
         //cout << "WriteJitterMatrix -> JMTX DATA" << endl;
         QByteArray send = QByteArray ( (matrix->getHeader()).ID.c_str(), 4 )
         + QByteArray ( (char*)( &((matrix->getHeader()).size) ), 4 )      
         + QByteArray ( reverse( (matrix->getMatrix()).ID.c_str() , 4), 4 )
         + QByteArray ( reverse( (char*)( &(matrix->getMatrix()).size ) , 4), 4 )
         + QByteArray ( reverse( (char*)( &(matrix->getMatrix()).planeCount ) , 4), 4 )
         + QByteArray ( reverse( (char*)( &(matrix->getMatrix()).type ) , 4), 4 )
         + QByteArray ( reverse( (char*)( &(matrix->getMatrix()).dimCount ) , 4), 4 );
         
         int i=0;
         for(i=0;i<32;i++)
         {
            send+=QByteArray ( reverse( (char*)( &(matrix->getMatrix()).dim[i] ) , 4), 4 );         
         }
         
         for(i=0;i<32;i++)
         {
            send+=QByteArray ( reverse( (char*)( &(matrix->getMatrix()).dimStride[i] ) , 4), 4 );         
         }
         
         send += QByteArray ( reverse( (char*)( &(matrix->getMatrix()).dataSize ) , 4), 4 )         
         + QByteArray ( reverse( (char*)( &(matrix->getMatrix()).time ) , 8), 8 );
         
         //cout << "WriteJitterMatrix -> Send Header" << endl;
         
         m_tcpClient.write(send);
         
         if(m_tcpClient.waitForBytesWritten())
         {            
            //cout << "WriteJitterMatrix -> OK"<< endl;
         }
         else
         {            
            cout << "WriteJitterMatrix -> ERROR"<< endl;
            return;
         }
         
         //cout << "WriteJitterMatrix -> Send Buffer" << endl; 
         
         qint64 byteSend=Q_INT64_C(0);
         
         while( byteSend < (matrix->getMatrix()).dataSize )
         {
            int max;
            if( ((matrix->getMatrix()).dataSize)-byteSend > 10000 )
            {
               max = 10000;            
            }
            else
            {            
               max = ((matrix->getMatrix()).dataSize)-byteSend;
            }
            
            byteSend += m_tcpClient.write(matrix->getBuffer() + byteSend, max);
            if(!m_tcpClient.waitForBytesWritten())  
            {            
               cout << "WriteJitterMatrix -> ERROR"<< endl;
               return;
            }
         }         
         (*(outputs[m_boolWriteID].buffer))[count] = ObjectRef(Bool::alloc(true));   
      }
   };
}//namespace FD


