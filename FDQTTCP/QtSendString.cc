// Copyright (C) 2001 Dominic Letourneau

#include "BufferedNode.h"
#include "operators.h"
#include <QtNetwork/QTcpSocket>
#include <string>

using namespace std;

namespace FD {
   
   class QtSendString;
   
   DECLARE_NODE(QtSendString)
   
   /*Node
   *
   * @name QtSendString
   * @category FDQtTCP
   * @description Create a tcp socket
   *
   * @input_name STRING
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
   
   class QtSendString : public BufferedNode {
      
      public:
      //Input ID
      int m_stringID;
      //Output ID
      int m_boolWriteID;
      //Parameters
      QString m_hostname;
      quint16 m_port;
      int m_waitForConnected;
      //socket
      QTcpSocket m_tcpClient;      
      
      QtSendString(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_stringID = addInput("STRING");
         
         //add outputs
         m_boolWriteID = addOutput("BOOLWRITE");
         
         //Initialize parameters
         m_hostname =  QString( ( object_cast<String>( parameters.get("HOSTNAME") ) ).c_str() );   
         m_port = dereference_cast<int>(parameters.get("PORT"));
         m_waitForConnected = dereference_cast<int>(parameters.get("WAITFORCONNECTED"));
      }  
      
      void calculate(int output_id, int count, Buffer &out)
      {
         RCPtr<String> stringIn = getInput(m_stringID,count);
         
         if (m_tcpClient.state() != 3)
         {
            m_tcpClient.connectToHost(m_hostname, m_port, QIODevice::WriteOnly);
            if(m_tcpClient.waitForConnected(m_waitForConnected))
            {
               cout << "QtSendString -> Connect" << endl;
            } 
            else
            {
               cout << "QtSendString -> Error of connection : ";
               cout << m_tcpClient.errorString().toStdString() << endl;
               (*(outputs[m_boolWriteID].buffer))[count] = ObjectRef(Bool::alloc(false));
               return;
            }
         }
         
         m_tcpClient.write( stringIn->c_str(), stringIn->size() );
         
         if(!m_tcpClient.waitForBytesWritten(10))
         {            
            cout << "WriteJitterMatrix -> ERROR"<< endl;
            (*(outputs[m_boolWriteID].buffer))[count] = ObjectRef(Bool::alloc(false));
            return;
         }
         (*(outputs[m_boolWriteID].buffer))[count] = ObjectRef(Bool::alloc(true));
      }    
   };
}//namespace FD


