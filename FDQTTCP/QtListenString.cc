#include "BufferedNode.h"
#include "operators.h"
#include <QtNetwork/QTcpServer>
#include <QtNetwork/QTcpSocket>
#include <string>

using namespace std;
namespace FD {
   
   class QtListenString;
   
   DECLARE_NODE(QtListenString)
   
   /*Node
   *
   * @name QtListenString
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
   * @output_name STRING
   * @output_description The server to be used
   * @output_type string
   *
   * @output_name NEW
   * @output_description The server to be used
   * @output_type bool
   *
   END*/
   
   class QtListenString : public BufferedNode {
      
      
      public:
      //Output ID
      int m_stringID;
      int m_newID;
      //Parameters
      QString m_hostAddress;
      quint16 m_port;
      
      QTcpServer m_tcpServer;
      QTcpSocket* m_tcpServerConnection;
      QByteArray m_bytesRead;
      string m_string;
      bool m_new;
      
      QtListenString(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      , m_tcpServerConnection(0)
      , m_string()
      , m_new(false)
      {
         //add outputs
         m_stringID = addOutput("STRING");
         m_newID = addOutput("NEW");
         
         //Initialize parameters
         m_hostAddress =  QString( ( object_cast<String>( parameters.get("HOSTADDRESS") ) ).c_str() );   
         m_port = dereference_cast<int>(parameters.get("PORT")); 
         
         tryToListen();
      }
      
      void calculate(int output_id, int count, Buffer &out)   
      {
         m_new = false;
         if(!m_tcpServer.isListening())
         {
            if(!tryToListen())
            {
               (*(outputs[m_stringID].buffer))[count] = ObjectRef(new String(m_string));
               (*(outputs[m_newID].buffer))[count] = ObjectRef(Bool::alloc(m_new));
               return;               
            }
         }
         if(m_tcpServer.waitForNewConnection(1))
         {
            cout<<"QtListenString -> Accept new connection"<<endl; 
            m_tcpServerConnection = m_tcpServer.nextPendingConnection();
         }
         if(m_tcpServerConnection!=0)
         {                         
            if(m_tcpServerConnection->waitForReadyRead(1))
            {
               read();
            }
         }
         (*(outputs[m_stringID].buffer))[count] = ObjectRef(new String(m_string));
         (*(outputs[m_newID].buffer))[count] = ObjectRef(Bool::alloc(m_new));
         
      } 
      bool tryToListen()
      {
         if (m_tcpServer.listen(QHostAddress(m_hostAddress), m_port)) 
         {
            cout << "QtListenString -> OK to listen" << endl;
            return true;
         }
         else
         {
            cout << "QtListenString -> ERROR to listen: ";
            cout<< QStringToString(m_tcpServer.errorString()) << endl;
         }
         return false;
      }
      
      void read()  
      {
         m_new = true;
         QByteArray read = m_tcpServerConnection->readAll();  
         cout <<  string( read.data(), read.size()) << endl;
         m_string = string( read.data(), read.size());
      }  
      
      const string QStringToString(const QString qstring)
      {
         return std::string( (char*)(qstring.data()), qstring.size() * sizeof(QChar));   
      }
      
   };
}//namespace FD

