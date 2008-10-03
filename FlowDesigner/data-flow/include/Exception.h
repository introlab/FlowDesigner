// Copyright (C) 1999 Jean-Marc Valin

#ifndef _NETWORKEXCEPTION_H_
#define _NETWORKEXCEPTION_H_

//#include "Node.h"
#include <iostream>
#include <string>
//#include "rc_ptrs.h"
#include "BaseException.h"

namespace FD {

/***************************************************************************/
/*
  NodeNotFoundException
  Dominic Letourneau
 */
/***************************************************************************/
/** The NodeNotFoundException is thrown when a node isn't in the nodeDictionary
    @author Dominic Letourneau
    @version 1.0
*/
class NodeNotFoundException : public BaseException {

public:

   ///This constructor is used to specify which node is not found.
   NodeNotFoundException(std::string name) {
      nodeName = name;
   }   
   ///The print method that prints on stderr by default
   virtual void print(std::ostream &out = std::cerr) {
      out<<"NodeNotFoundException occured, nodeName: "<<nodeName<<std::endl;
   }
   ///The node name.
   std::string nodeName;
};

/***************************************************************************/
/*
  NoSinkNodeException
  Dominic Letourneau
 */
/***************************************************************************/
/** NoSinkNodeException occurs when the network is not properly initialized.
    @author Dominic Letourneau
    @version 1.0
*/
class  NoSinkNodeException : public BaseException {

public:
   ///default constructor
   NoSinkNodeException() {errorNo = 0;}

   ///constructor with an error number
   NoSinkNodeException(int value) {
      errorNo = value;
   }
   ///The print method
   virtual void print(std::ostream &out = std::cerr) {
      out<<"NoSinkNodeException occured, errorNo: "<<errorNo<<std::endl;
   }
   ///the error number
   int errorNo;
};

/***************************************************************************/
/*
  NoInputNodeException
  Dominic Letourneau
 */
/***************************************************************************/
/** NoInputNodeException occurs when the network has no input node defined.
    @author Dominic Letourneau
    @version 1.0
*/
class  NoInputNodeException : public BaseException {

public:

   ///default constructor
   NoInputNodeException() {errorNo = 0;}

   ///constructor with an error value
   NoInputNodeException(int value) {
      errorNo = value;
   }

   ///the print method
   virtual void print(std::ostream &out = std::cerr) {
      out<<"NoInputNodeException occured, errorNo: "<<errorNo<<std::endl;
   }

   ///the error number
   int errorNo;
};


/***************************************************************************/
/*
  FactoryNotFoundException
  Dominic Letourneau
 */
/***************************************************************************/
/** The FactoryNotFoundException occurs when we try to create
    a new node with an unknown factory.
    @author Dominic Letourneau
    @version 1.0
*/
class FactoryNotFoundException : public BaseException {

public:

   ///The constructor with a factory name.
   FactoryNotFoundException(std::string name) {
      factoryName = name;
   }   
   ///The print method
   virtual void print(std::ostream &out = std::cerr) {
      out<<"FactoryNotFoundException occured, factoryName: "<<factoryName<<std::endl;
   }
   ///The factory name
   std::string factoryName;
};

}//end namespace FD

#endif
