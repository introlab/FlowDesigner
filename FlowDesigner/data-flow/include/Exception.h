// Copyright (C) 1999 Jean-Marc Valin
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this file.  If not, write to the Free Software Foundation,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#ifndef _NETWORKEXCEPTION_H_
#define _NETWORKEXCEPTION_H_

//#include "Node.h"
#include <iostream>
#include <string>
#include "rc_ptrs.h"
#include "BaseException.h"

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
   NodeNotFoundException(string name) {
      nodeName = name;
   }   
   ///The print method that prints on stderr by default
   virtual void print(ostream &out = cerr) {
      out<<"NodeNotFoundException occured, nodeName: "<<nodeName<<endl;
   }
   ///The node name.
   string nodeName;
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
   virtual void print(ostream &out = cerr) {
      out<<"NoSinkNodeException occured, errorNo: "<<errorNo<<endl;
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
   virtual void print(ostream &out = cerr) {
      out<<"NoInputNodeException occured, errorNo: "<<errorNo<<endl;
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
   FactoryNotFoundException(string name) {
      factoryName = name;
   }   
   ///The print method
   virtual void print(ostream &out = cerr) {
      out<<"FactoryNotFoundException occured, factoryName: "<<factoryName<<endl;
   }
   ///The factory name
   string factoryName;
};

#endif
