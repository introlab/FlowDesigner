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

#ifndef _BASEEXCEPTION_H_
#define _BASEEXCEPTION_H_

//#include "Node.h"
#include <iostream.h>
#include <string>
#include "rc_ptrs.h"

/***************************************************************************/
/*
  BaseException
  Dominic Letourneau
 */
/***************************************************************************/
/** This is the base class of all exceptions in the network. You should 
    always derive from this class if you want to make a new exception 
    handler.
    @author Dominic Letourneau
    @version 1.0
*/
class BaseException {
public:

   /** This pure virtual function is intended for debug purposes.
       Every exception should be able to print a message. */
   virtual void print (ostream &out = cerr) = 0;

   ///Destructor
   virtual ~BaseException(){;}

   virtual BaseException *add(BaseException *e);

};

class GenericCastException : public BaseException{
public:
   virtual void print(ostream &out = cerr)=0;
};

#include <vector>

/***************************************************************************/
/*
  ExceptionStack
  Jean-Marc Valin
 */
/***************************************************************************/
class ExceptionStack : public BaseException {
  protected:
   vector<BaseException *> stack;
public:
   ExceptionStack() {};
   
   BaseException *add(BaseException *e)
      {
	 stack.insert(stack.end(), e);
	 return this;
      }
   
   virtual ~ExceptionStack() 
      {for (int i=0;i<stack.size();i++) delete stack[i];}
   ///The print method that prints on stderr by default
   virtual void print(ostream &out = cerr) {
      for (int i=0;i<stack.size();i++)
	 stack[i]->print(out);
   }
};

inline BaseException *BaseException::add(BaseException *e)
{
   return (new ExceptionStack)->add(this)->add(e);
}

#endif
