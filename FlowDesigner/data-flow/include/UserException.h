// Copyright (C) 2002 Jean-Marc Valin & Dominic Letourneau
#ifndef _USEREXCEPTION_H_
#define _USEREXCEPTION_H_

using namespace std;

#include <iostream>
#include <string>


/** This is the base class of all exceptions in the network caused by user intervention. You should 
    always derive from this class if you want to make a new exception handler.
    @author Dominic Letourneau & Jean-Marc Valin
    @version 1.0
*/
class UserException {

public:

   /** This pure virtual function is intended for debug purposes.
       Every exception should be able to print a message. */
   //virtual void print (ostream &out = cerr) = 0;

   ///Destructor
   virtual ~UserException(){;}

};


#endif
