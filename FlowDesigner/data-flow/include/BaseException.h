// Copyright (C) 1999 Jean-Marc Valin

#ifndef _BASEEXCEPTION_H_
#define _BASEEXCEPTION_H_



//#include "Node.h"
#include <iostream>
#include <string>
//#include "rc_ptrs.h"

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
   virtual void print (std::ostream &out = std::cerr) = 0;

   /**Freezes the error message. This is used when the object an exception
      refers (or might refer) to will be destroyed*/
   virtual void freeze() {}

   ///Destructor
   virtual ~BaseException(){;}

   virtual BaseException *add(BaseException *e);

};

class GenericCastException : public BaseException{
public:
   virtual void print(std::ostream &out = std::cerr)=0;
};

/***************************************************************************/
/*
  GeneralException
  Jean-Marc Valin
 */
/***************************************************************************/
/** The GeneralException is intended to write a generic message to the user
    with file where the exception occured and the line number.
    user __FILE__ and __LINE__ for convenience.
    @author Jean-Marc Valin
    @version 1.0
*/
class GeneralException :public BaseException {
public:

   ///The constructor that takes the message the file name and the line number
   GeneralException(std::string _message, std::string _file, int _line) 
      : message(_message)
      , file (_file)
      , line (_line)
   {}

   ///The print method
   virtual void print(std::ostream &out = std::cerr)
   {
      out << file << " line " << line << ": " << message << std::endl;
   }

protected:
   ///the message
   std::string message;

   ///the file name
   std::string file;
   
   ///the line number
   int line;
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
	  
   std::vector<BaseException *> stack;

public:
   ExceptionStack() {};
   
   BaseException *add(BaseException *e)
      {
	 stack.insert(stack.end(), e);
	 return this;
      }
   
   virtual ~ExceptionStack() 
      {for (size_t i=0;i<stack.size();i++) delete stack[i];}
   ///The print method that prints on stderr by default
   virtual void print(std::ostream &out = std::cerr) {
      for (unsigned int i=0;i<stack.size();i++)
	 stack[i]->print(out);
   }

   virtual void freeze()
   {
      for (unsigned int i=0;i<stack.size();i++)
	 stack[i]->freeze();
   }
};

inline BaseException *BaseException::add(BaseException *e)
{
   return (new ExceptionStack)->add(this)->add(e);
}

#endif
