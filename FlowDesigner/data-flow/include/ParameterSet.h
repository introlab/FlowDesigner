// Copyright (C) 1999 Jean-Marc Valin

#ifndef PARAMETERSET_H
#define PARAMETERSET_H

#include "Object.h"
#include <map>
#include <string>
#include "BaseException.h"

namespace FD {

/** A ParameterSet is a data structure that holds all the parameters 
    needed for the construction of a new node.
    @author Jean-Marc Valin
    @version 1.0
*/
class ParameterSet : public std::map<std::string,std::pair<ObjectRef,bool> > {
public:
   /**Does a certain parameter exist?*/
   bool exist(const std::string &param) const;

   /**get a parameter's value*/
   ObjectRef get(std::string param) const;

   /**get the default parameter*/
   ObjectRef getDefault(std::string param, ObjectRef value);

   /**set the default parameter*/
   void defaultParam(std::string param, ObjectRef value);

   /**adding the parameters*/
   void add(std::string param, ObjectRef value);

   /**printing the parameters*/
   void print(std::ostream &out = std::cerr) const;

   /**check whether there are any unused (never read) parameters (unrecognized)*/
   void checkUnused() const;
};

/** The ParameterException occurs when a node needs a parameter
    for its initialization and couldn't find it or if a parameter
    is unknown.
    @author Jean-Marc Valin
    @version 1.0
*/
class ParameterException : public BaseException {

public:
   /**The constructor with the parameters*/
   ParameterException(std::string _message, std::string _param_name, ParameterSet _params)
      : param_name(_param_name)
      , params(_params)
      , message(_message)
   {}   
   /**The print method*/
   virtual void print(std::ostream &out = std::cerr) 
   {
      out << message << ": "<< param_name <<std::endl;
      out << "Given parameters are:\n";
      params.print(out);
   }
protected:
   /**the parameter name*/
   std::string param_name;
   /**the parameter set*/
   ParameterSet params;
   /**The error message*/
   std::string message;
};

}//end namespace FD
#endif
