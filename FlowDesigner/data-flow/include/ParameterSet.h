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

#ifndef PARAMETERSET_H
#define PARAMETERSET_H

#include <map>
#include <string>
#include "ObjectRef.h"
#include "Exception.h"

/** A ParameterSet is a data structure that holds all the parameters 
    needed for the construction of a new node.
    @author Jean-Marc Valin
    @version 1.0
*/
class ParameterSet : public map<string,pair<ObjectRef,bool> > {
public:
   /**Does a certain parameter exist?*/
   bool exist(const string &param) const;

   /**get a parameter's value*/
   ObjectRef get(string param) const;

   /**get the default parameter*/
   ObjectRef getDefault(string param, ObjectRef value);

   /**set the default parameter*/
   void defaultParam(string param, ObjectRef value);

   /**adding the parameters*/
   void add(string param, ObjectRef value);

   /**printing the parameters*/
   void print(ostream &out = cerr) const;

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
   ParameterException(string _message, string _param_name, ParameterSet _params)
      : message(_message)
      , param_name(_param_name)
      , params(_params)
   {}   
   /**The print method*/
   virtual void print(ostream &out = cerr) 
   {
      out << message << ": "<< param_name <<endl;
      out << "Given parameters are:\n";
      params.print(out);
   }
protected:
   /**the parameter name*/
   string param_name;
   /**the parameter set*/
   ParameterSet params;
   /**The error message*/
   string message;
};

#endif
