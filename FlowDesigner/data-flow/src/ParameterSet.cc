// Copyright (C) 1999 Jean-Marc Valin

#include "Node.h"
#include "ParameterSet.h"

//@implements core

bool ParameterSet::exist(const string &param) const
{
   if (find(param)!=end())
   {
      (const_cast <ParameterSet *> (this))->find(param)->second.second = true;
      return true;
   }
   return false;
}

ObjectRef ParameterSet::get(string param) const
{
   if (find(param)==end())
      throw new ParameterException("Missing Parameter", param,*this);
   //else return operator[](param);
   else 
      {
         (const_cast <ParameterSet *> (this))->find(param)->second.second = true;
         return find(param)->second.first;
      }
}

ObjectRef ParameterSet::getDefault(string param, ObjectRef value) 
{
   if (find(param)==end()) 
      return value;
   else 
      {
         (const_cast <ParameterSet *> (this))->find(param)->second.second = true;
         return operator[](param).first;
      }
}

void ParameterSet::defaultParam(string param, ObjectRef value)
{
   if (find(param)==end())
      (operator[](param))=pair<ObjectRef,bool> (value,false);
}

void ParameterSet::add(string param, ObjectRef value)
{
   //cerr<<"adding parameter : "<<param<<endl;
   (operator[](param))=pair<ObjectRef,bool> (value,false);
}

void ParameterSet::print (ostream &out) const
{
   for (ParameterSet::const_iterator it=begin(); it!=end();it++)
      out << it->first << " -> " << it->second.first << endl;
}


void ParameterSet::checkUnused() const
{
   for (ParameterSet::const_iterator it=begin(); it != end();it++) {

      if (!it->second.second)
      {   
         throw new ParameterException("Unused (unknown) parameter", it->first,*this);
      }
   }
}
