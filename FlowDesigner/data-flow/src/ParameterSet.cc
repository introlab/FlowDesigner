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

#include "Node.h"
#include "ParameterSet.h"

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
      throw ParameterException("Missing Parameter", param,*this);
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
      out << it->first << " -> " << typeid(*(it->second.first)).name() << endl;
}


void ParameterSet::checkUnused() const
{
   for (ParameterSet::const_iterator it=begin(); it != end();it++) {

      if (!it->second.second)
      {   
         throw ParameterException("Unused (unknown) parameter", it->first,*this);
      }
   }
}
