// Copyright (C) 2002 Jean-Marc Valin

#include "CompositeType.h"
#include <iostream>
#include "ObjectParser.h"
#include "binio.h"
using namespace std;

namespace FD {

DECLARE_TYPE(CompositeType)

void CompositeType::printOn(ostream &out) const
{
   out << "<CompositeType";
   map_type::const_iterator it = fields.begin();
   while (it != fields.end())
   {
      out << "\n<" << it->first << " " << it->second << " >";
      it++;
   }
   out << " >" << endl;
}

void CompositeType::readFrom(istream &in)
{
   string tag;
   while (1)
   {
      char ch;
      in >> ch;
      if (ch == '>') break;
      else if (ch != '<') 
       throw new ParsingException ("CompositeType::readFrom : Parse error: '<' expected");
      in >> tag;
      ObjectRef value;
      in >> value;
      addField(tag, value);
      if (!in) throw new ParsingException ("CompositeType::readFrom : Parse error trying to build " + tag);
      in >> tag;
      if (tag != ">") 
         throw new ParsingException ("CompositeType::readFrom : Parse error: '>' expected ");
   }

}


void CompositeType::serialize(std::ostream &out) const
{
	 //Write header
	 out << "{" << "CompositeType" << std::endl;
	 out << "|";
	 
	 //Write size
	 int tmp = fields.size();
	 BinIO::write(out, &tmp, 1);
	 
	 //Serialize all internal objects
	 for (CompositeType::map_type::const_iterator iter = fields.begin(); iter != fields.end(); iter++)
	 {
		 //Key
		 iter->first.serialize(out);
		 
		 //Value
		 iter->second->serialize(out);
	 }
	 
	 out << "}";
}

void CompositeType::unserialize(std::istream &in) 
{
	 int size; 
	 BinIO::read(in, &size, 1);
	
	 for (int i = 0; i < size; i++)
	 {
		 ObjectRef key;
		 ObjectRef value;
		 in >> key >> value;
		 RCPtr<String> keyPtr = key;
		 addField(*keyPtr,value);
	 }
	 //Should get the last "}"	
	 char ch;
	 in >> ch;
}



void CompositeType::conservativeAddField(const std::string &name, ObjectRef obj)
{
   fields.insert(map_element_type(name,obj));
}


}//namespace FD
