// Copyright (C) 2002 Jean-Marc Valin

#include "CompositeType.h"
#include <iostream>
#include "ObjectParser.h"

using namespace std;

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
