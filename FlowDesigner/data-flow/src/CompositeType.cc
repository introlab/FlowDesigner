// Copyright (C) 2001 Jean-Marc Valin

#include "CompositeType.h"
#include <iostream>
#include "ObjectParser.h"

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
   //cerr << "FFNet::readFrom\n";
   while (1)
   {
      char ch;
      in >> ch;
      if (ch == '>') break;
      else if (ch != '<') 
       throw new ParsingException ("Parse error: '<' expected");
      in >> tag;
      ObjectRef value;
      in >> value;
      addField(tag, value);
      if (!in) throw new ParsingException ("Parse error trying to build " + tag);
      in >> tag;
      if (tag != ">") 
         throw new ParsingException ("Parse error: '>' expected ");
   }

}
