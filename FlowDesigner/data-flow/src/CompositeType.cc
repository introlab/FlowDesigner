// Copyright (C) 2001 Jean-Marc Valin

#include "CompositeType.h"
#include <iostream>
#include "ObjectParser.h"

void CompositeType::printOn(ostream &out) const
{
   out << "<CompositeType \n";
   map_type::const_iterator it = fields.begin();
   while (it != fields.end())
   {
      out << "<" << it->first << " " << it->second << " >\n";
      it++;
   }
   out << " > " << endl;
}

void CompositeType::readFrom(istream &in)
{
}
