// Copyright (C) 1999 Jean-Marc Valin

#include "mean.h"

DECLARE_TYPE(Mean)
//@implements GMM


void Mean::printOn (ostream &out) const
{
   out << "<Mean " << endl;
   out << "<dimension " << dimension << "> ";
   out << "<mode " << mode << "> ";
   if (mode == accum)
      out << "<accum_count " << accum_count << "> ";
   out << endl;
   out << "<data";
   for (int i=0;i<dimension;i++)
      out << " " << operator[] (i);
   out << "> >\n";
}

void Mean::readFrom (istream &in)
{
   dimension=-1;
   string tag;
   while (1)
   {
      char ch;
      in >> ch;
      if (ch == '>') break;
      in >> tag;
      if (tag == "dimension")
      { 
         in >> dimension;
         resize(dimension);
      }      
      else if (tag == "mode")
         in >> mode;
      else if (tag == "accum_count")
         in >> accum_count;
      else if (tag == "data")
      {
         if (dimension==-1)
            throw new ParsingException("Mean::readFrom : dimension must be specified before data");
         for (int i=0;i<dimension;i++)
            in >> operator[] (i);
      } else 
         throw new ParsingException ("Mean::readFrom : unknown argument: " + tag);
      if (!in) throw new ParsingException ("Mean::readFrom : Parse error trying to build " + tag);
      in >> tag;
      if (tag != ">") throw new ParsingException ("Mean::readFrom : Parse error: '>' expected ");
   }
}

istream &operator >> (istream &in, Mean &gauss)
{
   if (!isValidType(in, "Mean")) return in;
   
   gauss.readFrom(in);

   return in;
}

