// Copyright (C) 1998-1999 Jean-Marc Valin

#include "mean_set.h"
#include "ObjectParser.h"

DECLARE_TYPE(MeanSet)
//@implements GMM

int MeanSet::getIDFor(RCPtr<Mean> cov)
{
   for (int i=0;i<nb_means;i++)
   {
      if (cov.get()==means[i].get())
         return i;
   }
   nb_means++;
   means.resize(nb_means);
   means[nb_means-1]=cov;
   return nb_means-1;
}

RCPtr<Mean> MeanSet::getPtrFor(int id) const
{
   if (id>=nb_means)
      throw new GeneralException("Invalid mean ID", __FILE__, __LINE__);
   return means[id];
}


void MeanSet::printOn(ostream &out) const
{
   out << "<MeanSet " << endl;
   out << "<means " << means << ">" << endl;
   out << "<nb_means " << nb_means << ">" << endl;
   out << ">\n";
}

void MeanSet::readFrom (istream &in)
{
   string tag;

   while (1)
   {
      char ch;
      in >> ch;
      if (ch == '>') break;
      else if (ch != '<') 
       throw new ParsingException ("Parse error: '<' expected");
      in >> tag;
      if (tag == "means")
         in >> means;
      else if (tag == "nb_means")
         in >> nb_means;
      else
         throw new ParsingException ("unknown argument: " + tag);

      if (!in) throw new ParsingException ("Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw new ParsingException ("Parse error: '>' expected ");
   }
}

istream &operator >> (istream &in, MeanSet &cov)
{
   if (!isValidType(in, "MeanSet")) return in;
   cov.readFrom(in);
   return in;
}
