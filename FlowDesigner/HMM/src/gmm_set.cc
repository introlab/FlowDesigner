// Copyright (C) 1998-1999 Jean-Marc Valin

#include "gmm_set.h"
#include "ObjectParser.h"

DECLARE_TYPE(GMMSet)
//@implements GMM

int GMMSet::getIDFor(RCPtr<GMM> cov)
{
   for (int i=0;i<nb_gmms;i++)
   {
      if (cov.get()==gmms[i].get())
         return i;
   }
   nb_gmms++;
   gmms.resize(nb_gmms);
   gmms[nb_gmms-1]=cov;
   return nb_gmms-1;
}

RCPtr<GMM> GMMSet::getPtrFor(int id)
{
   if (id>=nb_gmms)
      throw new GeneralException("Invalid gmm ID", __FILE__, __LINE__);
   return gmms[id];
}

void GMMSet::toIDs(GaussianSet & gauss)
{
   for (int i=0;i<nb_gmms;i++)
   {
      gmms[i]->toIDsUsing(gauss);
   }
}

void GMMSet::toPtrs(const GaussianSet & gauss) const
{
   for (int i=0;i<nb_gmms;i++)
   {
      gmms[i]->toPtrsUsing(gauss);
   }
}

void GMMSet::printOn(ostream &out) const
{
   out << "<GMMSet " << endl;
   out << "<gmms " << gmms << ">" << endl;
   out << "<nb_gmms " << nb_gmms << ">" << endl;
   out << ">\n";
}

void GMMSet::readFrom (istream &in)
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
      if (tag == "gmms")
         in >> gmms;
      else if (tag == "nb_gmms")
         in >> nb_gmms;
      else
         throw new ParsingException ("unknown argument: " + tag);

      if (!in) throw new ParsingException ("Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw new ParsingException ("Parse error: '>' expected ");
   }
}

istream &operator >> (istream &in, GMMSet &cov)
{
   if (!isValidType(in, "GMMSet")) return in;
   cov.readFrom(in);
   return in;
}
