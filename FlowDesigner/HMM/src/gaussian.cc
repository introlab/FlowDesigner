// Copyright (C) 1998-1999  Jean-Marc Valin & Daniel Kiecza
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


#include "covariance.h"
#include "gaussian.h"
#include <assert.h>
#include <iostream.h>
#include <fstream.h>
#include <iostream.h>

void Gaussian::print_mean(ostream &out , string separ ) const
{
   for (unsigned int i=0;i<mean->size();i++)
   {
      out << (*mean)[i] << separ;
   }
   out << endl;
}
void Gaussian::print_covar(ostream &out , string separ ) const
{
   for (unsigned int i=0;i<covariance->size();i++)
   {
      out << (*covariance)[i] << separ;
   }
   out <<  endl;
}


void Gaussian::to_real()
{
   unsigned int i;
   float accum_1 = 1/(float(accum_count));
   for( i = 0; i < mean->size(); i++ )
   {
      //cerr << (*mean)[i] << " ";
      (*mean)[i] *= accum_1;
   }
#ifdef DEBUG
   cerr << "accum_1: " << accum_1 <<endl;
#endif
   covariance->to_real(accum_1 , mean);
}


Gaussian::~Gaussian()
{
   if (mean != 0)
  {
    delete mean;
    mean       = 0;
  }
  
  if (covariance != 0)
  {
    delete covariance;
    covariance = 0;
  }
}


void Gaussian::printOn (ostream &out) const
{
   out << "<Gaussian " << endl;
   out << "<dimension " << dimension << "> ";
   out << "<accum_count " << accum_count << "> " << endl;
   out << "<mean " << *mean << ">" << endl; 
   out << "<covariance " << *covariance << ">" << endl;
   out << ">\n";
}


istream &operator >> (istream &in, Gaussian &gauss)
{
   if (!isValidType(in, "Gaussian")) return in;
   string tag;
   while (1)
   {
      char ch;
      in >> ch;
      if (ch == '>') break;
      in >> tag;
      if (tag == "dimension") 
         in >> gauss.dimension;
      else if (tag == "accum_count")
         in >> gauss.accum_count;
      else if (tag == "mean")
      {
         vector<float> *tmp = new vector<float>;
         in >> *tmp;
         gauss.mean = tmp;
      }
      else if (tag == "covariance")
      {
         //DiagonalCovariance *cov = new DiagonalCovariance;
         //in >> *cov;
         //gauss.covariance = cov;
         gauss.covariance = new DiagonalCovariance (in);
      } else 
         throw ParsingException ("unknown argument: " + tag);
      if (!in) throw ParsingException ("Parse error trying to build " + tag);
      in >> tag;
      if (tag != ">") throw ParsingException ("Parse error: '>' expected ");
   }
   

   string end;
   //in >> end;
   //cerr << "gauss terminator: " << end << endl;
   return in;
}

/*
void Gaussian::print() const
{
  if (dimension != 0)
  {
    cout << "==============================" << endl;
    cout << "Dimension: " << dimension << endl;

    cout << "Mean: " << endl;
    for (int i=0; i<dimension; i++)
      cout << mean[i] << ", ";
    cout << endl;
    
    cout << "Covariance: " << endl;
    covariance->print();
    cout << "==============================" << endl;
  }
}

*/
