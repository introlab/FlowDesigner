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

ostream &operator << (ostream &out, const Gaussian &gauss)
{
   out << "<GAUSSIAN " << endl;
   out << gauss.dimension << " " << gauss.accum_count << endl;
   out << *gauss.mean; 
   out << *gauss.covariance;
   out << ">\n";
   return out;
}

istream &operator >> (istream &in, Gaussian &gauss)
{
   string type;
   in >> type;
   cerr << "(type: " << type << ")" << endl;
   in >> gauss.dimension;
   in >> gauss.accum_count;
   vector<float> *tmp = new vector<float>;
   in >> *tmp;
   gauss.mean = tmp;
   string end;
   in >> end;
   cerr << "terminator: " << end << endl;
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
