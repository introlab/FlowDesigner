// Copyright (C) 1998-1999  Jean-Marc Valin
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

#ifndef MEAN_SET_H
#define MEAN_SET_H

#include "Vector.h"
#include "Object.h"
#include "mean.h"

class MeanSet : public Object {
protected:
   int nb_means;
   //Vector<Ptr<Vector<float> > > means;
   Vector<Ptr<Mean> > means;
public:
   /**Default Constructor*/
   MeanSet() 
   : nb_means(0)
   {};

   /**Indexing operator, returns the Ptr to the specified ID*/
   Ptr<Mean> operator [] (int id) {return means[id];}

   /**Returns the number of elements*/
   int size() {return nb_means;}

   /**Returns the id corresponding to the pointer (or add if absent)*/
   int getIDFor(Ptr<Mean>);

   /**Returns the pointer corresponding to the id*/
   Ptr<Mean> getPtrFor(int id) const;

   /** print function used for operator << */
   virtual void printOn(ostream &out=cout) const;

   /**Read function used for operator >> */
   void readFrom (istream &in=cin);

   /**extractor operator*/
   friend istream &operator >> (istream &in, MeanSet &cov);

};

#endif
