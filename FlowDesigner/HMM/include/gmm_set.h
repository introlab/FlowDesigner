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

#ifndef GMM_SET_H
#define GMM_SET_H

#include "gmm.h"
#include "gaussian_set.h"

#include "Object.h"

class GMMSet : public Object {
protected:
   int nb_gmms;
   Vector<Ptr<GMM> > gmms;
public:
   /**Default Constructor*/
   GMMSet() 
   : nb_gmms(0)
   {};

   /**Returns the id corresponding to the pointer (or add if absent)*/
   int getIDFor(Ptr<GMM>);

   /**Returns the pointer corresponding to the id*/
   Ptr<GMM> getPtrFor(int id);

   /**convert all elements to using IDs (before saving)*/
   void toIDs(GaussianSet & gauss);
 
   /**convert all elements to using Ptrs (after loading)*/
   void toPtrs(const GaussianSet & gauss) const;

   /** print function used for operator << */
   virtual void printOn(ostream &out=cout) const;

   /**Read function used for operator >> */
   void readFrom (istream &in=cin);

   /**extractor operator*/
   friend istream &operator >> (istream &in, GMMSet &cov);

};

#endif
