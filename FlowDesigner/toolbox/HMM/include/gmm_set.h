// Copyright (C) 1998-1999  Jean-Marc Valin

#ifndef GMM_SET_H
#define GMM_SET_H

#include "gmm.h"
#include "gaussian_set.h"
#include "Object.h"

namespace FD {

class GMMSet : public Object {
protected:
   int nb_gmms;
   Vector<RCPtr<GMM> > gmms;
public:
   /**Default Constructor*/
   GMMSet() 
   : nb_gmms(0)
   {};

   /**Returns the id corresponding to the pointer (or add if absent)*/
   int getIDFor(RCPtr<GMM>);

   /**Returns the pointer corresponding to the id*/
   RCPtr<GMM> getPtrFor(int id);

   /**convert all elements to using IDs (before saving)*/
   void toIDs(GaussianSet & gauss);
 
   /**convert all elements to using Ptrs (after loading)*/
   void toPtrs(const GaussianSet & gauss) const;

   /** print function used for operator << */
   virtual void printOn(std::ostream &out=std::cout) const;

   /**Read function used for operator >> */
   void readFrom (std::istream &in=std::cin);

   /**extractor operator*/
   friend std::istream &operator >> (std::istream &in, GMMSet &cov);

};
}//namespace FD

#endif
