// Copyright (C) 1998-1999  Jean-Marc Valin

#ifndef MEAN_SET_H
#define MEAN_SET_H

#include "Vector.h"
#include "Object.h"
#include "mean.h"

class MeanSet : public Object {
protected:
   int nb_means;
   //Vector<RCPtr<Vector<float> > > means;
   Vector<RCPtr<Mean> > means;
public:
   /**Default Constructor*/
   MeanSet() 
   : nb_means(0)
   {};

   /**Indexing operator, returns the Ptr to the specified ID*/
   RCPtr<Mean> operator [] (int id) {return means[id];}

   /**Returns the number of elements*/
   int size() {return nb_means;}

   /**Returns the id corresponding to the pointer (or add if absent)*/
   int getIDFor(RCPtr<Mean>);

   /**Returns the pointer corresponding to the id*/
   RCPtr<Mean> getPtrFor(int id) const;

   /** print function used for operator << */
   virtual void printOn(std::ostream &out=std::cout) const;

   /**Read function used for operator >> */
   void readFrom (std::istream &in=std::cin);

   /**extractor operator*/
   friend std::istream &operator >> (std::istream &in, MeanSet &cov);

};

#endif
