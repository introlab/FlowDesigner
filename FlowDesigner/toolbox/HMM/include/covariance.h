// Copyright (C) 1998-1999  Jean-Marc Valin
#ifndef COVARIANCE_H
#define COVARIANCE_H

#include <math.h>
#include <vector>
#include "Object.h"
#include "ObjectParser.h"
#include "misc.h"
#include "Vector.h"
#include "mean.h"
#include <fstream>

namespace FD {

class GMM; 

/**Abstract covariance class*/
class Covariance : public Object
{
protected:
   enum Mode {accum, real, rotated, inverted};
   
   /**Size of the covariance matrix*/
   size_t   dimension;
   
   /**Log of the determinant*/
   mutable double determinant;
   
   /**Whether or not the determinant has been computed*/
   mutable bool  determinant_is_valid;
   
   /**Mode*/
   int mode;

   /**Number of frames accumulated*/
   int accum_count;
public:
   /**Create a Covariance with dim dimensions*/
   Covariance(int dim) : dimension(dim) , determinant(-10000) , determinant_is_valid(false), mode(accum)
   {
      
   }
   /**Copy constructor*/
   Covariance(const Covariance &cov) 
      : dimension(cov.dimension) 
      , determinant(0)
      , determinant_is_valid(false)
      , mode (cov.mode)
      , accum_count(0)
   {}
   /**Virtual Destructor*/
   virtual ~Covariance() {}
   
   /**accumulates a frame to the covariance*/
   virtual void accumFrame(const float *v)=0;

   /**accumulates a frame to the covariance*/
   virtual void accumFrame(const std::vector<float> &v)=0;
  
   /**Returns the covariance size (dimension)*/
   unsigned int   size() const { return dimension; }

   /**Returns (and compute if necessary) the covariance log determinant*/
   double getDeterminant() const 
   { 
      if (!determinant_is_valid) compute_determinant(); 
      return determinant; 
   }

   /**Computes the determinant*/
   virtual void compute_determinant() const =0;

   /**Prints the covariance*/
   virtual void printOn(std::ostream &out=std::cout) const = 0;
   
   /**Computed the mahalanobis distance between the vectors using the 
      covariance*/
   virtual double mahalanobisDistance(const float *x1, const double *x2) const =0;

   /**Virtual indexing operator 1D (for diagonal covariance)*/
   virtual double&      operator[](size_t )=0;

   /**Virtual indexing operator 2D*/
   virtual double&      operator()(size_t,size_t)=0;

   /**Resets accumulation to zero*/
   virtual void reset()=0;
   
   /**Returns a copy of the covariance*/
   virtual Covariance * copy()=0;

   /**Converts from accumulate mode to real*/
   virtual void invert()=0;

   /**Substract mean^2, before the covariance can be inverted*/
   virtual void processMean(RCPtr<Mean> mean)=0;

   friend class GMM;
};


/**Diagonal Covariance class*/
class DiagonalCovariance : public Covariance  {
   /**The covariance data as the diagonal vector*/
   std::vector<double> data;
public:
   DiagonalCovariance(std::istream &in)
      : Covariance(0)
   {in >> *this;}
   
   DiagonalCovariance() 
      : Covariance(0)
      , data(std::vector<double>(0,0.0))
   {}

   /**Constructs a Diagonal Covariance with dimension dim*/
   DiagonalCovariance(int dim) 
      : Covariance(dim) 
      , data(std::vector<double>(dim,0.0)) 
   {}

   /**Copy Constructor*/
   DiagonalCovariance (const DiagonalCovariance &cov) 
      : Covariance(cov)
      , data(cov.data)
   {}

   /**returns the mahalanobis distance between x1 and x2 using the covariance*/
   double mahalanobisDistance(const float *x1, const double *x2) const;

   /**virtual, should not be used*/
   double&      operator[](size_t i) {return data[i];}

   /**virtual, should not be used*/
   double&      operator()(size_t i) {return data[i];}

   /**virtual, should not be used*/
   double&      operator()(size_t i,size_t) {return data[i];}

   /**Computes the determinant*/
   void compute_determinant() const;

   
   /**accumulates a frame to the covariance*/
   void accumFrame(const float *v)
   {
      for (size_t i=0;i<dimension;i++)
         data[i] += v[i]*v[i];
      accum_count++;
   }

   /**accumulates a frame to the covariance*/
   void accumFrame(const std::vector<float> &v)
   {
      for (size_t i=0;i<dimension;i++)
         data[i] += v[i]*v[i];
      accum_count++;
   }

   /**Reset accumulation to zero*/
   void reset();

   /**Returns a copy of the covariance*/
   Covariance *copy () {  return new DiagonalCovariance (*this); }

   /**Converts from accumulate mode to real*/
   void invert();

   /**Substract mean^2, before the covariance can be inverted*/
   void processMean(RCPtr<Mean> mean);

   /** print function used for operator << */
   virtual void printOn(std::ostream &out=std::cout) const;

   /**Read function used for operator >> */
   void readFrom (std::istream &in=std::cin);

   /**extractor operator*/
   friend std::istream &operator >> (std::istream &in, DiagonalCovariance &cov);
   friend class GMM;
}
;

/**Function that acts as a factory for diagonal covariance*/
inline Covariance *NewDiagonalCovariance(int dim) {return new DiagonalCovariance (dim);}

std::istream &operator >> (std::istream &in, DiagonalCovariance &cov);
}//namespace FD

#endif
