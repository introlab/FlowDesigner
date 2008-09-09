// Copyright (C) 1999 Jean-Marc Valin
#ifndef GMM_H
#define GMM_H
#include "gaussian.h"
#include "covariance.h"
#include "Object.h"
#include "ObjectParser.h"
#include "gaussian_set.h"
#include "DiagGMM.h"

namespace FD {

class GMM;

/**The result of scoring a frame against a GMM*/
class Score {
public:
   /**The log score*/
   float score;

   /**The ID of the nearest gaussian in the GMM used*/
   int gaussian_id;

   /**Pointer to the frame scored*/
   float * frame;

   /**Pointer to the GMM used when scoring*/
   const GMM *gmm;
public:

   /**Friend of GMM class*/
   friend class GMM;
};

/**Gaussian Mixture Model (GMM) class*/
class GMM : public Object{
public:

   typedef enum {real, accum} GMM_Mode;
protected:
   
   /**STL vector containing all the gaussians in the GMM*/
   //vector<Gaussian *>  gaussians;
   std::vector<RCPtr<Gaussian> >  gaussians;

   /**STL vector containing all the apriori weights of the gaussians*/
   std::vector<float>       apriori;

   /**Number of gaussians in the GMM*/
   size_t                 nb_gaussians;

   /**Whether of not the GMM trained (like real/accum mode) (GMM_Mode)*/
   int                 mode;

   /**Number of frames aligned to (used to train) the GMM*/
   int                 nb_frames_aligned;

   /**Number of dimensions*/
   size_t dimensions;

   /**Was the gaussian loaded using indexes for mean*/
   bool using_gaussianIDs;

   /**STL vector containing all the gaussian IDs in the GMM*/
   std::vector<int>  gaussianIDs;

public:
   /**Construct a GMM with nb_gauss gaussians, dim dimensions and a
      covariance pseudo-factory*/
   GMM(size_t nb_gauss, size_t dim, Covariance *(*cov_new)(int)) 
      : gaussians(std::vector<RCPtr<Gaussian> >(nb_gauss))
      //: gaussians(std::vector<Gaussian *>(nb_gauss,(Gaussian *)NULL))
      , apriori (std::vector<float>(nb_gauss,0.0)) 
      , nb_gaussians (nb_gauss)
      , mode(accum)
      , nb_frames_aligned(0)
      , dimensions (dim)
      , using_gaussianIDs(false)
   {
      for (size_t i=0;i<nb_gauss;i++)
         gaussians[i] = RCPtr<Gaussian> (new Gaussian (dim, cov_new));
   }

   GMM ()
   //: gaussians(std::vector<Gaussian *>(1,(Gaussian *)NULL))
      : gaussians(std::vector<RCPtr<Gaussian> >())
      , nb_gaussians (0)
      , mode(accum)
      , nb_frames_aligned(0)
      , dimensions(1)
      , using_gaussianIDs(false)
   {
      //gaussians[0] = new Gaussian (1, NewDiagonalCovariance);
   }

   void save(std::string file);

   /**Returns the number of gaussians in the GMM*/
   size_t get_nb_gaussians() const {return nb_gaussians;}

   /**Returns the i'th gaussian*/
   Gaussian &gaussian (int i) const {return *(gaussians[i]);}

   /**Accumulates (adds) the frame to the i'th gaussian*/
   void accum_to_gaussian(size_t i, const float * fr)
   {
      gaussians[i]->accum_frame(fr);
      apriori[i]+=1.0;
      nb_frames_aligned++;
   }

   /**Randomly init the GMM with a list (STL vector) of frames*/
   void init(std::vector<float * > frames);

   /**Performs k-means training*/
   void kmeans1(std::vector<float * > frames, int nb_iterations = 1);
   
   /**splits the largest gaussian in two*/
   void split1();

   /**Performs k-means training (using another GMM to score)*/
   void kmeans2(std::vector<float * > frames, GMM *gmm);

   /**Perform MAP adaptation (using another GMM to score)*/
   void adaptMAP(std::vector<float * > frame, GMM *gmm);

   /**Converts the GMM from accum mode to real mode*/
   void to_real();

   /**Converts the GMM from real mode to accum mode and set everything to zero*/
   void reset_to_accum_mode();

   /**Score a frame against the GMM without using the covariances 
      (nearest euclidian distance)*/
   Score minDistance(float * fr, Covariance *cov) const;

   /**Score a frame against the GMM*/
   Score score(float * fr) const;

   /**Double the number of gaussians*/
   void binary_split();

   /**Score a list (STL vector) of frames against the GMM 
      without using the covariances (nearest euclidian distance)*/
   std::vector<Score> minDistance(std::vector <float *> fr) const;

   /**Score a list (STL vector) of frames against the GMM*/
   std::vector<Score> score(std::vector <float *> fr) const;

   void toIDsUsing (GaussianSet & gauss);

   void toPtrsUsing (const GaussianSet & gauss);

   /**Creates a DiagGMM object from a GMM*/
   DiagGMM *createDiagGMM();

   /** print function used for operator << */
   virtual void printOn(std::ostream &out=std::cout) const;

   /**Read function used for operator >> */
   void readFrom (std::istream &in=std::cin);

   //friend std::ostream &operator << (std::ostream &out, const GMM &gmm);
   /**extractor for GMM*/
   friend std::istream &operator >> (std::istream &in, GMM &gmm);
}
;

//ostream &operator << (ostream &out, const GMM &gmm);
std::istream &operator >> (std::istream &in, GMM &gmm);

}//namespace FD

#endif
