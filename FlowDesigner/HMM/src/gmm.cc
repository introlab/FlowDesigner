// Copyright (C) 1998-1999 Jean-Marc Valin
#include "gmm.h"
#include <typeinfo>
#include <vector>
#include "vec.h"

#ifdef HAVE_VALUES_H
#include <values.h>
#endif

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif

using namespace std;
using namespace FD;

DECLARE_TYPE(GMM)
//@implements GMM
//@require DGMM

void GMM::init(vector<float *> frames)
{
   for (unsigned int i=0;i<frames.size();i++)
   {
      float * fr = frames[i];
      int gaus = rand()%nb_gaussians;
#ifdef DEBUG
      cerr << "going to: " << gaus << endl;
#endif
      accum_to_gaussian(gaus,fr);
   }
   to_real();
}

void GMM::reset_to_accum_mode()
{
   nb_frames_aligned=0;
   for(int i=0;i<nb_gaussians;i++)
   {
      gaussians[i]->reset_to_accum_mode();
      apriori[i]=0;
   }
   mode = accum;
}

void GMM::kmeans2(vector<float *> frames, GMM *gmm)
{
   vector<Score> scores;
   scores = gmm->minDistance(frames);
   //scores = gmm->score(frames);
   reset_to_accum_mode();
   unsigned int i;
   for (i=0;i<frames.size();i++)
   {
#ifdef DEBUG
      cerr << "going to: " << scores[i].gaussian_id << " score: " << scores[i].score << endl;
#endif
      accum_to_gaussian(scores[i].gaussian_id,(frames[i]));
   }
   for (i=0;i<nb_gaussians;i++)
      if (gaussians[i]->get_accum_count()==0)
      {
         //cerr << "accum zero\n";
         accum_to_gaussian(i, (frames[rand()%frames.size()]));
      }
   to_real();
}

void GMM::adaptMAP(vector<float *> frames, GMM *gmm)
{
   /*dimensions = gmm->dimensions;
   apriori = gmm->apriori; //(vector<float>(nb_gauss,0.0)) 
   nb_gaussians = gmm->nb_gaussians;
   mode = real;
   nb_frames_aligned=0;
   using_gaussianIDs = false;
   //for (int i=0;i<nb_gaussians;i++)
   //   gaussians.push_back(RCPtr<Gaussian> (new Gaussian(dimensions, NewDiagonalCovariance)));
   */
   

   vector<Score> scores;
   //scores = gmm->minDistance(frames);
   scores = gmm->score(frames);
   //reset_to_accum_mode();
   
   for (int i=0;i<nb_gaussians;i++)
   {
      int adaptCount=0;
      vector<float> adaptMean(dimensions, 0);
      for (int j=0;j<frames.size();j++)
      {
	 if (scores[j].gaussian_id == i)
	 {
	    for (int k=0;k<dimensions;k++)
	       adaptMean[k] += frames[j][k];
	    adaptCount++;
	 }
      }
      if (adaptCount)
      {
	 for (int j=0;j<dimensions;j++)
	    adaptMean[j] /= adaptCount;
      } else {
         cerr << "no data for gaussian " << i << endl;
      }
      
      //FIXME: This is not a correct MAP implementation
      //float weight = float(adaptCount)/30.0;
#if 0
      float weight = 1 - exp(float(-adaptCount)/20);
      if (weight > 1)
	 weight = 1;
#else
      float weight = float(adaptCount)/(float(adaptCount)+15);
#endif
      //weight=0;
      Vector<double> &mean = *gaussians[i]->mean;
      for (int j=0;j<mean.size();j++)
	 mean[j] = weight*adaptMean[j] + (1-weight)*mean[j];
      //gaussians[i+old_size]= RCPtr<Gaussian> (new Gaussian(*(gaussians[i])));
      //gaussians[i].
      //perform adaptation
   }
}

void GMM::kmeans1(vector<float *> frames, int nb_iterations)
{
   for (int i=0;i<nb_iterations;i++)
      kmeans2(frames,this);
}

void GMM::split1()
{
   int max_gauss=0, max_accum=gaussians[0]->get_accum_count();
   gaussians.resize(nb_gaussians+1);
   apriori.resize(nb_gaussians+1);
   for (int i=1;i<nb_gaussians;i++)
   {
      int accum=gaussians[i]->get_accum_count();
      if (accum>max_accum)
      {
         max_gauss=i;
         max_accum=accum;
      }
   }
   //cout << "spliting " << max_gauss << endl;
   gaussians[nb_gaussians]= RCPtr<Gaussian> (new Gaussian(*(gaussians[max_gauss])));
   Vector <double> &mean = gaussians[nb_gaussians]->getMean();
   for (unsigned int j=0;j<mean.size();j++)
      mean[j]+=.00001*(rand()%100-49.5);
   ++nb_gaussians;
}

void GMM::binary_split()
{
   int old_size=nb_gaussians;
   nb_gaussians *= 2;
   gaussians.resize(nb_gaussians);
   apriori.resize(nb_gaussians);
   for (int i=0;i<old_size;i++)
   {
      gaussians[i+old_size]= RCPtr<Gaussian> (new Gaussian(*(gaussians[i])));
      Vector <double> &mean = gaussians[i+old_size]->getMean();
      for (unsigned int j=0;j<mean.size();j++)
      {
         mean[j]*=1+.0002*(rand()%100-49.5);
         //mean[j]+=.0001;
      }
   }
}

void GMM::to_real()
{
   if (mode==real) return;
   for (int i=0;i<nb_gaussians;i++)
   {
      apriori[i]=log(apriori[i]/nb_frames_aligned);
      gaussians[i]->to_real();
   }


   for (int j=0;j<nb_gaussians;j++)
   {
      Gaussian &gauss = *gaussians[j];
      Covariance &_cov = *gauss.covariance;
      DiagonalCovariance *cov = dynamic_cast<DiagonalCovariance*> (&_cov);
      if (!cov)
	 throw new GeneralException("Covariance not diagonal in GMM::createDiagGMM()", 
				    __FILE__, __LINE__);
      /* Covariance normalization 
      float norm = 0;
      for (int i=0;i<dimensions;i++)
	 norm += .5*log((*cov).data[i]);
      apriori[j] += norm;
      */
   }


   mode = real;
}

vector<Score> GMM::minDistance(vector <float * > fr) const
{
   unsigned int i,j;
   Covariance *cov = new DiagonalCovariance (dimensions);
   for (j=0;j<dimensions;j++)
   {
      (*cov)[j]=0;
   }
   if (1) {
      int cov_count = 0;
   for (i=0;i<nb_gaussians;i++)
      for (j=0;j<dimensions;j++)
      {
         (*cov)[j]+=gaussians[i]->get_accum_count()/(*(gaussians[i]->covariance))[j];
         cov_count += gaussians[i]->get_accum_count();
      }
   for (j=0;j<dimensions;j++)
   {
      (*cov)[j]=cov_count/((*cov)[j]);
      //cerr << (*cov)[j] << " ";
   }
   //cerr << endl;
   } 
   cov->mode = (Covariance::inverted);


   vector<Score> scores(fr.size());
   for (i=0;i<fr.size();i++)
   {
      scores[i]=minDistance(fr[i],cov);
   }

   delete cov;
   return scores;
   
}

vector<Score> GMM::score(vector <float * > fr) const
{
   vector<Score> scores(fr.size());
   for (unsigned int i=0;i<fr.size();i++)
   {
      scores[i]=score(fr[i]);
   }
   return scores;
   
}

Score GMM::minDistance(float * fr,Covariance *cov) const
{
   int i,j;
   float min_dist = FLT_MAX ;
   int min_gauss = 0;
   Score frame_score;
   for (j=0;j<nb_gaussians;j++)
   {
      
      //float dist = gaussians[j]->euclidian(fr);
      float dist = gaussians[j]->mahalanobis(fr,cov);
      if (dist < min_dist)
      {
         min_dist=dist;
         min_gauss = j;
      }
      //cerr << "mean " << j << ": " << dist << endl;
   }
   frame_score.score = min_dist;
   frame_score.gaussian_id = min_gauss;
   frame_score.gmm = this;
   frame_score.frame = fr;
   return frame_score;
}

Score GMM::score(float * fr) const
{
   float min_dist = FLT_MAX ;
   int min_gauss = 0;
   Score frame_score;

   for (int j=0;j<nb_gaussians;j++)
   {

/* Covariance normalization 
      Gaussian &gauss = *gaussians[j];
      Covariance &_cov = *gauss.covariance;
      DiagonalCovariance *cov = dynamic_cast<DiagonalCovariance*> (&_cov);
      if (!cov)
	 throw new GeneralException("Covariance not diagonal in GMM::createDiagGMM()", 
				    __FILE__, __LINE__);
      float norm = 0;
      for (int i=0;i<dimensions;i++)
	 norm += .5*log((*cov).data[i]);
      float dist = gaussians[j]->mahalanobis(fr)-apriori[j]-norm;
*/



      float dist = gaussians[j]->mahalanobis(fr)-apriori[j];
      //cerr << "apriori[j]: " << apriori[j] << endl;
      if (dist < min_dist)
      {
         min_dist=dist;
         min_gauss = j;
      }
      //cerr << "mean " << j << ": " << dist << " " << apriori[j] << endl;
   }
   frame_score.score = min_dist;
   frame_score.gaussian_id = min_gauss;
   frame_score.gmm = this;
   frame_score.frame = fr;
   return frame_score;
}

void GMM::toIDsUsing (GaussianSet &gauss)
{
   if (using_gaussianIDs)
      return;
   gaussianIDs.resize(nb_gaussians);
   using_gaussianIDs=true;
   for (int i=0;i<nb_gaussians;i++)
      gaussianIDs[i]=gauss.getIDFor(gaussians[i]);

}

void GMM::toPtrsUsing (const GaussianSet &gauss)
{
   if (!using_gaussianIDs)
      return;
   using_gaussianIDs=false;
   for (int i=0;i<nb_gaussians;i++)
     gaussians[i]=gauss.getPtrFor(gaussianIDs[i]);
}


DiagGMM *GMM::createDiagGMM()
{
   DiagGMM *dg = new DiagGMM;
   dg->dim = dimensions;
   dg->nbGauss = nb_gaussians;
   dg->augDim = (dimensions+4)&0xfffffffc;
   int allocSize = 2 * dg->augDim * dg->nbGauss * sizeof(float)  +  CACHE_LINES;
   //allocSize += dg->augDim * sizeof(float);
   dg->ptr = new char [allocSize];
   dg->base = (float *) (((unsigned long)(dg->ptr) + (CACHE_LINES-1))&CACHE_MASK);
   float *ptr = dg->base;
   for (int k=0;k<nb_gaussians;k++)
   {
      Gaussian &gauss = *gaussians[k];
      Mean &mean = *gauss.mean;
      Covariance &_cov = *gauss.covariance;
      DiagonalCovariance *cov = dynamic_cast<DiagonalCovariance*> (&_cov);
      if (!cov)
	 throw new GeneralException("Covariance not diagonal in GMM::createDiagGMM()", 
				    __FILE__, __LINE__);
      for (int i=0;i<dimensions;i++)
	 ptr[i] = mean[i];
      for (int i=dimensions;i<dg->augDim;i++)
	 ptr[i]=0;
      ptr += dg->augDim;
      float norm = 0;
      for (int i=0;i<dimensions;i++)
      {
	 norm += .5*log((*cov).data[i]);
	 ptr[i] = -(*cov).data[i];
      }
      ptr[dimensions] = apriori[k]+norm;
      for (int i=dimensions+1;i<dg->augDim;i++)
	 ptr[i]=0;
      ptr += dg->augDim;
   }
   return dg;
}


void GMM::printOn(ostream &out) const
{
   out << "<GMM " << endl;
   out << "<nb_gaussians " << nb_gaussians << ">" << endl;
   out << "<mode " << mode << ">" << endl;
   out << "<nb_frames_aligned " << nb_frames_aligned << ">" << endl;
   out << "<dimensions " << dimensions << ">" << endl;
   out << "<apriori " << apriori << ">" << endl;
   if (using_gaussianIDs)
      out << "<gaussianIDs " << gaussianIDs << ">" << endl;
   else
      out << "<gaussians " << gaussians << ">" << endl;
   out << ">\n";
}


void GMM::readFrom (istream &in)
{
   string tag;

   while (1)
   {
      char ch;
      in >> ch;
      if (ch == '>') break;
      else if (ch != '<') 
       throw new ParsingException ("GMM::readFrom : Parse error: '<' expected");
      in >> tag;
      if (tag == "nb_gaussians")
         in >> nb_gaussians;
      else if (tag == "apriori")
         in >> apriori;
      else if (tag == "dimensions")
         in >> dimensions;
      else if (tag == "gaussians")
      {
         in >> gaussians;
         using_gaussianIDs = false;
      }
      else if (tag == "gaussianIDs")
      {
         in >> gaussianIDs;
         using_gaussianIDs = true;
      }
      else if (tag == "mode")
         in >> mode;
      else if (tag == "nb_frames_aligned")
         in >> nb_frames_aligned;
      else
         throw new ParsingException ("GMM::readFrom : unknown argument: " + tag);

      if (!in) throw new ParsingException ("GMM::readFrom : Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw new ParsingException ("GMM::readFrom : Parse error: '>' expected ");
   }
}

istream &operator >> (istream &in, GMM &gmm)
{
   if (!isValidType(in, "GMM")) return in;
   gmm.readFrom(in);
   return in;
}
