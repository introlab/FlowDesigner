// Copyright (C) 1998-1999 Jean-Marc Valin
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
#include "gmm.h"
#include <values.h>
#include <typeinfo>
#include <vector>

DECLARE_TYPE(GMM)

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
   gaussians[nb_gaussians]= Ptr<Gaussian> (new Gaussian(*(gaussians[max_gauss])));
   vector <float> &mean = gaussians[nb_gaussians]->getMean();
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
      gaussians[i+old_size]= Ptr<Gaussian> (new Gaussian(*(gaussians[i])));
      vector <float> &mean = gaussians[i+old_size]->getMean();
      for (unsigned int j=0;j<mean.size();j++)
      {
         mean[j]+=.000001*(rand()%100-49.5);
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


void GMM::printOn(ostream &out) const
{
   out << "<GMM " << endl;
   out << "<nb_gaussians " << nb_gaussians << ">" << endl;
   out << "<mode " << mode << ">" << endl;
   out << "<nb_frames_aligned " << nb_frames_aligned << ">" << endl;
   out << "<dimensions " << dimensions << ">" << endl;
   out << "<apriori " << apriori << ">" << endl;
   out << "<gaussians " << gaussians << ">" << endl;
   out << ">\n";
}

void GMM::toIDsUsing (GaussianSet &gauss)
{
   /*using_gaussiansID=true;
     covarianceID=covariances.getIDFor(covariance);*/

}

void GMM::toPtrsUsing (const GaussianSet &gauss)
{
   /*using_covarianceID=false;
     covariance=covariances.getPtrFor(covarianceID);*/
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
       throw ParsingException ("Parse error: '<' expected");
      in >> tag;
      if (tag == "nb_gaussians")
         in >> nb_gaussians;
      else if (tag == "apriori")
         in >> apriori;
      else if (tag == "dimensions")
         in >> dimensions;
      else if (tag == "gaussians")
      {
         /*vector<ObjectRef > tmp;
         in >> tmp;
         gaussians.resize(tmp.size());
         for (int i = 0; i<tmp.size();i++)
         gaussians[i] = tmp[i];*/
         in >> gaussians;
      }
      else if (tag == "mode")
         in >> mode;
      else if (tag == "nb_frames_aligned")
         in >> nb_frames_aligned;
      else
         throw ParsingException ("unknown argument: " + tag);

      if (!in) throw ParsingException ("Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw ParsingException ("Parse error: '>' expected ");
   }
}

istream &operator >> (istream &in, GMM &gmm)
{
   if (!isValidType(in, "GMM")) return in;
   gmm.readFrom(in);
   return in;
}
