// Copyright (C) 2001 Jean-Marc Valin

#include "NNetSet.h"
#include "TrainingAlgo.h"

DECLARE_TYPE(NNetSet)
//@implements NNetSet
//@require FFNet

NNetSet::NNetSet(int nbNets, const Vector<int> &topo, const Vector<string> &functions, vector<int> id, vector<float *> &tin, vector<float *> &tout)
{
   nets.resize(nbNets);

   vector<vector <float *> > in(nbNets);
   vector<vector <float *> > out(nbNets);
   for (int i=0;i<tin.size();i++)
   {
      in[id[i]].insert(in[id[i]].end(), tin[i]);
      out[id[i]].insert(out[id[i]].end(), tout[i]);
   }
   
   for (int i=0;i<nbNets;i++)
   {
      nets[i] = new FFNet (topo, functions, in[i], out[i]);
   }
   value = new float [nets[0]->getNbWeights()];
}

NNetSet::NNetSet(vector<int> id, vector<float *> &tin, vector<float *> &tout, NNetSet *net1, NNetSet *net2)
{
   int nbNets = net1->nets.size();
   cerr << "nbNets = " << nbNets << endl;
   nets.resize(nbNets);
   cerr << "resized\n";
   vector<vector <float *> > in(nbNets);
   vector<vector <float *> > out(nbNets);
   cerr << "separating...\n";
   for (int i=0;i<tin.size();i++)
   {
      in[id[i]].insert(in[id[i]].end(), tin[i]);
      out[id[i]].insert(out[id[i]].end(), tout[i]);
   }

   for (int i=0;i<nbNets;i++)
   {
      cerr << "net #" << i << endl;
      float err1 = net1->nets[i]->totalError(in[i], out[i]);
      float err2 = net2->nets[i]->totalError(in[i], out[i]);
      NNetSet *best = err1 < err2 ? net1 : net2;
      nets[i] = new FFNet (*best->nets[i]);
   }
   value = new float [nets[0]->getNbWeights()];

}

float *NNetSet::calc(int id, const float *input)
{
   //cerr << "calc for id " << id << endl;
   return nets[id]->calc(input, value);
   //cerr << "done...\n";
}
/*
void NNetSet::train(vector<int> id, vector<float *> tin, vector<float *> tout, int iter, 
		    double learnRate, double mom, double increase, double decrease, double errRatio, int nbSets)
{
   int nbNets = nets.size();
   cerr << "nbNets = " << nbNets << endl;
   vector<vector <float *> > in(nbNets);
   cerr << "tata\n";
   vector<vector <float *> > out(nbNets);
   cerr << "classification...\n";
   for (int i=0;i<tin.size();i++)
   {
      in[id[i]].insert(in[id[i]].end(), tin[i]);
      out[id[i]].insert(out[id[i]].end(), tout[i]);
   }
   
   cerr << "low-level training...\n";
   for (int i=0;i<nbNets;i++)
   {
      //if (i==47 || i==56)
      nets[i]->train(in[i],out[i],iter,learnRate,mom,increase,decrease,errRatio,nbSets);
   }
   
   }*/

void NNetSet::trainDeltaBar(vector<int> id, vector<float *> tin, vector<float *> tout, int iter, 
		    double learnRate, double increase, double decrease)
{
   int nbNets = nets.size();
   cerr << "nbNets = " << nbNets << endl;
   vector<vector <float *> > in(nbNets);
   cerr << "tata\n";
   vector<vector <float *> > out(nbNets);
   cerr << "classification...\n";
   for (int i=0;i<tin.size();i++)
   {
      in[id[i]].insert(in[id[i]].end(), tin[i]);
      out[id[i]].insert(out[id[i]].end(), tout[i]);
   }
   
   cerr << "low-level training...\n";
   for (int i=0;i<nbNets;i++)
   {
      //if (i==47 || i==56)
      //if (i==1 || i==2 || i==4 || i==5 || i==17 || i==23)
      TrainingDeltaBarDelta::train(&(*nets[i]), in[i],out[i],iter,learnRate,increase,decrease);
   }
   
}
/*
void NNetSet::trainCGB(vector<int> id, vector<float *> tin, vector<float *> tout, int iter, 
		    double sigma, double lambda)
{
   int nbNets = nets.size();
   cerr << "nbNets = " << nbNets << endl;
   vector<vector <float *> > in(nbNets);
   cerr << "tata\n";
   vector<vector <float *> > out(nbNets);
   cerr << "classification...\n";
   for (int i=0;i<tin.size();i++)
   {
      in[id[i]].insert(in[id[i]].end(), tin[i]);
      out[id[i]].insert(out[id[i]].end(), tout[i]);
   }
   
   cerr << "low-level training...\n";
   for (int i=0;i<nbNets;i++)
   {
      //if (i==47 || i==56)
      nets[i]->trainCGB(in[i],out[i],iter,sigma,lambda);
   }
   
}
*/

void NNetSet::printOn(ostream &out) const
{
   out << "<NNetSet " << endl;
   out << "<nets " << nets << ">" << endl;
   out << ">\n";
}


void NNetSet::readFrom (istream &in)
{
   string tag;

   while (1)
   {
      char ch;
      in >> ch;
      if (ch == '>') break;
      else if (ch != '<') 
       throw new ParsingException ("NNetSet::readFrom : Parse error: '<' expected");
      in >> tag;
      if (tag == "nets")
      {
	 cerr << "reading nets...\n";
         in >> nets;
	 cerr << "done\n";
      }
      else
         throw new ParsingException ("NNetSet::readFrom : unknown argument: " + tag);

      if (!in) throw new ParsingException ("NNetSet::readFrom : Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw new ParsingException ("NNetSet::readFrom : Parse error: '>' expected ");
   }
   value = new float [nets[0]->getNbWeights()];
}

istream &operator >> (istream &in, NNetSet &net)
{
   if (!isValidType(in, "NNetSet")) return in;
   net.readFrom(in);
   return in;
}
