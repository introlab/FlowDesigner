#include "NNetSet.h"

DECLARE_TYPE(NNetSet)

double *NNetSet::calc(int id, const double *input)
{
   //cerr << "calc for id " << id << endl;
   return nets[id]->calc(input);
   //cerr << "done...\n";
}

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
   
}

void NNetSet::trainDeltaBar(vector<int> id, vector<float *> tin, vector<float *> tout, int iter, 
		    double learnRate, double mom, double increase, double decrease, int nbSets)
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
      nets[i]->trainDeltaBar(in[i],out[i],iter,learnRate,mom,increase,decrease,nbSets);
   }
   
}

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
       throw new ParsingException ("Parse error: '<' expected");
      in >> tag;
      if (tag == "nets")
      {
	 cerr << "reading nets...\n";
         in >> nets;
	 cerr << "done\n";
      }
      else
         throw new ParsingException ("unknown argument: " + tag);

      if (!in) throw new ParsingException ("Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw new ParsingException ("Parse error: '>' expected ");
   }
}

istream &operator >> (istream &in, NNetSet &net)
{
   if (!isValidType(in, "NNetSet")) return in;
   net.readFrom(in);
   return in;
}
