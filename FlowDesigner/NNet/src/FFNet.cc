#include "FFNet.h"

#include <vector>
#include "ObjectParser.h"

DECLARE_TYPE(FFNet)

FFNet::FFNet(const Vector<int> &_topo)
   : topo(_topo)
   , layers(topo.size()-1)
{
   //topo = _topo;
   for (int i=0;i<topo.size()-1;i++)
   {
      if (i==topo.size()-2)
      {
	 layers[i]=new FFLayer(topo[i+1],topo[i], "lin");
      } else
	 layers[i]=new FFLayer(topo[i+1],topo[i]);
      layers[i]->init(1.0);
   }
   layers[0]->init(5);

   alpha = .000001;
   last_error=-1;
   momentum=.9;
   /*float *f=layers[0]->getWeights(0);
   f[0]=2;f[1]=-1;
   f[2]=1;*/
}

float *FFNet::calc(const float *input)
{
   layers[0]->update(input);
   for (int i=1;i<layers.size();i++)
      layers[i]->update(layers[i-1]->getValue());
   return layers[layers.size()-1]->getValue();
}

void FFNet::learn(float *input, float *output)
{
   int outputLayer = topo.size()-2;
   calc(input);

   //start with the output layer, towards the input
   for (int k=outputLayer;k>=0;k--)
   {
      FFLayer *currentLayer = layers[k];
      float *previousValue, *currentValue;
      if (k==0)
	 previousValue = input;
      else 
	 previousValue = layers[k-1]->getValue();

      currentValue = currentLayer->getValue();

      int layerSize = topo[k+1];
      int layerInputs = topo[k];
      float *delta = currentLayer->getError();
      for (int i=0;i<layerSize;i++)
      {
	 float *w = currentLayer->getTmpWeights(i);
	 //cerr << k << endl;
	 if (k==outputLayer)
	 {
	    //cerr << "output layer\n";
	    delta[i]=output[i]-currentValue[i];
	    //error += delta[i]*delta[i];
	    //cout << "error = " << delta[i] << endl;
	    delta[i] = currentLayer->deriv[i]*delta[i];
	 }
	 else
	 {
	    delta[i] = 0;
	    float *outErr = layers[k+1]->getError();
	    for (int j=0;j<topo[k+2];j++)
	    {
	       float *outW = layers[k+1]->getWeights(j);
	       delta[i]+= outErr[j]*outW[i];
	    }
	    delta[i] = currentLayer->deriv[i]*delta[i];
	 }
	 for (int j=0;j<layerInputs;j++)
	 {
	    w[j] += alpha * previousValue[j] * delta[i];
	 }
	 w[layerInputs] += alpha * delta[i];
      }
   }	 
}

void FFNet::train(vector<float *> in, vector<float *> out, int iter)
{
   int worse=0;
   while (iter)
   {
      //error = 0;
      int i,j;
      //cerr << "iter...\n";
      for (i=0;i<layers.size();i++)
      {
	 layers[i]->copyToTmp();
      }
      for (i=0;i<in.size();i++)
	 learn (in[i], out[i]);
      for (i=0;i<layers.size();i++)
      {
	 layers[i]->copyFromTmp(momentum);
      }
      iter--;

      double SSE = 0;
      for (i=0;i<in.size();i++)
      {
	 float *netOut = calc (in[i]);
	 for (j=0;j<topo[topo.size()-1];j++)
	    SSE += (netOut[j]-out[i][j])*(netOut[j]-out[i][j]);
      }
      momentum=.9;
      if (last_error > 0 && (SSE/last_error > 1.04 || worse > 4))
      {
	 momentum=0;
	 alpha *= .7;
	 for (i=0;i<layers.size();i++)
	    layers[i]->undo();
	 worse=0;
      }
      else if (last_error > 0 && SSE < last_error)
      {
	 alpha *= 1.05;
	 error = SSE;
	 worse=0;
      } else {
	 worse++;
	 //cerr << "ce quoi l'probleme??? " << error << " " << last_error << endl;
	 error=SSE;
      }
      cout << sqrt(error/in.size()/topo[topo.size()-1]) << "\t" << alpha << endl;

      last_error = error;
   }
}


void FFNet::printOn(ostream &out) const
{
   out << "<FFNet " << endl;
   out << "<topo " << topo << ">" << endl;
   out << "<layers " << layers << ">" << endl;

   out << ">\n";
}


void FFNet::readFrom (istream &in)
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
      if (tag == "topo")
         in >> topo;
      else if (tag == "layers")
         in >> layers;
      else
         throw new ParsingException ("unknown argument: " + tag);

      if (!in) throw new ParsingException ("Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw new ParsingException ("Parse error: '>' expected ");
   }
}

istream &operator >> (istream &in, FFNet &net)
{
   if (!isValidType(in, "FFNet")) return in;
   net.readFrom(in);
   return in;
}
