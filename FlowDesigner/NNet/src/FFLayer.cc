#include "FFLayer.h"
#include <stdlib.h>
#include "ObjectParser.h"

DECLARE_TYPE(FFLayer)

FFLayer::FFLayer (int _nbNeurons, int _nbInputs, string type)
   : nbNeurons(_nbNeurons)
   , nbInputs (_nbInputs)
   , funcType(type)
   , alloc(true)
{
   if (funcType == "lin")
   {
      func = lin;
      deriv_func = deriv_lin;
   } else if (funcType == "sigmoid")
   {
      func = sigmoid;
      deriv_func = deriv_sigmoid;
   } else if (funcType == "tansig")
   {
      func = tansig;
      deriv_func = deriv_tansig;
   } /*else if (funcType == "tanh")
   {
      func = tanh;
      deriv_func = deriv_tanh;
      }*/
   weights = new double [nbNeurons*(nbInputs+1)];
   /*for (int i=0;i<nbNeurons*(nbInputs+1);i++)
   {
      //weights[i]=1.0;
      weights[i]=sqrt(3.0/nbInputs)*((rand()%1000) * .002 - .1);
      }*/

   momentum = new double [nbNeurons*(nbInputs+1)];
   for (int i=0;i<nbNeurons*(nbInputs+1);i++)
      momentum[i]=0;

   gradient = new double [nbNeurons*(nbInputs+1)];
   saved_weights = new double [nbNeurons*(nbInputs+1)];

   deriv = new double [nbNeurons];
   value = new double [nbNeurons];
   error = new double [nbNeurons];
}

FFLayer::FFLayer(const FFLayer &layer)
   : nbNeurons(layer.nbNeurons)
   , nbInputs(layer.nbInputs)
   , funcType(layer.funcType)
   , func(layer.func)
   , deriv_func(layer.deriv_func)
   , alloc(true)
{

   weights = new double [nbNeurons*(nbInputs+1)];

   for (int i=0;i<nbNeurons*(nbInputs+1);i++)
      weights[i] = layer.weights[i];

   momentum = new double [nbNeurons*(nbInputs+1)];

   for (int i=0;i<nbNeurons*(nbInputs+1);i++)
      momentum[i]=0;
   
   gradient = new double [nbNeurons*(nbInputs+1)];

   saved_weights = new double [nbNeurons*(nbInputs+1)];
   
   deriv = new double [nbNeurons];
   value = new double [nbNeurons];
   error = new double [nbNeurons];
}

/*FFLayer &FFLayer::operator=(const FFLayer &layer)
{
   if (this == &layer)
   {
      return this;
   } else {
      re
   }
   
   }*/

void FFLayer::init(double minmax)
{
   for (int i=0;i<nbNeurons*(nbInputs+1);i++)
   {
      //weights[i]=1.0;
      weights[i]=sqrt(3.0/nbInputs)*((rand()%1000) * .002 - .1)/minmax;
   }
}

void FFLayer::init(double *mean, double *std)
{
   double meanSum = 0;
   for (int j=0;j<nbInputs;j++)
      meanSum += mean[j];
   for (int i=0;i<nbNeurons;i++)
   {
      double meanSum = 0;
      for (int j=0;j<nbInputs;j++)
      {
	 weights[i*(nbInputs+1) + j] = sqrt(3.0/nbInputs)*((rand()%1000) * .002 - .1)/std[j];
	 meanSum += weights[i*(nbInputs+1) + j]*mean[j];
      }
      weights[i*(nbInputs+1) + nbInputs] = sqrt(3.0/nbInputs)*((rand()%1000) * .002 - .1) - meanSum;
   }
}

void FFLayer::setBias(double *minmax)
{
   for (int i=0;i<nbNeurons;i++)
   {
      //cerr << minmax[i] << " ";
      weights[i*(nbInputs+1) + nbInputs] = minmax[i];
   }   
   //cerr << endl;
}

void FFLayer::printOn(ostream &out) const
{
   out << "<FFLayer " << endl;
   out << "<nbNeurons " << nbNeurons << ">" << endl;
   out << "<nbInputs " << nbInputs << ">" << endl;
   out << "<funcType " << funcType << " >" << endl;

   out << "<weights ";
   for (int i=0;i<nbNeurons*(nbInputs+1);i++)
      out << weights[i] << " ";
   out << " >" << endl;

   out << ">\n";
}


void FFLayer::readFrom (istream &in)
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
      //cerr << "layer tag = " << tag << endl;
      if (tag == "nbNeurons")
         in >> nbNeurons;
      else if (tag == "nbInputs")
         in >> nbInputs;
      else if (tag == "funcType")
      {
         in >> funcType;
	 if (funcType == "lin")
	 {
	    func = lin;
	    deriv_func = deriv_lin;
	 } else if (funcType == "sigmoid")
	 {
	    func = sigmoid;
	    deriv_func = deriv_sigmoid;
	 } else if (funcType == "tansig")
	 {
	    func = tansig;
	    deriv_func = deriv_tansig;
	 } /*else if (funcType == "tanh")
	 {
	    func = tanh;
	    deriv_func = deriv_tanh;
	    }*/
      } else if (tag == "weights")
      {
	 //cerr << "weights\n";
	 weights = new double [nbNeurons*(nbInputs+1)];
	 for (int i=0;i<nbNeurons*(nbInputs+1);i++)
	 {
	    //weights[i]=1.0;
	    in >> weights[i];
	    /* temporary workaround to gnome localisation "feature"
	    string tata;
	    in >> tata;
	    weights[i] = atof (tata.c_str());
	    cerr << tata << " ";*/
	 }
	 //cerr << "allocation...\n";
	 gradient = new double [nbNeurons*(nbInputs+1)];
	 saved_weights = new double [nbNeurons*(nbInputs+1)];
	 
	 deriv = new double [nbNeurons];
	 value = new double [nbNeurons];
	 error = new double [nbNeurons];
	 momentum = new double [nbNeurons*(nbInputs+1)];
	 for (int i=0;i<nbNeurons*(nbInputs+1);i++)
	 {
	    momentum[i]=0;
	 }

	 //cerr << "done with weights\n";
      }
      else
         throw new ParsingException ("unknown argument: " + tag);

      if (!in) throw new ParsingException ("Parse error trying to build " + tag);

      in >> tag;
      //cerr << "end tag = " << tag << endl;
      if (tag != ">") 
         throw new ParsingException ("Parse error: '>' expected ");
   }
   //cerr << "done with layer\n";
}


istream &operator >> (istream &in, FFLayer &layer)
{
   if (!isValidType(in, "FFLayer")) return in;
   layer.readFrom(in);
   return in;
}
