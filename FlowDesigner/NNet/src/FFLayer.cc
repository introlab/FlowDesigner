#include "FFLayer.h"
#include <stdlib.h>
#include "ObjectParser.h"

DECLARE_TYPE(FFLayer)

FFLayer::FFLayer (int _nbNeurons, int _nbInputs, string type)
   : nbNeurons(_nbNeurons)
   , nbInputs (_nbInputs)
   , funcType(type)
{
   if (funcType == "lin")
   {
      func = lin;
      deriv_func = deriv_lin;
   } else if (funcType == "sigmoid")
   {
      func = sigmoid;
      deriv_func = deriv_sigmoid;
   } else if (funcType == "tanh")
   {
      func = tanh;
      deriv_func = deriv_tanh;
   }
   weights = new float [nbNeurons*(nbInputs+1)];
   for (int i=0;i<nbNeurons*(nbInputs+1);i++)
   {
      //weights[i]=1.0;
      weights[i]=(rand()%1000) * .0002 - .05;
   }

   tmp_weights = new float [nbNeurons*(nbInputs+1)];

   deriv = new float [nbNeurons];
   value = new float [nbNeurons];
   error = new float [nbNeurons];
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
	 } else if (funcType == "tanh")
	 {
	    func = tanh;
	    deriv_func = deriv_tanh;
	 }
      } else if (tag == "weights")
      {
	 weights = new float [nbNeurons*(nbInputs+1)];
	 for (int i=0;i<nbNeurons*(nbInputs+1);i++)
	 {
	    //weights[i]=1.0;
	    in >> weights[i];
	 }
	 
	 tmp_weights = new float [nbNeurons*(nbInputs+1)];
	 
	 deriv = new float [nbNeurons];
	 value = new float [nbNeurons];
	 error = new float [nbNeurons];
	 
	 
      }
      else
         throw new ParsingException ("unknown argument: " + tag);

      if (!in) throw new ParsingException ("Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw new ParsingException ("Parse error: '>' expected ");
   }
}


istream &operator >> (istream &in, FFLayer &layer)
{
   if (!isValidType(in, "FFLayer")) return in;
   layer.readFrom(in);
   return in;
}
