// Copyright (C) 2008 Alberto Perotti
// Author: Alberto Perotti

#include "Constellation.h"


namespace FD {

Constellation::Constellation()
{
	ParameterSet ps;
	c_initialize(ps);
}


Constellation::Constellation(ParameterSet params)
{
	c_initialize(params);
}

void Constellation::c_initialize(ParameterSet &params)
{
	int i, j;
	int nbh, nbhp;
	unsigned int ui;

	if (params.exist("TYPE"))
	{
		if (object_cast<String> (params.get("TYPE")) == "PAM")
			type = PAM;
		else if (object_cast<String> (params.get("TYPE")) == "PSK")
			type = PSK;
		else if (object_cast<String> (params.get("TYPE")) == "QAM")
			type = QAM;
		else if (object_cast<String> (params.get("TYPE")) == "FILE")
			type = FILE_T;
	}
	else type = PAM;

      if (params.exist("NBITS"))
	 d_nbits = dereference_cast<int> (params.get("NBITS"));
      else
	 d_nbits = 1;

      nsignals = 1 << d_nbits;

      if (params.exist("ENERGY"))
	 Es = dereference_cast<float> (params.get("ENERGY"));
      else
	 Es = 1.;


	if(params.exist("FILENAME"))
	{
		filename = object_cast<String> (params.get("FILENAME"));
	}

	resize(1 << d_nbits);

	complex<float>  tmpc;
	double Es0 = 0.;
	ifstream ifs;
	switch(type)
	{
		case PAM:
			for (i = 0; i < nsignals; i++)
			{
				this->at(BIN2GRAY(i)) = complex<float>(nsignals - 1 - 2 * i, 0.);
				this->at(BIN2GRAY(i)) *= sqrt((3. * Es) / float(nsignals * nsignals - 1.));
			}
			break;
		case PSK:
			for (i = 0; i < nsignals; i++)
			{
				this->at(BIN2GRAY(i)) = complex<float>(cos((1 + 2 * i) * PI / float(nsignals)),
						sin((1 + 2 * i) * PI / float(nsignals)));
				this->at(BIN2GRAY(i)) *= sqrt(Es);
			}
			break;
		case QAM:
			nbh = 1 << (d_nbits >> 1);
			nbhp = 1 << ((d_nbits + 1) >> 1);
			for(i = 0; i < nbhp; i++)
			{
				for(j = 0; j < nbh; j++)
				{
					this->at(BIN2GRAY(j) + nbh * BIN2GRAY(i)) =
						complex<float>(nbhp - 1 - 2 * i, nbh - 1 - 2 * j);
					Es0 += this->at(BIN2GRAY(j) + nbh * BIN2GRAY(i)).real() *
						this->at(BIN2GRAY(j) + nbh * BIN2GRAY(i)).real();
					Es0 += this->at(BIN2GRAY(j) + nbh * BIN2GRAY(i)).imag() *
						this->at(BIN2GRAY(j) + nbh * BIN2GRAY(i)).imag();
				}
			}
			Es0 /= float(nsignals);
			for(i = 0; i < nbhp; i++)
			{
				for(j = 0; j < nbh; j++)
				{
					this->at(BIN2GRAY(j) + nbh * BIN2GRAY(i)) *= sqrt(Es / Es0);
				}
			}
			break;
		case FILE_T:
			ifs.open(filename.c_str());
			ifs >> *this;
			ifs.close();

			for(ui = 0; ui < this->size(); ui++)
			{
				Es0 += this->at(ui).real() * this->at(ui).real();
				Es0 += this->at(ui).imag() * this->at(ui).imag();
			}
			nsignals = this->size();
			d_nbits = floor(log2(nsignals));
			Es0 /= float(nsignals);
			for(i = 0; i < nsignals; i++)
			{
				this->at(i) *= sqrt(Es / Es0);
			}
			break;
		default:
			throw 1;
	}
}

}	//namespace FD
