#include "covariance.h"

#include "frame.h"

#include<iostream.h>
#include<fstream.h>
#include<assert.h>

#define IMPLERR cerr << "NOT IMPLEMENTED!" << endl


Covariance::Covariance()
{
  setCovariance(0, 0, 0, 1);
}

Covariance::Covariance(int _dimension, float *_covariance, int _type) 
{
  setCovariance(_dimension, _covariance, _type, 1);
}

Covariance::Covariance(const Covariance &_covariance)
{
  setCovariance(_covariance.getDimension(), _covariance.getCovariance(), _covariance.getType(), 1);
}


Covariance::~Covariance()
{
  delete covariance;
}


void Covariance::setCovariance(int _dimension, float *_covariance, int _type, int _new=1)
{
  if (!_new)
    delete covariance;

  dimension  = _dimension;
  type       = _type;

  switch (type)
  {
  case diagonal:
    covariance  = new float [dimension];
    determinant = 1;
    for (int i=0; i<dimension; i++)
    {
      covariance[i] = _covariance[i];
      determinant *= covariance[i];
    }
    break;
  case radial:
    IMPLERR;
    break;
  case full:
    covariance = new float [dimension*dimension];
     for (int i=0; i<dimension*dimension; i++)
      covariance[i] = _covariance[i];
     // ***** calculate determinant!!
    break;
  default:
    break;
  }
}



float Covariance::calculateMetric(const Frame &frame)
{
  float result = 0.0;
  int i,j;
  
  switch (type)
  {
  case diagonal:
    for ( i=0; i<dimension; i++)
      result += covariance[i] * frame[i] * frame[i];
    break;
  case radial:
    break;
  case full:
    for (i=0; i<dimension; i++)
      for (j=0; j<dimension; j++)
        result += covariance[i*dimension+j] * frame[i] * frame[j];
    break;
  default:
    break;
  }

  return result;  
}


void Covariance::print()
{
  if (covariance != 0)
  {
    switch (type)
    {
    case diagonal:
      for (int i=0; i<dimension; i++)
      {
	for (int j=0; j<dimension; j++)
	  if (i==j)
	    cout << covariance[i] << ", ";
	  else
	    cout << "0.0, ";
	cout << endl;
      }
      break;
    case radial:
      break;
    case full:
      for (int i=0; i<dimension; i++)
      {
	for (int j=0; j<dimension; j++)
	  cout << covariance[i*dimension+j] << ", ";
	cout << endl;
      }
      break;
    default:
      break;
    }
  }
}



void Covariance::save(ofstream &file)
{
  file.write((char*) &type,        sizeof(type));
  file.write((char*) &dimension,   sizeof(dimension));
  file.write((char*) &determinant, sizeof(determinant));

  cout << "Wrote: " << type << ", " << dimension << ", " << determinant << endl;

  switch (type)
  {
  case diagonal:
    file.write((char*) covariance, sizeof(float)*dimension);
    break;
  case radial:
    break;
  case full:
    file.write((char*) covariance, sizeof(float)*dimension*dimension);
    break;
  default:
    break;
  }
}


void Covariance::load(ifstream &file)
{
  file.read((char*) &type,        sizeof(type));
  file.read((char*) &dimension,   sizeof(dimension));
  file.read((char*) &determinant, sizeof(determinant));
  
  if (covariance != 0)
  {
    delete covariance;
    covariance = 0;
  }
  
  switch (type)
  {
  case diagonal:
    covariance  = new float [dimension];
    file.read((char*) covariance, sizeof(float)*dimension);
    break;
  case radial:
    break;
  case full:
    covariance  = new float [dimension*dimension];
    file.read((char*) covariance, sizeof(float)*dimension*dimension);
    break;
  default:
    break;
  }
}


float &Covariance::operator()(int i, int j)
{
  switch (type)
  {
  case diagonal:
    assert (i==j && i >= 0 && i < dimension);
    return covariance[i];
    break;
  case radial:
    IMPLERR;
    break;
  case full:
    assert (i*dimension+j >= 0 && i*dimension+j < dimension*dimension);
    return covariance[i*dimension+j];
    break;
  default:
    IMPLERR;
    break;
  }
}


Covariance &Covariance::operator=(const Covariance &_covariance)
{  
  if (this != &_covariance)
  {
    setCovariance(_covariance.getDimension(), _covariance.getCovariance(), _covariance.getType(), 0);
  }
  return *this;
}

