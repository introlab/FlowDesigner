#ifndef COVARIANCE_H
#define COVARIANCE_H

class Frame;
class ifstream;
class ofstream;

enum covariance_type {diagonal, radial, full};

class Covariance 
{
private:
  int   dimension;     // *** dimension of the covariance matrix
  int   type;          // *** one of diagonal, radial, full ...
  float *covariance;   // *** matrix data
  float determinant;   // *** log of the determinant

public:
  Covariance();
  Covariance(int, float *, int);
  Covariance(const Covariance &);

  ~Covariance();

  void setCovariance(int, float *, int, int);

  int   getDimension()   const { return dimension; }
  int   getType()        const { return type; }
  float *getCovariance() const { return covariance; }
  float getDeterminant() const { return determinant; }

  float calculateMetric(const Frame &);  // used to calculate score ...

  void load(ifstream &);
  void save(ofstream &);

  void print();

  float&      operator()(int,int);
  Covariance& operator=(const Covariance&);
};

#endif  

