// Copyright (C) 1999 Jean-Marc Valin

#ifndef GEN_TYPE_ARRAY_H
#define GEN_TYPE_ARRAY_H

#include "Object.h"
#include "Vector.h"
#include "ObjectParser.h"
#include "misc.h"
#include "vec.h"

namespace FD {
	
	template<class T>
	class Array : public Vector<T>
	{
	public:
		Array() : Vector<T>() {}
		explicit Array(int n, const T &x = T()) : Vector<T>(n, x) {}
		void printOn(std::ostream &out) const
		{
			out << *static_cast<const Vector<T> *> (this);
		}
		
		/*virtual void rawWrite(std::ostream &out) const
		 {
		 out.write ((const char*) &operator[](0), size()*sizeof(T));
		 }*/
		
		void readFrom(std::istream &in=std::cin);
		
		virtual void destroy() {delete this;}
		
		Array<T> &operator+= (const Array<T> &v2) 
		{
			if (this->size() != v2.size())
				std::cerr << "Array size mismatch\n";
			for (size_t i=0;i<this->size();i++)
				this->operator[](i) += v2[i];
			return *this;
		}
		
		Array<T> &operator-= (const Array<T> &v2) 
		{
			if (this->size() != v2.size())
				std::cerr << "Array size mismatch\n";
			for (size_t i=0;i<this->size();i++)
				this->operator[](i) -= v2[i];
			return *this;
		}
		
		Array<T> operator+ (const Array<T> &v2) 
		{
			Array<T> v(*this);
			v += v2;
			return v;
		}
		
		Array<T> operator- (const Array<T> &v2) 
		{
			Array<T> v(*this);
			v -= v2;
			return v;
		}
		
		Array<T> operator- () 
		{
			Array<T> v(this->size());
			for (size_t i=0;i<this->size();i++)
				v[i] = -this->operator[](i);
			return v;
		}
		
		T operator* (const Array<T> &v2) 
		{
			if (this->size() != v2.size())
				std::cerr << "Array size mismatch\n";
			T sum=0;
			for (size_t i=0;i<this->size();i++)
				sum += this->operator[](i)*v2[i];
			return sum;
		}
		
		Array<T> &operator*= (T scal) 
		{
			for (size_t i=0;i<this->size();i++)
				this->operator[](i) *= scal;
			return *this;
		}
		
		Array<T> &operator/= (T scal) 
		{
			for (int i=0;i<this->size();i++)
				this->operator[](i) /= scal;
			return *this;
		}
		
		Array<T> operator* (T scal) 
		{
			Array<T> v(*this);
			v *= scal;
			return v;
		}
		
		
		Array<T> operator/ (T scal) 
		{
			Array<T> v(*this);
			v /= scal;
			return v;
		}
		
		T norm() 
		{
			return sqrt(vec_norm2(&this->operator[](0), this->size()));
		}
		
		T norm2() 
		{
			return vec_norm2(&this->operator[](0), this->size());
		}
		
	};
	
	
	
	template <class T>
	inline void Array<T>::readFrom(std::istream &in)
	{
		int items_found=0;
		
		while (!in.eof())
		{
			T tmp;
			in >> tmp;
			if (in.fail()) break;
			items_found++;
			this->resize(items_found);
			this->operator[] (items_found-1)=tmp;
		}
		in.clear();
		char ch;
		in >> ch;       
	}
	
	
}//namespace FD

#endif
