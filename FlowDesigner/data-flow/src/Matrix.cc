// Copyright (C) 2001 Jean-Marc Valin

#include "Matrix.h"
#include "Object.h"
#include "ObjectParser.h"
#include <complex>
#include "operators.h"
#include "Complex.h"

//@implements core

DECLARE_TYPE2("Matrix", Matrix<float>)
DECLARE_TYPE(Matrix<bool>)
DECLARE_TYPE(Matrix<int>)
DECLARE_TYPE(Matrix<float>)
DECLARE_TYPE(Matrix<double>)
DECLARE_TYPE2("Matrix<complex<float>>",Matrix<complex<float> >)
DECLARE_TYPE2("Matrix<complex<double>>",Matrix<complex<double> >)
DECLARE_TYPE(Matrix<ObjectRef>)
DECLARE_TYPE(Matrix<string>)
DECLARE_TYPE(Matrix<String>)


//index implementation for Matrix<bool>
ObjectRef Matrix<bool>::index(int _row, int _col) {
	//look for range ?
	return ObjectRef(Bool::alloc(static_cast<basicType> ((*this)(_row,_col))));
}

//index implementation for Matrix<int>
ObjectRef Matrix<int>::index(int _row, int _col) {
	//look for range ?
	return ObjectRef(Int::alloc(static_cast<basicType> ((*this)(_row,_col))));
}

//index implementation for Matrix<float>
ObjectRef Matrix<float>::index(int _row, int _col) {
	//look for range ?
	return ObjectRef(Float::alloc(static_cast<basicType> ((*this)(_row,_col))));
}

//index implementation for Matrix<double>
ObjectRef Matrix<double>::index(int _row, int _col) {
	//look for range ?
	return ObjectRef(Double::alloc(static_cast<basicType> ((*this)(_row,_col))));
}

//index implementation for Matrix<complex<float> >
ObjectRef Matrix<complex<float> >::index(int _row, int _col) {
	//look for range ?
	return ObjectRef(Complex<float>::alloc(static_cast<basicType> ((*this)(_row,_col))));
}

//index implementation for Matrix<complex<double> >
ObjectRef Matrix<complex<double> >::index(int _row, int _col) {
	//look for range ?
	return ObjectRef(Complex<double>::alloc(static_cast<basicType> ((*this)(_row,_col))));
}

//index implementation for Matrix<ObjectRef>
ObjectRef Matrix<ObjectRef>::index(int _row, int _col) {
	//look for range ?
	return ObjectRef((*this)(_row,_col));
}

//index implementation for Matrix<String>
ObjectRef Matrix<String>::index(int _row, int _col) {
	//look for range ?
	String *new_string = new String();
	*new_string = (*this)(_row,_col);
	return ObjectRef(new_string);
}

//index implementation for Matrix<string>
ObjectRef Matrix<string>::index(int _row, int _col) {
	//look for range ?
	String *new_string = new String();
	*new_string = (*this)(_row,_col);
	return ObjectRef(new_string);
}
