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


//getIndex implementation for Matrix<bool>
ObjectRef Matrix<bool>::getIndex(int _row, int _col) {
	//look for range ?
	return ObjectRef(Bool::alloc(static_cast<basicType> ((*this)(_row,_col))));
}

//getIndex implementation for Matrix<int>
ObjectRef Matrix<int>::getIndex(int _row, int _col) {
	//look for range ?
	return ObjectRef(Int::alloc(static_cast<basicType> ((*this)(_row,_col))));
}

//getIndex implementation for Matrix<float>
ObjectRef Matrix<float>::getIndex(int _row, int _col) {
	//look for range ?
	return ObjectRef(Float::alloc(static_cast<basicType> ((*this)(_row,_col))));
}

//getIndex implementation for Matrix<double>
ObjectRef Matrix<double>::getIndex(int _row, int _col) {
	//look for range ?
	return ObjectRef(Double::alloc(static_cast<basicType> ((*this)(_row,_col))));
}

//getIndex implementation for Matrix<complex<float> >
ObjectRef Matrix<complex<float> >::getIndex(int _row, int _col) {
	//look for range ?
	return ObjectRef(Complex<float>::alloc(static_cast<basicType> ((*this)(_row,_col))));
}

//getIndex implementation for Matrix<complex<double> >
ObjectRef Matrix<complex<double> >::getIndex(int _row, int _col) {
	//look for range ?
	return ObjectRef(Complex<double>::alloc(static_cast<basicType> ((*this)(_row,_col))));
}

//getIndex implementation for Matrix<ObjectRef>
ObjectRef Matrix<ObjectRef>::getIndex(int _row, int _col) {
	//look for range ?
	return ObjectRef((*this)(_row,_col));
}

//getIndex implementation for Matrix<String>
ObjectRef Matrix<String>::getIndex(int _row, int _col) {
	//look for range ?
	String *new_string = new String();
	*new_string = (*this)(_row,_col);
	return ObjectRef(new_string);
}

//getIndex implementation for Matrix<string>
ObjectRef Matrix<string>::getIndex(int _row, int _col) {
	//look for range ?
	String *new_string = new String();
	*new_string = (*this)(_row,_col);
	return ObjectRef(new_string);
}
