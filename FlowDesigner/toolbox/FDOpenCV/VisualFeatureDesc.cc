/* Copyright (C) 2005 Pierre Moisan (Pierre.Moisan@USherbrooke.ca) 

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.
   
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
   
   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include "VisualFeatureDesc.h"
#include "operators.h"
#include "conversion.h"
#include "vmethod.h"
#include "Vector.h"

using namespace FD;
using namespace std;

namespace RobotFlow {

//
// Declaring known types
//
DECLARE_TYPE(VisualFeatureDesc<double>)

typedef VisualFeatureDesc<double> VisualDoubleFeatureDesc;
typedef Vector<VisualFeatureDesc<double> *> VisualDoubleFeatureDescVec;

//
// Adding operators
//


// Concat operator using two VisualFeatureDesc inputs
template <class X, class Y, class Z>
ObjectRef concatVFDFunction(ObjectRef op1, ObjectRef op2) {
	RCPtr<X> op1Value = op1;
	RCPtr<Y> op2Value = op2;
	
	//creating output vector
	RCPtr<Z> resultValue = RCPtr<Z>(Z::alloc(2));
	
	//concat 2 values into a vector
	(*resultValue)[0] =  &(*op1Value);
	(*resultValue)[1] =  &(*op2Value);
	
	return resultValue;
}

REGISTER_DOUBLE_VTABLE_TEMPLATE(
	concatVtable,
	concatVFDFunction,
	VisualFeatureDesc<double>,
	VisualFeatureDesc<double>,
	Vector<VisualFeatureDesc<double> *>,
	0)

// Concat operator using two Vector<VisualFeatureDesc *> inputs
template <class X, class Y, class Z>
ObjectRef concatVFDVecVecFunction(ObjectRef op1, ObjectRef op2) {
	RCPtr<X> op1Value = op1;
	RCPtr<Y> op2Value = op2;
	
	RCPtr<Z> resultValue(Z::alloc(op1Value->size() + op2Value->size()));
	
	//copy first part
	for (int i =0; i < op1Value->size(); i++) {
		(*resultValue)[i] = (*op1Value)[i];
	}
	
	//copy last part
	for (int i =0; i < op2Value->size(); i++) {
		(*resultValue)[i + op1Value->size()] = (*op2Value)[i];
	}
	
	return resultValue;
}
REGISTER_DOUBLE_VTABLE_TEMPLATE(
	concatVtable,
	concatVFDVecVecFunction,
	Vector<VisualFeatureDesc<double> *>,
	Vector<VisualFeatureDesc<double> *>,
	Vector<VisualFeatureDesc<double> *>,
	0)

// Concat operator using a Vector<VisualFeatureDesc *> input
// followed by a VisualFeatureDesc input
template<class X, class Y, class Z>
ObjectRef concatVectorVFDFunction(ObjectRef op1, ObjectRef op2) {
	RCPtr<X> op1Value = op1;
	RCPtr<Y> op2Value = op2;
	
	//creating new vector
	RCPtr<Z> resultValue(Z::alloc(op1Value->size() + 1));
	
	//copying values from vector
	for (int i = 0; i < resultValue->size(); i++) {    
		(*resultValue)[i] = (*op1Value)[i];
	}
	
	//adding last element
	(*resultValue)[resultValue->size() - 1] = &(*op2Value);
	
	return resultValue;
}
REGISTER_DOUBLE_VTABLE_TEMPLATE(
	concatVtable,
	concatVectorVFDFunction,
	Vector<VisualFeatureDesc<double> *>,
	VisualFeatureDesc<double>,
	Vector<VisualFeatureDesc<double> *>,
	0)

// Concat operator using a VisualFeatureDesc input
// followed by a Vector<VisualFeatureDesc *> input
template<class X, class Y, class Z>
ObjectRef concatVFDVectorFunction(ObjectRef op1, ObjectRef op2) {
	RCPtr<X> op1Value = op1;
	RCPtr<Y> op2Value = op2;
	
	//creating new vector
	RCPtr<Z> resultValue(Z::alloc(op2Value->size() + 1));
	
	//copying values from vector
	for (int i = 1; i < resultValue->size(); i++) {    
		(*resultValue)[i] = (*op2Value)[i - 1];
	}
	
	//adding first element
	(*resultValue)[0] = &(*op1Value);
	
	return resultValue;
}
REGISTER_DOUBLE_VTABLE_TEMPLATE(
	concatVtable,
	concatVFDVectorFunction,
	VisualFeatureDesc<double>,
	Vector<VisualFeatureDesc<double> *>,
	Vector<VisualFeatureDesc<double> *>,
	0)
	
// ToVect conversion 
template <class T, class U>
ObjectRef VisualFeatDesc2VectorConversion (ObjectRef in) {
	RCPtr<T> FromVFD = in;
	RCPtr<U> ToVector(U::alloc(1));
	(*ToVector)[0] = &(*FromVFD);
	return ToVector;
}

static int dummy_init_for_visual_feature_to_vect = FD::vmethod()->registerFunct0(
		VisualFeatDesc2VectorConversion<VisualFeatureDesc<double>,Vector<VisualFeatureDesc<double>* > >,
		&typeid(VisualFeatureDesc<double>),
		"toVect");
  
  //TODO working conversion!

  //static int dummy_init_for_visual_feature_conversion  = Conversion::addConvFunction
  //<VisualFeatureDesc<double>, Vector<VisualFeatureDesc<double> *> >
  //(VisualFeatDesc2VectorConversion<Vector<VisualFeatureDesc<double> *> >);


}//namespace RobotFlow
