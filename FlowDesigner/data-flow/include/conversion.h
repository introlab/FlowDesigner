// Copyright (C) 2004 Jean-Marc Valin & Dominic Letourneau

#ifndef CONVERSION_H
#define CONVERSION_H

#include "ObjectRef.h"
#include "typemap.h"


typedef ObjectRef (*conv_func)(ObjectRef);

class Conversion {
   static TypeMap<TypeMap<conv_func> > &conv_table();
 public:
   template<class T, class U>
   static int addConvFunction(conv_func func) 
   {
      conv_table()[&typeid(T)][&typeid(U)] = func;
      return 0;
   }
   
   template <class T>
   static ObjectRef convertTo(ObjectRef x)
   {
      TypeMap<TypeMap<conv_func> >::iterator it = conv_table().find(&typeid(*x));
      if (it != conv_table().end())
      {
         TypeMap<conv_func>::iterator it2 = it->second.find(&typeid(T));
         if (it2 != it->second.end())
         {
            return it2->second(x);
         } else {
            cerr << "Cannot cast this to type requested\nThis needs to throw an exception\n";
            return nilObject;
         }
      } else {
         cerr << "Cannot cast\nThis needs to throw an exception\n";
         return nilObject;
      }
   }
};


template<class T, class U>
class dummy_conv_table_init_class {
    static int dummy_var;
};

#define REGISTER_CONVERSION(from, to, func) \
   int dummy_conv_table_init_class<from,to >::dummy_var =         \
        Conversion::addConvFunction<from,to >(func);

#define REGISTER_CONVERSION_TEMPLATE(from, to, func) \
   int dummy_conv_table_init_class<from,to >::dummy_var =         \
        Conversion::addConvFunction<from,to >(func<from,to >);


#endif
