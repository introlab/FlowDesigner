// Copyright (C) 1999 Jean-Marc Valin

#ifndef DOUBLE_DISPATCH_H
#define DOUBLE_DISPATCH_H

#include "typemap.h"
#include "ObjectRef.h"
#include "BaseException.h"
#include <map>
#include <string>


namespace FD {

class DoubleDispatch;

class DoubleDispatchException : public BaseException {
  protected:
   DoubleDispatch *table;
   std::string type1, type2;
  public:
   DoubleDispatchException(DoubleDispatch *_table, std::string _type1, std::string _type2);
   virtual void print(std::ostream &out = std::cerr);
   
};


class DoubleDispatch {
  public:
   typedef ObjectRef (*funct_ptr) (ObjectRef x, ObjectRef y);

   static DoubleDispatch& getTable(const std::string &tableName);
   static std::map<std::string,DoubleDispatch>& getAllTables();

  protected:

   //std::string name;
   typedef TypeMap<funct_ptr> vtable1Type;
   typedef TypeMap<vtable1Type> vtable2Type;
   vtable2Type vtable;

  public:
   
   std::string getName();

   DoubleDispatch(){}

   int getSize() {return vtable.size();}

   int registerFunct(funct_ptr ptr, const std::type_info *x, const std::type_info *y)
   {
     vtable[x][y] = ptr;
     return vtable.size();
   }
   
   ObjectRef call(ObjectRef x, ObjectRef y)
   {
      const std::type_info *t1 = &typeid(x.get());
      const std::type_info *t2 = &typeid(y.get());
      vtable2Type::iterator v1 = vtable.find(t1);
      if (v1!=vtable.end())
      {
	 vtable1Type::iterator v2 = v1->second.find(t2);
	 if (v2!=v1->second.end())
	 {
	    return v2->second(x,y);
	 } else {
	    throw new DoubleDispatchException(this, t1->name(), t2->name());
	 }
      } else {
	 throw new DoubleDispatchException(this, t1->name(), t2->name());
      }
   }
};

#define REGISTER_DOUBLE_VTABLE(name, func, type1, type2) \
  int dummy_vtable_init_for ## name ## _ ## func  = \
    DoubleDispatch::getTable(#name).registerFunct(func, &typeid(type1), &typeid(type2));			



#define REGISTER_DOUBLE_VTABLE_TEMPLATE(name, func, type1, type2, type3, id) \
  int dummy_vtable_init_for ## name ## func ## _ ## id =  \
    DoubleDispatch::getTable(#name).registerFunct(func<type1,type2,type3>, &typeid(type1), &typeid(type2));

}//namespace FD
#endif
