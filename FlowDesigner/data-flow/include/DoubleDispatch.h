// Copyright (C) 1999 Jean-Marc Valin

#ifndef DOUBLE_DISPATCH_H
#define DOUBLE_DISPATCH_H

#include "typemap.h"
#include "ObjectRef.h"
#include "BaseException.h"

//#ifdef NO_HASH_MAP
#include <map>
//#else
//#include <hash_map>
//#endif

class DoubleDispatch;

class DoubleDispatchException : public BaseException {
  protected:
   DoubleDispatch *table;
   string type1, type2;
  public:
   DoubleDispatchException(DoubleDispatch *_table, string _type1, string _type2);
   virtual void print(ostream &out = cerr);
   
};

//template<class T, class U>
class DoubleDispatch {
  public:
   typedef ObjectRef (*funct_ptr) (ObjectRef x, ObjectRef y);

  protected:
   string name;
//#ifdef NO_HASH_MAP
   //typedef map<const type_info *, funct_ptr> vtable1Type;
   //typedef map<const type_info *, vtable1Type > vtable2Type;

   typedef TypeMap<funct_ptr> vtable1Type;
   typedef TypeMap<vtable1Type> vtable2Type;
//#else
//   typedef hash_map<const type_info *, funct_ptr> vtable1Type;
//   typedef hash_map<const type_info *, vtable1Type > vtable2Type;
//#endif
   vtable2Type vtable;

  public:
   DoubleDispatch(string _name) : name(_name) {}

   const string &getName() {return name;}

   void registerFunct(funct_ptr ptr, const type_info *x, const type_info *y)
   {
      vtable[x][y] = ptr;
   }

   ObjectRef call(ObjectRef x, ObjectRef y)
   {
      const type_info *t1 = &typeid(*x);
      const type_info *t2 = &typeid(*y);
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


#define DEFINE_DOUBLE_VTABLE(klass) class klass {                                   \
  public:                                                                           \
   static DoubleDispatch &vtable() {static DoubleDispatch table(# klass); return table;}     \
   static ObjectRef perform(ObjectRef x, ObjectRef y)                               \
   {                                                                                \
      return vtable().call(x,y);                                                    \
   }                                                                                \
   static int reg(DoubleDispatch::funct_ptr ptr, const type_info *x, const type_info *y) \
   {                                                                                \
      vtable().registerFunct(ptr,x,y);                                              \
      return 0;                                                                     \
   }                                                                                \
};

#define REGISTER_DOUBLE_VTABLE(klass, func, type1, type2) \
        int dummy_vtable_init_for ## klass ## _ ## func =\
        klass::reg(func, &typeid(type1), &typeid(type2));

//
//Maybe it should be called something else and moved somewhere else
//(DL)
//
/*
class SingleDispatch;

class SingleDispatchException : public BaseException {
  protected:
   SingleDispatch *table;
   string type1;
  public:
   SingleDispatchException(SingleDispatch *_table, string _type1);
   virtual void print(ostream &out = cerr);
   
};


class SingleDispatch {
  public:
   typedef ObjectRef (*single_funct_ptr) (ObjectRef x);

  protected:

   string name;

   typedef TypeMap<single_funct_ptr> vtable1Type;


   vtable1Type vtable;

  public:
   SingleDispatch(string _name) : name(_name) {}

   const string &getName() {return name;}

   void registerFunct(single_funct_ptr ptr, const type_info *x) {
     vtable[x]= ptr;
   }

   ObjectRef call(ObjectRef x) {

      const type_info *t1 = &typeid(*x);

      vtable1Type::iterator v1 = vtable.find(t1);
      if (v1!=vtable.end()) {
	return v1->second(x);
      } else {
	 throw new SingleDispatchException(this, t1->name());
      }
   }
};

#define DEFINE_SINGLE_VTABLE(klass) class klass {                                   \
  public:                                                                           \
   static SingleDispatch &vtable() {static SingleDispatch table(# klass); return table;}  \
   static ObjectRef perform(ObjectRef x)                                            \
   {                                                                                \
      return vtable().call(x);                                                      \
   }                                                                                \
   static int reg(SingleDispatch::single_funct_ptr ptr, const type_info *x)                \
   {                                                                                \
      vtable().registerFunct(ptr,x);                                                \
      return 0;                                                                     \
   }                                                                                \
};

#define REGISTER_SINGLE_VTABLE(klass, func, type1) \
        int dummy_vtable_init_for ## klass ## _ ## func =\
        klass::reg(func, &typeid(type1));
*/





#endif
