#include <string>
#include <map>
#include "Object.h"

class SymbolSet {
  protected:
   int currentID;
   map<string,int> translationMap;
  public:
   SymbolSet()
      : currentID(0)
   {}
   int get(const string &str)
   {
      map<string,int>::iterator sym = translationMap.find(str);
      if (sym == translationMap.end())
      {
	 translationMap.insert(make_pair(str, currentID++));
	 return currentID-1;
      } else {
	 return sym->second;
      }
   }

   int get (char *str)
   {
      return get(string(str));
   }

   string reverseLookup (int ID)
   {
      map<string,int>::iterator it = translationMap.begin();
      while (it!=translationMap.end())
      {
	 if (it->second == ID)
	    return it->first;
	 ++it;
      }
      return string("");
   }
};



class VirtualMethods {
  protected:
   SymbolSet* symbols;

   typedef ObjectRef (*funct_ptr0) (ObjectRef x);
   typedef ObjectRef (*funct_ptr1) (ObjectRef x, ObjectRef a);
   typedef ObjectRef (*funct_ptr2) (ObjectRef x, ObjectRef a, ObjectRef b);
   typedef ObjectRef (*funct_ptr3) (ObjectRef x, ObjectRef a, ObjectRef b, ObjectRef c);

   typedef map<const type_info *, funct_ptr0> vtableType0;
   typedef map<const type_info *, funct_ptr1> vtableType1;
   typedef map<const type_info *, funct_ptr2> vtableType2;
   typedef map<const type_info *, funct_ptr3> vtableType3;

   vector<vtableType0> tables0;
   vector<vtableType1> tables1;
   vector<vtableType2> tables2;
   vector<vtableType3> tables3;
  public:
   VirtualMethods() 
   {
      symbols=new SymbolSet;
   }

   int lookup(const string &str)
   {
      return symbols->get(str);
   }

   int lookup (char *str)
   {
      return symbols->get(str);
   }

   int registerFunct0(funct_ptr0 ptr, const type_info *x, string name)
   {
      int id = symbols->get(name);
      if (id >= tables0.size())
	 tables0.resize(id+1);
      tables0[id][x] = ptr;
   }

   int registerFunct1(funct_ptr1 ptr, const type_info *x, string name)
   {
      int id = symbols->get(name);
      if (id >= tables1.size())
	 tables1.resize(id+1);
      tables1[id][x] = ptr;
   }

   int registerFunct2(funct_ptr2 ptr, const type_info *x, string name)
   {
      int id = symbols->get(name);
      if (id >= tables2.size())
	 tables2.resize(id+1);
      tables2[id][x] = ptr;
   }

   int registerFunct3(funct_ptr3 ptr, const type_info *x, string name)
   {
      int id = symbols->get(name);
      if (id >= tables3.size())
	 tables3.resize(id+1);
      tables3[id][x] = ptr;
   }

   ObjectRef call(int id, ObjectRef x)
   {
      const type_info *t1 = &typeid(*x);
      vtableType0 &vtable=tables0[id];
      vtableType0::iterator v1 = vtable.find(t1);
      if (v1!=vtable.end())
      {
	 return v1->second(x);
      } else {
	 cerr << "doesnotunderstand...\n";
	 cerr << "reverse = " << symbols->reverseLookup(id) << endl;
	 try {
	 x->doesNotUnderstand(symbols->reverseLookup(id));
	 } catch (...)
	    {
	       cerr << "caught\n"; throw;
	    }
	 //throw new GeneralException("Virtual function error", __FILE__, __LINE__);
      }
   }

   ObjectRef call(int id, ObjectRef x, ObjectRef y)
   {
      const type_info *t1 = &typeid(*x);
      vtableType1::iterator v1 = tables1[id].find(t1);
      if (v1!=tables1[id].end())
      {
	 return v1->second(x,y);
      } else {
	 x->doesNotUnderstand(symbols->reverseLookup(id));
	 //throw new GeneralException("Virtual function error", __FILE__, __LINE__);
      }
   }
   
};


//extern VirtualMethods* vmethod;
VirtualMethods* vmethod();

#define REGISTER_VTABLE0(name, type, func, id) \
        static int dummy_vtable_init_for ## _ ## name ## id =\
        vmethod()->registerFunct0(func, &typeid(type), # name);
