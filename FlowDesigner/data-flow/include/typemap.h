#ifndef TYPE_MAP_H
#define TYPE_MAP_H

#include <algorithm>
#include <map>
#include <typeinfo>

using namespace std;

//Yes, this is yet another Visual C++ bug. It seems like the type_info::before() 
//method is buggy, or it could be the STL map, I don't know. All I know is that
//the follwing code doesn't work under MSVC++. That's why there's a (ugly) workaround.
#ifndef WIN32

struct compare_const_type_info_ptr : public binary_function<const type_info *, const type_info *, bool>
{
   bool operator()(const type_info *lhs, const type_info *rhs) const
   {
      return lhs->before(*rhs);
   }
};

template<typename T>
class TypeMap : public map<const type_info *, T, compare_const_type_info_ptr >
{
};

#else


#include <vector>


template<typename T>
class TypeMap {
  public:
   typedef pair<const type_info *, T> element;
   typedef element *iterator;
   typedef const element *const_iterator;
  private:
   vector<element> tmap;
  public:
   
   const_iterator begin() const {return tmap.begin();}
   iterator begin() {return tmap.begin();}

   const_iterator end() const {return tmap.end();}
   iterator end() {return tmap.end();}

   iterator find(const type_info *t) 
   {
      for (int i=0;i<tmap.size();i++)
	 if (*t == *(tmap[i].first))
	    return &tmap[i];
      return end();
   }

   const_iterator find(const type_info *t) const
   {
      for (int i=0;i<tmap.size();i++)
	 if (*t == *(tmap[i].first))
	    return &tmap[i];
      return end();
   }

   T &operator[] (const type_info *t) 
   {
      iterator it = find(t);
      if (it != end())
	 return it->second;
      else
      {
	 element it;
	 it.first = t;
	 tmap.push_back(it);
	 return tmap[tmap.size()-1].second;
      }
   }

   
};



#endif


#endif
