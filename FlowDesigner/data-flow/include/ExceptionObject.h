// Copyright (C) 2001 Jean-Marc Valin

#ifndef EXCEPTION_OBJECT_H
#define EXCEPTION_OBJECT_H

class ExceptionObject : public Object {

   BaseException *e;
public:

   ExceptionObject(BaseException *_e)
      : e(_e)
   {
      
   }

   void doThrow()
   {
      throw e;
   }

   void printOn(std::ostream &out) const
   {
      e->print(out);
   }
};

#endif
