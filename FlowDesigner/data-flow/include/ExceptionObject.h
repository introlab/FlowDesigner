// Copyright (C) 2001 Jean-Marc Valin

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
};
