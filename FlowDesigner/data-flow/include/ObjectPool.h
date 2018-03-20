// Copyright (C) 1999 Jean-Marc Valin

#ifndef OBJECT_POOL_H
#define OBJECT_POOL_H

#include <vector>
#include "multithread.h"

namespace FD {

#define MAX_STORE 100

template <class T>
class ObjectPool {
protected:

    static std::vector <T *> stack;
    static FastMutex mutex;


public:
    ObjectPool()
    {
        //Do not put anything there... everything is static!
    }

    ~ObjectPool()
    {
        //Do not put anything there... everything is static!
    }



    static T *alloc()
    {
        mutex.lock();
        if (stack.size())
        {
            T *ret = stack.back();
            stack.pop_back();
            ret->ref();
            mutex.unlock();
            return ret;
        } else {
            mutex.unlock();
            return new T;
        }
    }

    static void release(T *obj)
    {
        mutex.lock();
        if (stack.size() > MAX_STORE)
        {
            delete obj;
        } else {
            stack.push_back(obj);
        }
        mutex.unlock();
    }

};
}//namespace FD
#endif
