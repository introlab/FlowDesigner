// Copyright (C) 2001 Jean-Marc Valin

#include "net_types.h"
#include <stdio.h>
#include <unistd.h>
#include <iostream>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>

//vector<GenericType<int> *> ObjectPool<GenericType<int> >::stack;
//vector<GenericType<double> *> ObjectPool<GenericType<double> >::stack;
//vector<GenericType<float> *> ObjectPool<GenericType<float> >::stack;
//vector<GenericType<FILE *> *> ObjectPool<GenericType<FILE *> >::stack;
//vector<GenericType<bool> *> ObjectPool<GenericType<bool> >::stack;
vector<Int *> ObjectPool<Int>::stack;
vector<Bool *> ObjectPool<Bool>::stack;
vector<Float *> ObjectPool<Float>::stack;
vector<Double *> ObjectPool<Double>::stack;

ObjectRef TrueObject(new Bool(true));
ObjectRef FalseObject(new Bool(false));

FILEPTR::FILEPTR(FILE *file) 
   : GenericType<FILE *> (file)
{}

FILEPTR::FILEPTR(const string &filename, const string &mode) 
   : GenericType<FILE *> (fopen(filename.c_str(), mode.c_str()))
{}

FILEPTR::~FILEPTR() 
{
   fclose(value);
}


FILEDES::FILEDES(int fd) 
   : GenericType<int> (fd)
{}

FILEDES::FILEDES(const string &filename, int mode) 
   : GenericType<int> (open(filename.c_str(), mode))
{}

FILEDES::~FILEDES() 
{
   close(value);
}


