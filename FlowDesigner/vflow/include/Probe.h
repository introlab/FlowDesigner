// Copyright (C) 2001 Jean-Marc Valin

#ifndef PROBE_H
#define PROBE_H


#include "Node.h"
#include <gnome.h>
#include <pthread.h>

class Probe : public Node {
  protected:

   ObjectRef inputValue;

   /**The ID of the 'output' output*/
   int outputID;

   /**The ID of the 'output' output*/
   int inputID;

   GtkWidget *window1;
   GtkWidget *toolbar2;
   GtkWidget *button16;
   GtkWidget *button17;
   GtkWidget *button18;
   GtkWidget *button19;
   GtkWidget *vbox2;
   GtkWidget *entry1;

   pthread_mutex_t mutex;
   pthread_cond_t cond;
   int nbClick;
   //sem_t sem;

   int breakAt;

   int skip;

   bool exit_status;

   string probeName;
   
   bool traceEnable;

   bool displayEnable;
  public:
   Probe(string nodeName, ParameterSet params);

   virtual ~Probe();

   /**Class specific initialization routine.
      Each class will call its subclass initialize() method*/
   virtual void initialize();

   /**Class reset routine.
      Each class will call its superclass reset() method*/
   virtual void reset();

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count); 

   /**Standard request-passing method between nodes during initialization*/
   virtual void request(int outputID, const ParameterSet &req) {
     inputs[inputID].node->request(inputs[inputID].outputID,req);
   }

   virtual void trace();
   
   virtual void next();

   virtual void cont();
      
   virtual void setBreak();

   virtual void display();

   virtual void show_hide();

   virtual void stop();
protected:
   /**Default constructor, should not be used*/
   Probe() {throw new GeneralException("Probe copy constructor should not be called",__FILE__,__LINE__);}

};

#endif

