#ifndef PROBE_H
#define PROBE_H


#include "Node.h"
#include <gnome.h>
#include <semaphore.h>
#include <pthread.h>

class Probe : public Node {
  protected:

   ObjectRef inputValue;

   /**The ID of the 'output' output*/
   int outputID;

   /**The ID of the 'output' output*/
   int inputID;

   GtkWidget *window1;
   GtkWidget *button16;
   GtkWidget *button17;
   GtkWidget *button18;
   GtkWidget *button19;
   GtkWidget *vbox2;
   GtkWidget *entry1;

   pthread_mutex_t mutex;
   //sem_t sem;

   int breakAt;

   int skip;

   bool traceEnable;

   bool displayEnable;
  public:
   Probe(string nodeName, ParameterSet params);

   virtual ~Probe();

   /**Class specific initialization routine.
      Each class will call its subclass specificInitialize() method*/
   virtual void specificInitialize();

   /**Class reset routine.
      Each class will call its superclass reset() method*/
   virtual void reset();

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count); 

   /**Standard request-passing method between nodes during initialization*/
   virtual void request(int outputID, const ParameterSet &req) {inputs[inputID].node->request(inputs[inputID].outputID,req);}

   virtual void trace();
   
   virtual void next();

   virtual void cont();
      
   virtual void setBreak();

   virtual void display();

   virtual void show_hide();

protected:
   /**Default constructor, should not be used*/
   Probe() {throw new GeneralException("Probe copy constructor should not be called",__FILE__,__LINE__);}

};

#endif

