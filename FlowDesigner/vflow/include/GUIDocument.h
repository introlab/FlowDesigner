#ifndef GUIDOCUMENT_H
#define GUIDOCUMENT_H

#include "UIDocument.h"
#include "GUINetPopup.h"
#include <gnome.h>
#include <pthread.h>

class DocParameterData {
public:

   GtkWidget *optionmenu;
   GtkWidget *optionmenu_menu;
   GtkWidget *combo_entry;
   GtkWidget *entry;
   GtkWidget *label;

   };



class GUIDocument : public UIDocument {
protected:
   
   GnomeMDIGenericChild *mdiChild;

   //GtkWidget *view;
   GtkWidget *docproperty;

   
   GtkWidget *vbox2;
   GtkWidget *notebook1;
   GtkWidget *less2;
   string less_text;

   vector<DocParameterData> params;
public:
   static pthread_t runThread;
   static bool isRunning;
   static Network *runningNet;
public:
   GUIDocument(string _name);

   ~GUIDocument() {};
   
   GnomeMDIGenericChild *getMDIChild() {return mdiChild;}

   GtkWidget *getView() {return vbox2;}

   GtkWidget *getNotebook() {return notebook1;}

   void less_print(const string &message);

   void less_print(const char *message);

   void less_clear();

   void create();

   GtkWidget *createView();

   virtual void load();

   void removeCurrentNet ();

   UINetwork *newNetwork(UIDocument *_doc, const string &_name, UINetwork::Type type);
   
   UINetwork *newNetwork(UIDocument *_doc, xmlNodePtr _net);

   virtual void setFullPath(const string &fullpath);

   virtual void run();

   void createParamDialog();

   void showParams();

   void changedParams();

   void applyParams();

   void insertLoadedParam(DocParameterData *param, string type, string value);

   void threadRun();
   
   friend class GUINetPopup;

   static void threadStop();
};


#endif
