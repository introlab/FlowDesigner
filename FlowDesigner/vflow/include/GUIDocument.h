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



class GUIDocument : public GnomeMDIChild , public UIDocument {
protected:

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





class GUIDocument_class {
   GnomeMDIChildClass parent_class;
   
   void (*document_changed)(GUIDocument *, gpointer);
};

//void UIDocument_class_init(UIDocument_class *);
//void UIDocument_init(UIDocument *);
//GtkType UIDocument_get_type ();

#define GUIDOCUMENT(obj)	         GTK_CHECK_CAST (obj, GUIDocument_get_type (), GUIDocument)
#define GUIDOCUMENT_CLASS(klass)  GTK_CHECK_CLASS_CAST (klass, GUIDocument_get_type (), GUIDocument_class)
#define IS_GUIDOCUMENT(obj)	 GTK_CHECK_TYPE (obj, GUIDocument_get_type ())
GtkType GUIDocument_get_type ();
GUIDocument *GUIDocument_new ();

#endif
