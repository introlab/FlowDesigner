// Copyright (C) 2001 Jean-Marc Valin & Dominic Letourneau
// main interface re-written / cleaned.
// Dominic Letourneau (20/08/2001)


#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <locale.h>
#include <gnome.h>
#include "UIDocument.h"
#include "GUIDocument.h"
#include "GUINetwork.h"
#include "BaseException.h"
#include "path.h"
#include <signal.h>
#include <stdlib.h>
#include <sstream>

#include "ExternalApp.h"

class vflowGUI {

  //callbacks
  friend void doc_prop_event(GtkWidget *widget, vflowGUI *vflow);
  friend void new_doc_event  (GtkMenuItem *menuitem, vflowGUI *vflow);
  friend void file_open_ok_sel(GtkWidget *w, vflowGUI *vflow);
  friend void open_doc_event (GtkMenuItem *menuitem, vflowGUI *vflow);
  friend void close_doc_event (GtkMenuItem *menuitem, vflowGUI *vflow);
  friend void file_saveas_ok_sel (GtkWidget *w, vflowGUI *vflow);
  friend void saveas_doc_event (GtkWidget *widget, vflowGUI *vflow);
  friend void save_doc_event (GtkWidget *widget, vflowGUI *vflow);
  friend void export_doc_event (GtkWidget *widget, vflowGUI *vflow);
  friend void run_doc_event (GtkWidget *widget, vflowGUI *vflow);
  friend void stop_doc_event (GtkWidget *widget, vflowGUI *vflow);
  friend void build_event (GtkMenuItem *menuitem, vflowGUI *vflow);
  friend void exit_event  (GtkMenuItem  *menuitem, vflowGUI *vflow);
  friend gint remove_doc_cb (GnomeMDI *mdi, GnomeMDIChild *child);
  friend void overflow_doc_event(GtkMenuItem *menuitem, vflowGUI *vflow);
  friend void overflow_noderef_event(GtkMenuItem *menuitem, vflowGUI *vflow);
  friend void about_event  (GtkMenuItem *menuitem, vflowGUI *vflow);

 public:

  //singleton
  static vflowGUI* instance();

  void set_run_mode (bool isRuning);

  void load_document (const string fname);

  void create_empty_document();

  void copy(GUIDocument *doc);

  void paste (GUIDocument *doc);

  void cut(GUIDocument *doc);

  ~vflowGUI();

  GnomeMDI* get_mdi() {return mdi;}


  static const int CLIPBOARD_COPY_MODE;
  static const int CLIPBOARD_CUT_MODE;

 private:

  GnomeMDI *mdi;

  vflowGUI();

  static vflowGUI* vflowApp;

  void create_mdi();

  list<GUINode*> clipboard;

  int clipboardMode;



};

