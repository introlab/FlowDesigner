// Copyright (C) 2001 Jean-Marc Valin & Dominic Letourneau
// main interface re-written / cleaned.
// Dominic Letourneau (20/08/2001)

#ifndef VFLOW_H
#define VFLOW_H

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
#include <map>

#include "ExternalApp.h"

class vflowGUI {

  //callbacks
  friend void doc_prop_event(GtkWidget *widget, vflowGUI *vflow);
  friend void new_doc_event  (GtkMenuItem *menuitem, vflowGUI *vflow);
  friend void file_open_ok_sel(GtkWidget *w, vflowGUI *vflow);
  friend void open_doc_event (GtkMenuItem *menuitem, vflowGUI *vflow);
  friend gint close_doc_event (GtkMenuItem *menuitem, vflowGUI *vflow);
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

  //notebook events
  friend void vflow_change_current_page_event(GtkNotebook *notebook, gint arg1, vflowGUI *vflow);
  friend gboolean vflow_focus_tab_event(GtkNotebook *notebook, GtkNotebookTab arg1, vflowGUI *vflow);
  friend gboolean vflow_select_page_event(GtkNotebook *notebook, gboolean arg1, vflowGUI *vflow);
  


 public:

  //singleton
  static vflowGUI* instance();

  void set_run_mode (bool isRuning);

  void load_document (const string fname);

  void create_empty_document();

  void add_notebook_document(GUIDocument *doc, GtkWidget *child);
  
  void remove_notebook_document(GUIDocument *doc, GtkWidget *child);

  void display_statusbar_text(const string &text);

  void copy(GUIDocument *doc);

  void paste (GUIDocument *doc);

  void cut(GUIDocument *doc);

  void clear(GUIDocument *doc);

  ~vflowGUI();

  GtkWidget* get_mdi() {return mdi;}
 
  GUIDocument *getCurrentDoc();

  static const int CLIPBOARD_COPY_MODE;
  static const int CLIPBOARD_CUT_MODE;
  static const int CLIPBOARD_NONE_MODE;

 private:

  GtkWidget *mdi;
  GtkWidget *notebook1;
  GtkWidget *button5;
  GtkWidget *button6;
 

  vflowGUI();

  static vflowGUI* vflowApp;

  void create_mdi();

  list<GUINode*> clipboard;

  int clipboardMode;

  GtkWidget* get_notebook() {return notebook1;}

  


};

#endif
