#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>

#include <gnome.h>
#include "GUIDocument.h"
#include <sstream>
#include "BaseException.h"

class CodeGenState {
   GtkWidget* dialog;
   GUIDocument *doc;
   GtkWidget *fileentry1;
   GtkWidget *combo_entry1;
public:
   CodeGenState(GUIDocument *_doc);
      ~CodeGenState() {gtk_widget_destroy(dialog);}
   void ok();
   void cancel() {}
};

void CodeGenState::ok()
{
   try {
      char *filename=gnome_file_entry_get_full_path(GNOME_FILE_ENTRY(fileentry1),FALSE);
      if (!filename || !strlen(filename))
	 filename = "vflow_code.cc";

      char *funcname = gtk_entry_get_text(GTK_ENTRY(combo_entry1));
      cerr << strlen(funcname);
      if (!funcname || !strlen(funcname))
	 funcname = "buildDoc";

      //cerr << "file = " << filename <<endl;
      //cerr << "function = " << funcname << endl;

      doc->less_print("Warning: automatic code generation is still experimental");
      ofstream out(filename);
      doc->genCode(out, funcname);

      doc->less_print(string("C++ code generated in '") + filename 
		      + "', build function name is '" + funcname + "'");

   } catch (BaseException *e)
   {
      ostringstream str;
      e->print(str);
      doc->less_print(str.str());
   }
}

void GUIDocument_codegen(GUIDocument *doc)
{
   new CodeGenState(doc);
}

static void codegen_ok(GtkButton *button, CodeGenState *gen)
{
   gen->ok();
   delete gen;
}

static void codegen_cancel(GtkButton *button, CodeGenState *gen)
{
   gen->cancel();
   delete gen;
}

CodeGenState::CodeGenState(GUIDocument *_doc)
   : doc(_doc)
{
  GtkWidget *window1;
  GtkWidget *vbox1;
  GtkWidget *label1;
  GtkWidget *hseparator1;
  GtkWidget *table1;
  GtkWidget *entry1;
  GtkWidget *combo_entry2;
  GtkWidget *label2;
  GtkWidget *label3;
  GtkWidget *vbox2;
  GSList *vbox2_group = NULL;
  GtkWidget *radiobutton1;
  GtkWidget *radiobutton2;
  GtkWidget *radiobutton3;
  GtkWidget *hseparator2;
  GtkWidget *hbuttonbox1;
  GtkWidget *button1;
  GtkWidget *button2;

  window1 = gtk_window_new (GTK_WINDOW_TOPLEVEL);

  dialog=window1;

  gtk_object_set_data (GTK_OBJECT (window1), "window1", window1);
  gtk_window_set_title (GTK_WINDOW (window1), _("Overflow Code Generator"));

  vbox1 = gtk_vbox_new (FALSE, 0);
  gtk_widget_ref (vbox1);
  gtk_object_set_data_full (GTK_OBJECT (window1), "vbox1", vbox1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (vbox1);
  gtk_container_add (GTK_CONTAINER (window1), vbox1);

  label1 = gtk_label_new (_("Code generator"));
  gtk_widget_ref (label1);
  gtk_object_set_data_full (GTK_OBJECT (window1), "label1", label1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label1);
  gtk_box_pack_start (GTK_BOX (vbox1), label1, FALSE, FALSE, 0);

  hseparator1 = gtk_hseparator_new ();
  gtk_widget_ref (hseparator1);
  gtk_object_set_data_full (GTK_OBJECT (window1), "hseparator1", hseparator1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (hseparator1);
  gtk_box_pack_start (GTK_BOX (vbox1), hseparator1, TRUE, TRUE, 0);

  table1 = gtk_table_new (2, 2, FALSE);
  gtk_widget_ref (table1);
  gtk_object_set_data_full (GTK_OBJECT (window1), "table1", table1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (table1);
  gtk_box_pack_start (GTK_BOX (vbox1), table1, TRUE, TRUE, 0);

  entry1 = gnome_entry_new (NULL);
  gtk_widget_ref (entry1);
  gtk_object_set_data_full (GTK_OBJECT (window1), "entry1", entry1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (entry1);
  gtk_table_attach (GTK_TABLE (table1), entry1, 1, 2, 0, 1,
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);

  combo_entry1 = gnome_entry_gtk_entry (GNOME_ENTRY (entry1));
  gtk_widget_ref (combo_entry1);
  gtk_object_set_data_full (GTK_OBJECT (window1), "combo_entry1", combo_entry1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (combo_entry1);

  fileentry1 = gnome_file_entry_new (NULL, NULL);
  gtk_widget_ref (fileentry1);
  gtk_object_set_data_full (GTK_OBJECT (window1), "fileentry1", fileentry1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (fileentry1);
  gtk_table_attach (GTK_TABLE (table1), fileentry1, 1, 2, 1, 2,
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);

  combo_entry2 = gnome_file_entry_gtk_entry (GNOME_FILE_ENTRY (fileentry1));
  gtk_widget_ref (combo_entry2);
  gtk_object_set_data_full (GTK_OBJECT (window1), "combo_entry2", combo_entry2,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (combo_entry2);

  label2 = gtk_label_new (_("Build function name"));
  gtk_widget_ref (label2);
  gtk_object_set_data_full (GTK_OBJECT (window1), "label2", label2,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label2);
  gtk_table_attach (GTK_TABLE (table1), label2, 0, 1, 0, 1,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  gtk_misc_set_alignment (GTK_MISC (label2), 0, 0.5);

  label3 = gtk_label_new (_("Output file"));
  gtk_widget_ref (label3);
  gtk_object_set_data_full (GTK_OBJECT (window1), "label3", label3,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label3);
  gtk_table_attach (GTK_TABLE (table1), label3, 0, 1, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  gtk_misc_set_alignment (GTK_MISC (label3), 0, 0.5);

  vbox2 = gtk_vbox_new (FALSE, 0);
  gtk_widget_ref (vbox2);
  gtk_object_set_data_full (GTK_OBJECT (window1), "vbox2", vbox2,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (vbox2);
  gtk_box_pack_start (GTK_BOX (vbox1), vbox2, TRUE, TRUE, 0);

  radiobutton1 = gtk_radio_button_new_with_label (vbox2_group, _("Build function only"));
  vbox2_group = gtk_radio_button_group (GTK_RADIO_BUTTON (radiobutton1));
  gtk_widget_ref (radiobutton1);
  gtk_object_set_data_full (GTK_OBJECT (window1), "radiobutton1", radiobutton1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (radiobutton1);
  gtk_box_pack_start (GTK_BOX (vbox2), radiobutton1, FALSE, FALSE, 0);

  radiobutton2 = gtk_radio_button_new_with_label (vbox2_group, _("Generate main"));
  vbox2_group = gtk_radio_button_group (GTK_RADIO_BUTTON (radiobutton2));
  gtk_widget_ref (radiobutton2);
  gtk_object_set_data_full (GTK_OBJECT (window1), "radiobutton2", radiobutton2,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (radiobutton2);
  gtk_box_pack_start (GTK_BOX (vbox2), radiobutton2, FALSE, FALSE, 0);

  radiobutton3 = gtk_radio_button_new_with_label (vbox2_group, _("Generate executable"));
  vbox2_group = gtk_radio_button_group (GTK_RADIO_BUTTON (radiobutton3));
  gtk_widget_ref (radiobutton3);
  gtk_object_set_data_full (GTK_OBJECT (window1), "radiobutton3", radiobutton3,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (radiobutton3);
  gtk_box_pack_start (GTK_BOX (vbox2), radiobutton3, FALSE, FALSE, 0);

  hseparator2 = gtk_hseparator_new ();
  gtk_widget_ref (hseparator2);
  gtk_object_set_data_full (GTK_OBJECT (window1), "hseparator2", hseparator2,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (hseparator2);
  gtk_box_pack_start (GTK_BOX (vbox1), hseparator2, TRUE, TRUE, 0);

  hbuttonbox1 = gtk_hbutton_box_new ();
  gtk_widget_ref (hbuttonbox1);
  gtk_object_set_data_full (GTK_OBJECT (window1), "hbuttonbox1", hbuttonbox1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (hbuttonbox1);
  gtk_box_pack_start (GTK_BOX (vbox1), hbuttonbox1, TRUE, TRUE, 0);
  gtk_button_box_set_layout (GTK_BUTTON_BOX (hbuttonbox1), GTK_BUTTONBOX_END);

  button1 = gnome_stock_button (GNOME_STOCK_BUTTON_OK);
  gtk_widget_ref (button1);
  gtk_object_set_data_full (GTK_OBJECT (window1), "button1", button1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (button1);
  gtk_container_add (GTK_CONTAINER (hbuttonbox1), button1);
  GTK_WIDGET_SET_FLAGS (button1, GTK_CAN_DEFAULT);

     gtk_signal_connect (GTK_OBJECT (button1), "clicked",
                        GTK_SIGNAL_FUNC( codegen_ok), this);


  button2 = gnome_stock_button (GNOME_STOCK_BUTTON_CANCEL);
  gtk_widget_ref (button2);
  gtk_object_set_data_full (GTK_OBJECT (window1), "button2", button2,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (button2);
  gtk_container_add (GTK_CONTAINER (hbuttonbox1), button2);
  GTK_WIDGET_SET_FLAGS (button2, GTK_CAN_DEFAULT);

     gtk_signal_connect (GTK_OBJECT (button2), "clicked",
                        GTK_SIGNAL_FUNC( codegen_cancel), this);


     gtk_widget_show(dialog);
  //return window1;
}

