#include "misc_gui.h"
#include <gnome.h>
#include "vflow.h"

using namespace std;

namespace FD {

#define GLADE_HOOKUP_OBJECT(component,widget,name) \
  g_object_set_data_full (G_OBJECT (component), name, \
    gtk_widget_ref (widget), (GDestroyNotify) gtk_widget_unref)

#define GLADE_HOOKUP_OBJECT_NO_REF(component,widget,name) \
  g_object_set_data (G_OBJECT (component), name, widget)


string ask_string_dialog (const char *question, const char *default_str)
{
 GtkWidget *dialog1;
  GtkWidget *dialog_vbox1;
  GtkWidget *vbox1;
  GtkWidget *hbox1;
  GtkWidget *image1;
  GtkWidget *label1;
  GtkWidget *entry1;
  GtkWidget *dialog_action_area1;
  GtkWidget *cancelbutton1;
  GtkWidget *okbutton1;

  dialog1 = gtk_dialog_new ();
  gtk_window_set_title (GTK_WINDOW (dialog1), _("dialog1"));

  gtk_window_set_transient_for(GTK_WINDOW(dialog1), GTK_WINDOW(vflowGUI::instance()->get_mdi()));
  gtk_window_set_position (GTK_WINDOW(dialog1), GTK_WIN_POS_CENTER_ON_PARENT);

  dialog_vbox1 = GTK_DIALOG (dialog1)->vbox;
  gtk_widget_show (dialog_vbox1);

  vbox1 = gtk_vbox_new (FALSE, 0);
  gtk_widget_show (vbox1);
  gtk_box_pack_start (GTK_BOX (dialog_vbox1), vbox1, TRUE, TRUE, 0);

  hbox1 = gtk_hbox_new (FALSE, 0);
  gtk_widget_show (hbox1);
  gtk_box_pack_start (GTK_BOX (vbox1), hbox1, TRUE, TRUE, 0);

  image1 = gtk_image_new_from_stock ("gtk-dialog-question", GTK_ICON_SIZE_DIALOG);
  gtk_widget_show (image1);
  gtk_box_pack_start (GTK_BOX (hbox1), image1, TRUE, TRUE, 0);

  label1 = gtk_label_new (question);
  gtk_widget_show (label1);
  gtk_box_pack_start (GTK_BOX (hbox1), label1, FALSE, FALSE, 0);
  gtk_label_set_justify (GTK_LABEL (label1), GTK_JUSTIFY_LEFT);

  entry1 = gtk_entry_new ();
  gtk_widget_show (entry1);
  gtk_box_pack_start (GTK_BOX (vbox1), entry1, FALSE, FALSE, 0);
  gtk_entry_set_text(GTK_ENTRY(entry1), default_str);

  dialog_action_area1 = GTK_DIALOG (dialog1)->action_area;
  gtk_widget_show (dialog_action_area1);
  gtk_button_box_set_layout (GTK_BUTTON_BOX (dialog_action_area1), GTK_BUTTONBOX_END);

  cancelbutton1 = gtk_button_new_from_stock ("gtk-cancel");
  gtk_widget_show (cancelbutton1);
  gtk_dialog_add_action_widget (GTK_DIALOG (dialog1), cancelbutton1, GTK_RESPONSE_CANCEL);
  GTK_WIDGET_SET_FLAGS (cancelbutton1, GTK_CAN_DEFAULT);

  okbutton1 = gtk_button_new_from_stock ("gtk-ok");
  gtk_widget_show (okbutton1);
  gtk_dialog_add_action_widget (GTK_DIALOG (dialog1), okbutton1, GTK_RESPONSE_OK);
  GTK_WIDGET_SET_FLAGS (okbutton1, GTK_CAN_DEFAULT);

  /* Store pointers to all widgets, for use by lookup_widget(). */
  GLADE_HOOKUP_OBJECT_NO_REF (dialog1, dialog1, "dialog1");
  GLADE_HOOKUP_OBJECT_NO_REF (dialog1, dialog_vbox1, "dialog_vbox1");
  GLADE_HOOKUP_OBJECT (dialog1, vbox1, "vbox1");
  GLADE_HOOKUP_OBJECT (dialog1, hbox1, "hbox1");
  GLADE_HOOKUP_OBJECT (dialog1, image1, "image1");
  GLADE_HOOKUP_OBJECT (dialog1, label1, "label1");
  GLADE_HOOKUP_OBJECT (dialog1, entry1, "entry1");
  GLADE_HOOKUP_OBJECT_NO_REF (dialog1, dialog_action_area1, "dialog_action_area1");
  GLADE_HOOKUP_OBJECT (dialog1, cancelbutton1, "cancelbutton1");
  GLADE_HOOKUP_OBJECT (dialog1, okbutton1, "okbutton1");

  gtk_object_set_data(GTK_OBJECT(dialog1), "entry", entry1);
  gtk_widget_show (dialog1);
  //return dialog1;

   gint result = gtk_dialog_run (GTK_DIALOG (dialog1));
   string str;
   switch (result)
   {
   case GTK_RESPONSE_OK:
      {
         GtkWidget *entry = (GtkWidget*)gtk_object_get_data(GTK_OBJECT(dialog1), "entry");
         str = (const char*)gtk_entry_get_text(GTK_ENTRY(entry));
         
      }
      break;
   default:
      break;
   }
   gtk_widget_destroy (dialog1);

   return str;
}


gint close_save_dialog (const char *question)
{
  GtkWidget *dialog1;
  GtkWidget *dialog_vbox1;
  GtkWidget *hbox1;
  GtkWidget *image1;
  GtkWidget *label2;
  GtkWidget *dialog_action_area1;
  GtkWidget *cancelbutton1;
  GtkWidget *alignment1;
  GtkWidget *hbox2;
  GtkWidget *image2;
  GtkWidget *label3;
  GtkWidget *okbutton1;
  GtkWidget *button11;

  dialog1 = gtk_dialog_new ();
  gtk_window_set_title (GTK_WINDOW (dialog1), _("dialog1"));

  gtk_window_set_transient_for(GTK_WINDOW(dialog1), GTK_WINDOW(vflowGUI::instance()->get_mdi()));
  gtk_window_set_position (GTK_WINDOW(dialog1), GTK_WIN_POS_CENTER_ON_PARENT);
  //gtk_window_set_position (GTK_WINDOW(dialog1), GTK_WIN_POS_CENTER);

  dialog_vbox1 = GTK_DIALOG (dialog1)->vbox;
  gtk_widget_show (dialog_vbox1);

  hbox1 = gtk_hbox_new (FALSE, 0);
  gtk_widget_show (hbox1);
  gtk_box_pack_start (GTK_BOX (dialog_vbox1), hbox1, TRUE, TRUE, 0);

  image1 = gtk_image_new_from_stock ("gtk-dialog-question", GTK_ICON_SIZE_DIALOG
);
  gtk_widget_show (image1);
  gtk_box_pack_start (GTK_BOX (hbox1), image1, TRUE, TRUE, 0);

  label2 = gtk_label_new (question);
  gtk_widget_show (label2);
  gtk_box_pack_start (GTK_BOX (hbox1), label2, FALSE, FALSE, 0);
  gtk_label_set_justify (GTK_LABEL (label2), GTK_JUSTIFY_LEFT);
  gtk_misc_set_padding (GTK_MISC (label2), 0, 32);

  dialog_action_area1 = GTK_DIALOG (dialog1)->action_area;
  gtk_widget_show (dialog_action_area1);
  gtk_button_box_set_layout (GTK_BUTTON_BOX (dialog_action_area1), GTK_BUTTONBOX_END);

  cancelbutton1 = gtk_button_new ();
  gtk_widget_show (cancelbutton1);
  gtk_dialog_add_action_widget (GTK_DIALOG (dialog1), cancelbutton1, GTK_RESPONSE_NO);
  GTK_WIDGET_SET_FLAGS (cancelbutton1, GTK_CAN_DEFAULT);

  alignment1 = gtk_alignment_new (0.5, 0.5, 0, 0);
  gtk_widget_show (alignment1);
  gtk_container_add (GTK_CONTAINER (cancelbutton1), alignment1);

  hbox2 = gtk_hbox_new (FALSE, 2);
  gtk_widget_show (hbox2);
  gtk_container_add (GTK_CONTAINER (alignment1), hbox2);

  image2 = gtk_image_new_from_stock ("gtk-no", GTK_ICON_SIZE_BUTTON);
  gtk_widget_show (image2);
  gtk_box_pack_start (GTK_BOX (hbox2), image2, FALSE, FALSE, 0);

  label3 = gtk_label_new_with_mnemonic (_("Don't Save"));
  gtk_widget_show (label3);
  gtk_box_pack_start (GTK_BOX (hbox2), label3, FALSE, FALSE, 0);
  gtk_label_set_justify (GTK_LABEL (label3), GTK_JUSTIFY_LEFT);

  okbutton1 = gtk_button_new_from_stock ("gtk-cancel");
  gtk_widget_show (okbutton1);
  gtk_dialog_add_action_widget (GTK_DIALOG (dialog1), okbutton1, GTK_RESPONSE_CANCEL);
  GTK_WIDGET_SET_FLAGS (okbutton1, GTK_CAN_DEFAULT);

  button11 = gtk_button_new_from_stock ("gtk-save");
  gtk_widget_show (button11);
  gtk_dialog_add_action_widget (GTK_DIALOG (dialog1), button11, GTK_RESPONSE_ACCEPT);
  GTK_WIDGET_SET_FLAGS (button11, GTK_CAN_DEFAULT);

  /* Store pointers to all widgets, for use by lookup_widget(). */
  GLADE_HOOKUP_OBJECT_NO_REF (dialog1, dialog1, "dialog1");
  GLADE_HOOKUP_OBJECT_NO_REF (dialog1, dialog_vbox1, "dialog_vbox1");
  GLADE_HOOKUP_OBJECT (dialog1, hbox1, "hbox1");
  GLADE_HOOKUP_OBJECT (dialog1, image1, "image1");
  GLADE_HOOKUP_OBJECT (dialog1, label2, "label2");
  GLADE_HOOKUP_OBJECT_NO_REF (dialog1, dialog_action_area1, "dialog_action_area1");
  GLADE_HOOKUP_OBJECT (dialog1, cancelbutton1, "cancelbutton1");
  GLADE_HOOKUP_OBJECT (dialog1, alignment1, "alignment1");
  GLADE_HOOKUP_OBJECT (dialog1, hbox2, "hbox2");
  GLADE_HOOKUP_OBJECT (dialog1, image2, "image2");
  GLADE_HOOKUP_OBJECT (dialog1, label3, "label3");
  GLADE_HOOKUP_OBJECT (dialog1, okbutton1, "okbutton1");
  GLADE_HOOKUP_OBJECT (dialog1, button11, "button11");

  //return dialog1;

  gtk_widget_show (dialog1);
  //return dialog1;

  gint ret = gtk_dialog_run (GTK_DIALOG (dialog1));
  gtk_widget_destroy (dialog1);
  return ret;
}

}//namespace FD
