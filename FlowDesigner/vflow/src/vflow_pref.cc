// Copyright (C) 2001 Jean-Marc Valin & Dominic Letourneau

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <locale.h>
#include <gnome.h>

GtkWidget *create_propertybox1 ()
{
  GtkWidget *propertybox1;
  GtkWidget *notebook1;
  GtkWidget *vbox3;
  GtkWidget *frame1;
  GtkWidget *vbox4;
  GtkWidget *checkbutton2;
  GtkWidget *checkbutton3;
  GtkWidget *frame2;
  GtkWidget *vbox5;
  GtkWidget *checkbutton4;
  GtkWidget *checkbutton5;
  GtkWidget *label1;
  GtkWidget *vbox1;
  GtkWidget *mdiframe;
  GtkWidget *table2;
  GtkWidget *label7;
  GtkWidget *label8;
  GtkWidget *optionmenu1;
  GtkWidget *optionmenu1_menu;
  GtkWidget *glade_menuitem;
  GtkWidget *optionmenu2;
  GtkWidget *optionmenu2_menu;
  GtkWidget *label2;
  GtkWidget *vbox2;
  GtkWidget *colorframe;
  GtkWidget *table1;
  GtkWidget *colorpicker1;
  GtkWidget *colorpicker2;
  GtkWidget *colorpicker3;
  GtkWidget *label4;
  GtkWidget *label5;
  GtkWidget *label6;
  GtkWidget *label3;

  propertybox1 = gnome_property_box_new ();
  gtk_object_set_data (GTK_OBJECT (propertybox1), "propertybox1", propertybox1);
  gtk_window_set_policy (GTK_WINDOW (propertybox1), FALSE, FALSE, FALSE);

  notebook1 = GNOME_PROPERTY_BOX (propertybox1)->notebook;
  gtk_object_set_data (GTK_OBJECT (propertybox1), "notebook1", notebook1);
  gtk_widget_show (notebook1);

  vbox3 = gtk_vbox_new (FALSE, 0);
  gtk_widget_ref (vbox3);
  gtk_object_set_data_full (GTK_OBJECT (propertybox1), "vbox3", vbox3,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (vbox3);
  gtk_container_add (GTK_CONTAINER (notebook1), vbox3);

  frame1 = gtk_frame_new (_("Display"));
  gtk_widget_ref (frame1);
  gtk_object_set_data_full (GTK_OBJECT (propertybox1), "frame1", frame1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (frame1);
  gtk_box_pack_start (GTK_BOX (vbox3), frame1, TRUE, TRUE, 0);

  vbox4 = gtk_vbox_new (FALSE, 0);
  gtk_widget_ref (vbox4);
  gtk_object_set_data_full (GTK_OBJECT (propertybox1), "vbox4", vbox4,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (vbox4);
  gtk_container_add (GTK_CONTAINER (frame1), vbox4);

  checkbutton2 = gtk_check_button_new_with_label (_("Show all Input/Output names"));
  gtk_widget_ref (checkbutton2);
  gtk_object_set_data_full (GTK_OBJECT (propertybox1), "checkbutton2", checkbutton2,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (checkbutton2);
  gtk_box_pack_start (GTK_BOX (vbox4), checkbutton2, FALSE, FALSE, 0);

  checkbutton3 = gtk_check_button_new_with_label (_("Show tooltips"));
  gtk_widget_ref (checkbutton3);
  gtk_object_set_data_full (GTK_OBJECT (propertybox1), "checkbutton3", checkbutton3,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (checkbutton3);
  gtk_box_pack_start (GTK_BOX (vbox4), checkbutton3, FALSE, FALSE, 0);

  frame2 = gtk_frame_new (_("Run"));
  gtk_widget_ref (frame2);
  gtk_object_set_data_full (GTK_OBJECT (propertybox1), "frame2", frame2,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (frame2);
  gtk_box_pack_start (GTK_BOX (vbox3), frame2, TRUE, TRUE, 0);

  vbox5 = gtk_vbox_new (FALSE, 0);
  gtk_widget_ref (vbox5);
  gtk_object_set_data_full (GTK_OBJECT (propertybox1), "vbox5", vbox5,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (vbox5);
  gtk_container_add (GTK_CONTAINER (frame2), vbox5);

  checkbutton4 = gtk_check_button_new_with_label (_("Print program output in text area"));
  gtk_widget_ref (checkbutton4);
  gtk_object_set_data_full (GTK_OBJECT (propertybox1), "checkbutton4", checkbutton4,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (checkbutton4);
  gtk_box_pack_start (GTK_BOX (vbox5), checkbutton4, FALSE, FALSE, 0);

  checkbutton5 = gtk_check_button_new_with_label (_("Run in a separate process"));
  gtk_widget_ref (checkbutton5);
  gtk_object_set_data_full (GTK_OBJECT (propertybox1), "checkbutton5", checkbutton5,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (checkbutton5);
  gtk_box_pack_start (GTK_BOX (vbox5), checkbutton5, FALSE, FALSE, 0);

  label1 = gtk_label_new (_("General"));
  gtk_widget_ref (label1);
  gtk_object_set_data_full (GTK_OBJECT (propertybox1), "label1", label1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label1);
  gtk_notebook_set_tab_label (GTK_NOTEBOOK (notebook1), gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook1), 0), label1);

  vbox1 = gtk_vbox_new (FALSE, 0);
  gtk_widget_ref (vbox1);
  gtk_object_set_data_full (GTK_OBJECT (propertybox1), "vbox1", vbox1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (vbox1);
  gtk_container_add (GTK_CONTAINER (notebook1), vbox1);

  mdiframe = gtk_frame_new (_("Multi Document Interface (MDI)"));
  gtk_widget_ref (mdiframe);
  gtk_object_set_data_full (GTK_OBJECT (propertybox1), "mdiframe", mdiframe,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (mdiframe);
  gtk_box_pack_start (GTK_BOX (vbox1), mdiframe, TRUE, TRUE, 0);

  table2 = gtk_table_new (2, 2, FALSE);
  gtk_widget_ref (table2);
  gtk_object_set_data_full (GTK_OBJECT (propertybox1), "table2", table2,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (table2);
  gtk_container_add (GTK_CONTAINER (mdiframe), table2);

  label7 = gtk_label_new (_("Mode"));
  gtk_widget_ref (label7);
  gtk_object_set_data_full (GTK_OBJECT (propertybox1), "label7", label7,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label7);
  gtk_table_attach (GTK_TABLE (table2), label7, 0, 1, 0, 1,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  gtk_label_set_justify (GTK_LABEL (label7), GTK_JUSTIFY_RIGHT);
  gtk_misc_set_alignment (GTK_MISC (label7), 0, 0.5);

  label8 = gtk_label_new (_("Notebook Tab Position"));
  gtk_widget_ref (label8);
  gtk_object_set_data_full (GTK_OBJECT (propertybox1), "label8", label8,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label8);
  gtk_table_attach (GTK_TABLE (table2), label8, 0, 1, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  gtk_label_set_justify (GTK_LABEL (label8), GTK_JUSTIFY_RIGHT);
  gtk_misc_set_alignment (GTK_MISC (label8), 0, 0.5);

  optionmenu1 = gtk_option_menu_new ();
  gtk_widget_ref (optionmenu1);
  gtk_object_set_data_full (GTK_OBJECT (propertybox1), "optionmenu1", optionmenu1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (optionmenu1);
  gtk_table_attach (GTK_TABLE (table2), optionmenu1, 1, 2, 0, 1,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  optionmenu1_menu = gtk_menu_new ();
  glade_menuitem = gtk_menu_item_new_with_label (_("Notebook"));
  gtk_widget_show (glade_menuitem);
  gtk_menu_append (GTK_MENU (optionmenu1_menu), glade_menuitem);
  glade_menuitem = gtk_menu_item_new_with_label (_("Toplevel"));
  gtk_widget_show (glade_menuitem);
  gtk_menu_append (GTK_MENU (optionmenu1_menu), glade_menuitem);
  glade_menuitem = gtk_menu_item_new_with_label (_("Modal"));
  gtk_widget_show (glade_menuitem);
  gtk_menu_append (GTK_MENU (optionmenu1_menu), glade_menuitem);
  glade_menuitem = gtk_menu_item_new_with_label (_("Default"));
  gtk_widget_show (glade_menuitem);
  gtk_menu_append (GTK_MENU (optionmenu1_menu), glade_menuitem);
  gtk_option_menu_set_menu (GTK_OPTION_MENU (optionmenu1), optionmenu1_menu);

  optionmenu2 = gtk_option_menu_new ();
  gtk_widget_ref (optionmenu2);
  gtk_object_set_data_full (GTK_OBJECT (propertybox1), "optionmenu2", optionmenu2,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (optionmenu2);
  gtk_table_attach (GTK_TABLE (table2), optionmenu2, 1, 2, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  optionmenu2_menu = gtk_menu_new ();
  glade_menuitem = gtk_menu_item_new_with_label (_("Left"));
  gtk_widget_show (glade_menuitem);
  gtk_menu_append (GTK_MENU (optionmenu2_menu), glade_menuitem);
  glade_menuitem = gtk_menu_item_new_with_label (_("Right"));
  gtk_widget_show (glade_menuitem);
  gtk_menu_append (GTK_MENU (optionmenu2_menu), glade_menuitem);
  glade_menuitem = gtk_menu_item_new_with_label (_("Top"));
  gtk_widget_show (glade_menuitem);
  gtk_menu_append (GTK_MENU (optionmenu2_menu), glade_menuitem);
  glade_menuitem = gtk_menu_item_new_with_label (_("Bottom"));
  gtk_widget_show (glade_menuitem);
  gtk_menu_append (GTK_MENU (optionmenu2_menu), glade_menuitem);
  gtk_option_menu_set_menu (GTK_OPTION_MENU (optionmenu2), optionmenu2_menu);
  gtk_option_menu_set_history (GTK_OPTION_MENU (optionmenu2), 2);

  label2 = gtk_label_new (_("Documents"));
  gtk_widget_ref (label2);
  gtk_object_set_data_full (GTK_OBJECT (propertybox1), "label2", label2,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label2);
  gtk_notebook_set_tab_label (GTK_NOTEBOOK (notebook1), gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook1), 1), label2);

  vbox2 = gtk_vbox_new (FALSE, 0);
  gtk_widget_ref (vbox2);
  gtk_object_set_data_full (GTK_OBJECT (propertybox1), "vbox2", vbox2,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (vbox2);
  gtk_container_add (GTK_CONTAINER (notebook1), vbox2);

  colorframe = gtk_frame_new (_("Colors"));
  gtk_widget_ref (colorframe);
  gtk_object_set_data_full (GTK_OBJECT (propertybox1), "colorframe", colorframe,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (colorframe);
  gtk_box_pack_start (GTK_BOX (vbox2), colorframe, TRUE, TRUE, 0);

  table1 = gtk_table_new (3, 2, FALSE);
  gtk_widget_ref (table1);
  gtk_object_set_data_full (GTK_OBJECT (propertybox1), "table1", table1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (table1);
  gtk_container_add (GTK_CONTAINER (colorframe), table1);

  colorpicker1 = gnome_color_picker_new ();
  gtk_widget_ref (colorpicker1);
  gtk_object_set_data_full (GTK_OBJECT (propertybox1), "colorpicker1", colorpicker1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (colorpicker1);
  gtk_table_attach (GTK_TABLE (table1), colorpicker1, 0, 1, 0, 1,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);

  colorpicker2 = gnome_color_picker_new ();
  gtk_widget_ref (colorpicker2);
  gtk_object_set_data_full (GTK_OBJECT (propertybox1), "colorpicker2", colorpicker2,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (colorpicker2);
  gtk_table_attach (GTK_TABLE (table1), colorpicker2, 0, 1, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);

  colorpicker3 = gnome_color_picker_new ();
  gtk_widget_ref (colorpicker3);
  gtk_object_set_data_full (GTK_OBJECT (propertybox1), "colorpicker3", colorpicker3,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (colorpicker3);
  gtk_table_attach (GTK_TABLE (table1), colorpicker3, 0, 1, 2, 3,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);

  label4 = gtk_label_new (_("    Regular nodes"));
  gtk_widget_ref (label4);
  gtk_object_set_data_full (GTK_OBJECT (propertybox1), "label4", label4,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label4);
  gtk_table_attach (GTK_TABLE (table1), label4, 1, 2, 0, 1,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  gtk_misc_set_alignment (GTK_MISC (label4), 0, 0.5);

  label5 = gtk_label_new (_("    Highlited nodes"));
  gtk_widget_ref (label5);
  gtk_object_set_data_full (GTK_OBJECT (propertybox1), "label5", label5,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label5);
  gtk_table_attach (GTK_TABLE (table1), label5, 1, 2, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  gtk_misc_set_alignment (GTK_MISC (label5), 0, 0.5);

  label6 = gtk_label_new (_("    Errors"));
  gtk_widget_ref (label6);
  gtk_object_set_data_full (GTK_OBJECT (propertybox1), "label6", label6,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label6);
  gtk_table_attach (GTK_TABLE (table1), label6, 1, 2, 2, 3,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  gtk_misc_set_alignment (GTK_MISC (label6), 0, 0.5);

  label3 = gtk_label_new (_("Colors"));
  gtk_widget_ref (label3);
  gtk_object_set_data_full (GTK_OBJECT (propertybox1), "label3", label3,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label3);
  gtk_notebook_set_tab_label (GTK_NOTEBOOK (notebook1), gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook1), 2), label3);

  gtk_widget_show(propertybox1);
  return propertybox1;
}

