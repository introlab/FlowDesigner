#include "GUINodeParameters.h"
#include "GUINode.h"
#include "GUINetwork.h"

static void param_apply (GnomePropertyBox *propertybox, gint arg1, gpointer user_data)
{
   ((GUINodeParameters *)(user_data))->apply();
}

static void entry_changed (GtkEntry *entry, gpointer user_data)
{
   ((GUINodeParameters *)(user_data))->changed();
}

static void type_changed (GnomePropertyBox *propertybox, gpointer user_data)
{
   ((GUINodeParameters *)(user_data))->changed();
}

GUINodeParameters::GUINodeParameters(UINode *_node, string type)
   : UINodeParameters (_node, type)
{
   //cerr << "GUINodeParameters::GUINodeParameters\n";
   int i;

   //GtkWidget *nodeproperty;
  GtkWidget *notebook2;
  GtkWidget *table2;
  GtkWidget *label14;
  GtkWidget *label15;
  GtkWidget *label16;
  GtkWidget *hseparator4;
  GtkWidget *hseparator5;
  GtkWidget *hseparator6;
  GtkWidget *glade_menuitem;
  

  GtkWidget *label12;
  GtkWidget *scrolledwindow2;
  GtkWidget *text1;
  GtkWidget *label13;

  nodeproperty = gnome_property_box_new ();
  gnome_dialog_close_hides (GNOME_DIALOG(nodeproperty), TRUE);

  gtk_object_set_data (GTK_OBJECT (nodeproperty), "nodeproperty", nodeproperty);

  notebook2 = GNOME_PROPERTY_BOX (nodeproperty)->notebook;
  gtk_object_set_data (GTK_OBJECT (nodeproperty), "notebook2", notebook2);
  gtk_widget_show (notebook2);

  table2 = gtk_table_new (2+params.size(), 3, FALSE);
  gtk_widget_ref (table2);
  gtk_object_set_data_full (GTK_OBJECT (nodeproperty), "table2", table2,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (table2);
  gtk_container_add (GTK_CONTAINER (notebook2), table2);

  label14 = gtk_label_new (_("Name"));
  gtk_widget_ref (label14);
  gtk_object_set_data_full (GTK_OBJECT (nodeproperty), "label14", label14,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label14);
  gtk_table_attach (GTK_TABLE (table2), label14, 0, 1, 0, 1,
                    (GtkAttachOptions) (0),
                    (GtkAttachOptions) (0), 0, 0);

  label15 = gtk_label_new (_("Type"));
  gtk_widget_ref (label15);
  gtk_object_set_data_full (GTK_OBJECT (nodeproperty), "label15", label15,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label15);
  gtk_table_attach (GTK_TABLE (table2), label15, 1, 2, 0, 1,
                    (GtkAttachOptions) (0),
                    (GtkAttachOptions) (0), 0, 0);

  label16 = gtk_label_new (_("Value"));
  gtk_widget_ref (label16);
  gtk_object_set_data_full (GTK_OBJECT (nodeproperty), "label16", label16,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label16);
  gtk_table_attach (GTK_TABLE (table2), label16, 2, 3, 0, 1,
                    (GtkAttachOptions) (0),
                    (GtkAttachOptions) (0), 0, 0);

  hseparator4 = gtk_hseparator_new ();
  gtk_widget_ref (hseparator4);
  gtk_object_set_data_full (GTK_OBJECT (nodeproperty), "hseparator4", hseparator4,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (hseparator4);
  gtk_table_attach (GTK_TABLE (table2), hseparator4, 0, 1, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);

  hseparator5 = gtk_hseparator_new ();
  gtk_widget_ref (hseparator5);
  gtk_object_set_data_full (GTK_OBJECT (nodeproperty), "hseparator5", hseparator5,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (hseparator5);
  gtk_table_attach (GTK_TABLE (table2), hseparator5, 1, 2, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (GTK_FILL), 0, 0);

  hseparator6 = gtk_hseparator_new ();
  gtk_widget_ref (hseparator6);
  gtk_object_set_data_full (GTK_OBJECT (nodeproperty), "hseparator6", hseparator6,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (hseparator6);
  gtk_table_attach (GTK_TABLE (table2), hseparator6, 2, 3, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (GTK_FILL), 0, 0);



  
  for (i=0;i<params.size();i++)
  {
     params[i].label = gtk_label_new (params[i].name.c_str());
     gtk_widget_ref (params[i].label);
     gtk_object_set_data_full (GTK_OBJECT (nodeproperty), "label", params[i].label,
                               (GtkDestroyNotify) gtk_widget_unref);
     gtk_widget_show (params[i].label);
     gtk_table_attach (GTK_TABLE (table2), params[i].label, 0, 1, 2+i, 3+i,
                       (GtkAttachOptions) (0),
                       (GtkAttachOptions) (0), 0, 0);



     params[i].optionmenu = gtk_option_menu_new ();
     gtk_widget_ref (params[i].optionmenu);
     gtk_object_set_data_full (GTK_OBJECT (nodeproperty), "optionmenu", params[i].optionmenu,
                               (GtkDestroyNotify) gtk_widget_unref);
     gtk_widget_show (params[i].optionmenu);
     gtk_table_attach (GTK_TABLE (table2), params[i].optionmenu, 1, 2, 2+i, 3+i,
                       (GtkAttachOptions) (0),
                       (GtkAttachOptions) (0), 0, 0);
     params[i].optionmenu_menu = gtk_menu_new ();

     const vector<string> &types=allTypes();
     for (int j=0;j<types.size();j++)
     {
        glade_menuitem = gtk_menu_item_new_with_label ((const gchar *)types[j].c_str());
        gtk_object_set_user_data(GTK_OBJECT(glade_menuitem), (void *)types[j].c_str());
        gtk_widget_show (glade_menuitem);
        gtk_menu_append (GTK_MENU (params[i].optionmenu_menu), glade_menuitem);
     }

     gtk_option_menu_set_menu (GTK_OPTION_MENU (params[i].optionmenu), params[i].optionmenu_menu);
     gtk_option_menu_set_history (GTK_OPTION_MENU (params[i].optionmenu), 0);
          

     gtk_signal_connect (GTK_OBJECT ( params[i].optionmenu_menu ), "selection-done",
                        GTK_SIGNAL_FUNC( type_changed), this);




     params[i].entry = gnome_entry_new (NULL);
     gtk_widget_ref (params[i].entry);
     gtk_object_set_data_full (GTK_OBJECT (nodeproperty), "entry", params[i].entry,
                               (GtkDestroyNotify) gtk_widget_unref);
     gtk_widget_show (params[i].entry);
     gtk_table_attach (GTK_TABLE (table2), params[i].entry, 2, 3, 2+i, 3+i,
                       (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                       (GtkAttachOptions) (0), 0, 0);
     
     params[i].combo_entry = gnome_entry_gtk_entry (GNOME_ENTRY (params[i].entry));
     gtk_widget_ref (params[i].combo_entry);
     gtk_object_set_data_full (GTK_OBJECT (nodeproperty), "combo_entry", params[i].combo_entry,
                               (GtkDestroyNotify) gtk_widget_unref);
     gtk_widget_show (params[i].combo_entry);
     
     GtkWidget *gtkentr = gnome_entry_gtk_entry(GNOME_ENTRY(params[i].entry));
     gtk_signal_connect (GTK_OBJECT ( gtkentr  ), "changed",
                         GTK_SIGNAL_FUNC(entry_changed), this);
  }
  






  label12 = gtk_label_new (_("Parameters"));
  gtk_widget_ref (label12);
  gtk_object_set_data_full (GTK_OBJECT (nodeproperty), "label12", label12,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label12);
  gtk_notebook_set_tab_label (GTK_NOTEBOOK (notebook2), gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook2), 0), label12);

  scrolledwindow2 = gtk_scrolled_window_new (NULL, NULL);
  gtk_widget_ref (scrolledwindow2);
  gtk_object_set_data_full (GTK_OBJECT (nodeproperty), "scrolledwindow2", scrolledwindow2,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (scrolledwindow2);
  gtk_container_add (GTK_CONTAINER (notebook2), scrolledwindow2);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolledwindow2), GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);

  text1 = gtk_text_new (NULL, NULL);
  gtk_widget_ref (text1);
  gtk_object_set_data_full (GTK_OBJECT (nodeproperty), "text1", text1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (text1);
  gtk_container_add (GTK_CONTAINER (scrolledwindow2), text1);
  gtk_text_set_editable(GTK_TEXT(text1),TRUE);

  label13 = gtk_label_new (_("Comments"));
  gtk_widget_ref (label13);
  gtk_object_set_data_full (GTK_OBJECT (nodeproperty), "label13", label13,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label13);
  gtk_notebook_set_tab_label (GTK_NOTEBOOK (notebook2), gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook2), 1), label13);

  //gnome_property_box_changed(GNOME_PROPERTY_BOX(nodeproperty));
  gtk_signal_connect (GTK_OBJECT (nodeproperty), "apply",
                      GTK_SIGNAL_FUNC(param_apply), this);

  //cerr << "this is " << this << "\n";
}

GUINodeParameters::~GUINodeParameters()
{
   gtk_widget_destroy(nodeproperty);
}

void GUINodeParameters::show()
{
   gtk_widget_show (nodeproperty);
}

void GUINodeParameters::hide()
{
   gtk_widget_hide (nodeproperty);
}


void GUINodeParameters::apply()
{
   for (int i=0;i<params.size();i++)
   {
      //GtkWidget *gtk_option_menu_get_menu(params[i].optionmenu);
      GtkWidget *menu = gtk_menu_get_active (GTK_MENU(params[i].optionmenu_menu));
      params[i].type = (char *)gtk_object_get_user_data (GTK_OBJECT(menu));

      GtkWidget *gtkentr = gnome_entry_gtk_entry(GNOME_ENTRY(params[i].entry));
      params[i].value = gtk_entry_get_text(GTK_ENTRY(gtkentr));
      
      //cerr << "<param: " << params[i].name << ", " << params[i].type << ":" << params[i].value << ">\n";
   }
   //cerr << "apply\n";
   node->getNetwork()->setModified();
}

void GUINodeParameters::changed()
{
   gnome_property_box_changed(GNOME_PROPERTY_BOX(nodeproperty));
}


void GUINodeParameters::insertLoadedParam(ParameterData *param, string type, string value)
{
   const vector<string> &types=allTypes();
   for (int i=0;i<types.size();i++)
      if (types[i] == type)
	 gtk_option_menu_set_history (GTK_OPTION_MENU (param->optionmenu), i);
   GtkWidget *gtkentr = gnome_entry_gtk_entry(GNOME_ENTRY(param->entry));
   gtk_entry_set_text(GTK_ENTRY(gtkentr),(gchar *)value.c_str());
   
}