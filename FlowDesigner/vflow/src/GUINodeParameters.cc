// Copyright (C) 2001 Jean-Marc Valin

#include "GUINodeParameters.h"
#include "GUINode.h"
#include "GUINetwork.h"
#include "Network.h"
#include "object_param.h"

static int user_count = 0;

static void param_apply (GnomePropertyBox *propertybox, gint arg1, GUINodeParameters* user_data)
{
   //cerr << "user_data = " << user_data << endl;
   user_data->apply();
}

static void param_close (GnomePropertyBox *propertybox, GUINodeParameters* user_data)
{
   //cerr << "user_data = " << user_data << endl;
   user_data->hide();
}

static void entry_changed (GtkEntry *entry, GUINodeParameters* user_data)
{
   user_data->changed();
}

static void type_changed (GnomePropertyBox *propertybox, GUINodeParameters* user_data)
{
   user_data->changed();
}

static void comments_changed (GtkText *entry, GUINodeParameters* user_data)
{
   user_data->changed();
}

static void input_adjustment_changed (GtkAdjustment *adjustment, GUINodeParameters* user_data) {

  
  //let's add the required UITerminal & GUITerminal
  
  GUINode *node = user_data->getGUINode();
  
  char input_name[9];
  sprintf(input_name,"USER_%3.3i",user_count++);
  
  
  cerr<<"creating input : "<<input_name<<endl;
  
  node->addTerminal(string(input_name), UINetTerminal::INPUT);
  
  
  cerr<<"input adjustment callback"<<endl;
  ((GUINodeParameters *)(user_data))->changed();
  

}

static void output_adjustment_changed (GtkAdjustment *adjustment, GUINodeParameters* user_data) {


  //let's add the required UITerminal & GUITerminal
  GUINode *node = user_data->getGUINode();
  
  char input_name[9];
  sprintf(input_name,"USER_%3.3i",user_count++);
  
  
  cerr<<"creating input : "<<input_name<<endl;
  node->addTerminal(string(input_name), UINetTerminal::OUTPUT);


  //cout<<"output adjustment callback"<<endl;
  ((GUINodeParameters *)(user_data))->changed();

}




GUINodeParameters::GUINodeParameters(GUINode *_node, string type, UINodeParameters *_nodeParams)
   : node(_node)
   , nodeParams(_nodeParams)
   , textParams(_nodeParams->get_textParams())
{
   params.resize(textParams.size());
   //cerr << "GUINodeParameters::GUINodeParameters" << textParams.size() << "\n";
   
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
   GtkWidget *label13;
   GtkWidget *label_properties;
   GtkWidget *table_properties;
   GtkWidget *input_size_label;
   GtkWidget *output_size_label;
   GtkObject *input_adjustment;
   GtkObject *output_adjustment;
   GtkWidget *input_spinbutton;
   GtkWidget *output_spinbutton;


   nodeproperty = gnome_property_box_new ();
   //gnome_dialog_close_hides (GNOME_DIALOG(nodeproperty), TRUE);
//
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
      params[i].label = gtk_label_new (textParams[i]->name.c_str());
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

      const vector<string> &types=ObjectParam::allTypes();
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
      for (int k=0;k<types.size();k++)
      {
	 //cerr << types[k] << " vs " << textParams[i]->type << endl;
	 if (types[k] == textParams[i]->type)
	    gtk_option_menu_set_history (GTK_OPTION_MENU (params[i].optionmenu), k);
      }



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
     
      gtk_entry_set_text(GTK_ENTRY(gtkentr),(gchar *)textParams[i]->value.c_str());
     
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

   text_comments = gtk_text_new (NULL, NULL);
   gtk_widget_ref (text_comments);
   gtk_object_set_data_full (GTK_OBJECT (nodeproperty), "text_comments", text_comments,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (text_comments);
   gtk_container_add (GTK_CONTAINER (scrolledwindow2), text_comments);
   gtk_text_set_editable(GTK_TEXT(text_comments),TRUE);



   label13 = gtk_label_new (_("Comments"));
   gtk_widget_ref (label13);
   gtk_object_set_data_full (GTK_OBJECT (nodeproperty), "label13", label13,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (label13);
   gtk_notebook_set_tab_label (GTK_NOTEBOOK (notebook2), gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook2), 1), label13);
   {
      int a=0;
      const char *str = nodeParams->getComments().c_str();
      gtk_editable_insert_text (GTK_EDITABLE(text_comments), str, nodeParams->getComments().size(), &a);
   }
   gtk_signal_connect (GTK_OBJECT ( text_comments  ), "changed",
		       GTK_SIGNAL_FUNC(comments_changed), this);
  
  

   //properties tab (DL)

   label_properties = gtk_label_new(_("Properties"));
   gtk_widget_ref (label_properties);
   gtk_object_set_data_full (GTK_OBJECT (nodeproperty), "label_properties", label_properties,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (label_properties);


   table_properties = gtk_table_new (2, 2, TRUE);
   gtk_widget_ref (table_properties);
  

   gtk_object_set_data_full (GTK_OBJECT (nodeproperty), "table_properties", table_properties,
			     (GtkDestroyNotify) gtk_widget_unref);

   gtk_widget_show(table_properties);


   input_size_label = gtk_label_new(_("Input Size"));
   gtk_widget_ref (input_size_label);
   gtk_object_set_data_full (GTK_OBJECT (nodeproperty), "input_size_label", input_size_label,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show(input_size_label);


   //cout<<"input size : "<< _node->getInputs().size()<<endl;

   input_adjustment = gtk_adjustment_new ((float) _node->getInputs().size(), //value
					  (float) _node->getInputs().size(), //lower
					  100.0, //higher
					  1.0,
					  1.0,
					  1.0);

 

   input_spinbutton =  gtk_spin_button_new (GTK_ADJUSTMENT(input_adjustment),
					    1.0,0);
   gtk_widget_ref (input_spinbutton);
   gtk_object_set_data_full (GTK_OBJECT (nodeproperty), "input_spinbutton",input_spinbutton,
			     (GtkDestroyNotify) gtk_widget_unref);

   gtk_widget_show(input_spinbutton);

   //Here we should check if the node is resizable and set to TRUE if needed
   gtk_entry_set_editable (GTK_ENTRY(input_spinbutton),FALSE);

   //we should also activate/deactivate the button if the node is resizable

   output_size_label = gtk_label_new(_("Output Size"));
   gtk_widget_ref (output_size_label);
   gtk_object_set_data_full (GTK_OBJECT (nodeproperty), "output_size_label", output_size_label,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show(output_size_label);

   //cout<<"output size : "<< _node->getOutputs().size()<<endl;


   output_adjustment = gtk_adjustment_new ((float) _node->getOutputs().size(), //value
					   (float) _node->getOutputs().size(), //lower
					   100.0, //higher
					   1.0,
					   1.0,
					   1.0);
  


   output_spinbutton =  gtk_spin_button_new (GTK_ADJUSTMENT(output_adjustment),
					     1.0,0);
   gtk_widget_ref (output_spinbutton);
   gtk_object_set_data_full (GTK_OBJECT (nodeproperty), "output_spinbutton",output_spinbutton,
			     (GtkDestroyNotify) gtk_widget_unref);

   gtk_widget_show(output_spinbutton);

   //Here we should check if the node is resizable and set to TRUE if needed
   gtk_entry_set_editable (GTK_ENTRY(output_spinbutton),FALSE);

   //we should also activate/deactivate the button if the node is resizable

                                              


   gtk_table_attach (GTK_TABLE (table_properties), input_size_label, 0, 1, 0, 1,
		     (GtkAttachOptions) (GTK_FILL),
		     (GtkAttachOptions) (GTK_FILL), 0, 0);

   gtk_table_attach (GTK_TABLE (table_properties), input_spinbutton, 1, 2, 0, 1,
		     (GtkAttachOptions) (GTK_FILL),
		     (GtkAttachOptions) (GTK_FILL), 0, 0);


   gtk_table_attach (GTK_TABLE (table_properties), output_size_label, 0, 1, 1, 2,
		     (GtkAttachOptions) (GTK_FILL),
		     (GtkAttachOptions) (GTK_FILL), 0, 0);

   gtk_table_attach (GTK_TABLE (table_properties), output_spinbutton, 1, 2, 1, 2,
		     (GtkAttachOptions) (GTK_FILL),
		     (GtkAttachOptions) (GTK_FILL), 0, 0);


   gtk_table_set_homogeneous (GTK_TABLE(table_properties),TRUE);


   gtk_notebook_append_page (GTK_NOTEBOOK(notebook2),
			     table_properties,
			     label_properties);


   gtk_signal_connect (GTK_OBJECT (input_adjustment ), "value-changed",
		       GTK_SIGNAL_FUNC(input_adjustment_changed),this);
  
   //gtk_signal_connect (GTK_OBJECT (input_adjustment ), "changed",
   //		      GTK_SIGNAL_FUNC(input_adjustment_changed), this);
  
   gtk_signal_connect (GTK_OBJECT (output_adjustment ), "value-changed",
		       GTK_SIGNAL_FUNC(output_adjustment_changed),this);
  
   //gtk_signal_connect (GTK_OBJECT (output_adjustment ), "changed",
   //		      GTK_SIGNAL_FUNC(output_adjustment_changed), this);


   //end properties tab (DL)


   //gnome_property_box_changed(GNOME_PROPERTY_BOX(nodeproperty));
   gtk_signal_connect (GTK_OBJECT (nodeproperty), "apply",
		       GTK_SIGNAL_FUNC(param_apply), this);

   gtk_signal_connect (GTK_OBJECT (nodeproperty), "close",
		       GTK_SIGNAL_FUNC(param_close), this);

   gnome_property_box_set_state(GNOME_PROPERTY_BOX (nodeproperty), false);
   gtk_widget_show(nodeproperty);
   //cerr << "this is " << this << "\n";
}

GUINodeParameters::~GUINodeParameters()
{
   gtk_widget_destroy(nodeproperty);
   //cerr << "destroyed\n";
}

void GUINodeParameters::show()
{
   gtk_widget_show (nodeproperty);
}

void GUINodeParameters::hide()
{
   //gtk_widget_hide (nodeproperty);
   delete this;
   //cerr << this << endl;
}


void GUINodeParameters::apply()
{
   //cerr << this << endl;
   for (int i=0;i<params.size();i++)
   {
      //GtkWidget *gtk_option_menu_get_menu(params[i].optionmenu);
      GtkWidget *menu = gtk_menu_get_active (GTK_MENU(params[i].optionmenu_menu));
      textParams[i]->type = (char *)gtk_object_get_user_data (GTK_OBJECT(menu));

      GtkWidget *gtkentr = gnome_entry_gtk_entry(GNOME_ENTRY(params[i].entry));
      textParams[i]->value = gtk_entry_get_text(GTK_ENTRY(gtkentr));
      
      //cerr << "<param: " << params[i].name << ", " << params[i].type << ":" << params[i].value << ">\n";
   }
   //cerr << "apply\n";
   nodeParams->setComments(string(gtk_editable_get_chars(GTK_EDITABLE(text_comments), 0, -1)));
   node->getNetwork()->setModified();
}

void GUINodeParameters::changed()
{
   gnome_property_box_changed(GNOME_PROPERTY_BOX(nodeproperty));
}

ParameterData *GUINodeParameters::getParamDataNamed(string n)
{
   for (int i=0;i<textParams.size();i++)
      if (textParams[i]->name == n)
         return &(params[i]);
   return NULL;
}

void GUINodeParameters::insertLoadedParam(ParameterText *param, string type, string value)
{
   ParameterData *data = getParamDataNamed(param->name);
   const vector<string> &types=ObjectParam::allTypes();
   for (int i=0;i<types.size();i++)
      if (types[i] == type)
	 gtk_option_menu_set_history (GTK_OPTION_MENU (data->optionmenu), i);
   GtkWidget *gtkentr = gnome_entry_gtk_entry(GNOME_ENTRY(data->entry));
   gtk_entry_set_text(GTK_ENTRY(gtkentr),(gchar *)value.c_str());
   
}
