// Copyright (C) 2001 Jean-Marc Valin

#include <string>
#include <iostream>
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

static void comments_changed (GtkTextView *entry, GUINodeParameters* user_data)
{
   user_data->changed();
}

static void node_remove_input_select (GtkWidget *item, GUINodeParameters *node_param)
{
   node_param->setInputSelect(item);
}

static void node_remove_input_unselect (GtkWidget *item, GUINodeParameters *node_param)
{
   node_param->unsetInputSelect(item);
}

static void node_remove_output_select (GtkWidget *item, GUINodeParameters *node_param)
{
   node_param->setOutputSelect(item);
}

static void node_remove_output_unselect (GtkWidget *item, GUINodeParameters *node_param)
{
   node_param->unsetOutputSelect(item);
}

void GUINodeParameters::addInput()
{
   char *new_input = (char*) gtk_entry_get_text(GTK_ENTRY(input_entry));
   if (!new_input || strlen(new_input)==0)
      return;

   gtk_list_select_all(GTK_LIST(list1));

   GtkWidget *inp1 = gtk_list_item_new_with_label(new_input);
   gtk_widget_ref (inp1);
   gtk_object_set_data (GTK_OBJECT (inp1), "label", (gpointer)new_input);
   gtk_signal_connect (GTK_OBJECT (inp1), "select",
                       GTK_SIGNAL_FUNC (node_remove_input_select),
                       this);
   gtk_signal_connect (GTK_OBJECT (inp1), "deselect",
                       GTK_SIGNAL_FUNC (node_remove_input_unselect),
                       this);
   gtk_object_set_data_full (GTK_OBJECT (list1), "table1", inp1,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show(inp1);
   GList *in_lis = g_list_append (NULL, inp1);

   gtk_list_append_items(GTK_LIST(list1), in_lis);
   node->addTerminal(string(new_input), UINetTerminal::INPUT);
}

void GUINodeParameters::addOutput()
{

   char *new_output = (char*)gtk_entry_get_text(GTK_ENTRY(output_entry));
 
   if (!new_output || strlen(new_output)==0)
      return;

   gtk_list_select_all(GTK_LIST(list1));

   GtkWidget *outp1 = gtk_list_item_new_with_label(new_output);
   gtk_widget_ref (outp1);

   gtk_object_set_data (GTK_OBJECT (outp1), "label", (gpointer)new_output);
   gtk_signal_connect (GTK_OBJECT (outp1), "select",
                       GTK_SIGNAL_FUNC (node_remove_output_select),
                       this);
   gtk_signal_connect (GTK_OBJECT (outp1), "deselect",
                       GTK_SIGNAL_FUNC (node_remove_output_unselect),
                       this);
   gtk_object_set_data_full (GTK_OBJECT (list1), "table1", outp1,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show(outp1);
   GList *out_lis = g_list_append (NULL, outp1);

   gtk_list_append_items(GTK_LIST(list2), out_lis);
   node->addTerminal(string(new_output), UINetTerminal::OUTPUT);
}

void GUINodeParameters::removeInput()
{
   if (!inputSelect)
   {
      //cerr << "no input selected\n";
      return;
   }
   char *input_name = (char *)gtk_object_get_data(GTK_OBJECT(inputSelect),"label");
   GList *lis = g_list_append (NULL, inputSelect);
   gtk_list_remove_items(GTK_LIST(list1), lis);
   node->removeTerminal(string(input_name), UINetTerminal::INPUT);
}

void GUINodeParameters::removeOutput()
{
   if (!outputSelect)
   {
      //cerr << "no output selected\n";
      return;
   }
   char *output_name = (char *)gtk_object_get_data(GTK_OBJECT(outputSelect),"label");
   GList *lis = g_list_append (NULL, outputSelect);
   gtk_list_remove_items(GTK_LIST(list2), lis);
   node->removeTerminal(string(output_name), UINetTerminal::OUTPUT);
}

static void node_add_input (GtkButton *button, GUINodeParameters *node_param)
{
   node_param->addInput();
}

static void node_add_output (GtkButton *button, GUINodeParameters *node_param)
{
   node_param->addOutput();
}

static void node_remove_input (GtkButton *button, GUINodeParameters *node_param)
{
   node_param->removeInput();
}

static void node_remove_output (GtkButton *button, GUINodeParameters *node_param)
{
   node_param->removeOutput();
}



GUINodeParameters::GUINodeParameters(UINode *_node, string type)
  : UINodeParameters(_node,type), inputSelect(NULL), outputSelect(NULL), nodeproperty(NULL)
{

}

GUINodeParameters::~GUINodeParameters()
{
  if (nodeproperty) {
    gtk_widget_destroy(nodeproperty);
  }
}

void GUINodeParameters::show()
{
  //create all widgets
  createWindow();
}

void GUINodeParameters::hide()
{
  //destroy widgets
  gtk_widget_destroy(nodeproperty);

  nodeproperty = NULL;

}


void GUINodeParameters::apply()
{
   //Set this to true if there's any change involving a subnet_param change
   bool changedNetInterface=false;
   for (int i=0;i<params.size();i++)
   {
      //GtkWidget *gtk_option_menu_get_menu(params[i].optionmenu);
      GtkWidget *menu = gtk_menu_get_active (GTK_MENU(params[i].optionmenu_menu));
      GtkWidget *gtkentr = gnome_entry_gtk_entry(GNOME_ENTRY(params[i].entry));
      string newType = (char *)gtk_object_get_user_data (GTK_OBJECT(menu));
      string newValue = gtk_entry_get_text(GTK_ENTRY(gtkentr));

      //There's a subnet_param involved
      if (newType == "subnet_param" || textParams[i]->type == "subnet_param")
      {
	 //There's a change somewhere
	if (newType != textParams[i]->type || newValue != textParams[i]->value) {
	  //subnet param 
	  changedNetInterface=true;
	  //cerr<<"changedNetInterface = true"<<endl;
	  //cerr<<"type : "<<newType<<endl;
	  //cerr<<"value: "<<newValue<<endl;
	}
      }

      //update type, value
      textParams[i]->type = newType;
      textParams[i]->value = newValue;
   }
   //nodeParams->setComments(string(gtk_editable_get_chars(GTK_EDITABLE(text_comments), 0, -1)));
   GtkTextBuffer *buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (text_comments));
   GtkTextIter start;
   GtkTextIter end;

   gtk_text_buffer_get_bounds(buffer, &start, &end); 
   
   setComments(string(gtk_text_buffer_get_text(buffer, &start, &end, true)));
   
   node->getNetwork()->setModified();

   node->getNetwork()->interfaceChangeNotify();   


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

/*
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
*/

void GUINodeParameters::createWindow() {
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

   GtkWidget *table1;
   GtkWidget *vseparator1;
   GtkWidget *vseparator2;
   GtkWidget *vseparator3;
   GtkWidget *hbox1;
   GtkWidget *button1;
   GtkWidget *button2;
   GtkWidget *hbox2;
   GtkWidget *button3;
   GtkWidget *button4;
   GtkWidget *label1;
   GtkWidget *label2;

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

   text_comments = gtk_text_view_new ();
   gtk_widget_ref (text_comments);
   gtk_object_set_data_full (GTK_OBJECT (nodeproperty), "text_comments", text_comments,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (text_comments);
   gtk_container_add (GTK_CONTAINER (scrolledwindow2), text_comments);
   gtk_text_view_set_editable(GTK_TEXT_VIEW(text_comments),TRUE);



   label13 = gtk_label_new (_("Comments"));
   gtk_widget_ref (label13);
   gtk_object_set_data_full (GTK_OBJECT (nodeproperty), "label13", label13,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (label13);
   gtk_notebook_set_tab_label (GTK_NOTEBOOK (notebook2), gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook2), 1), label13);
   {
      int a=0;
      const char *str = getComments().c_str();
     gtk_text_buffer_set_text (gtk_text_view_get_buffer (GTK_TEXT_VIEW (text_comments)),
                              str, -1);

     //gtk_editable_insert_text (GTK_EDITABLE(text_comments), str, nodeParams->getComments().size(), &a);
   }
   g_signal_connect (G_OBJECT ( gtk_text_view_get_buffer (GTK_TEXT_VIEW (text_comments))  ), "changed",
		       GTK_SIGNAL_FUNC(comments_changed), this);
  
  

   //properties tab (DL)
   //removed until it works

   
      
   GList *in_lis = NULL;
   GList *out_lis = NULL;
         
   vector<UITerminal *> inputs = node->getInputs();
   vector<UITerminal *> outputs = node->getOutputs();
   for (int i=0;i<inputs.size();i++)
   {
      const char *input_name = inputs[i]->getName().c_str();
      GtkWidget *inp1 = gtk_list_item_new_with_label(input_name);
      gtk_widget_ref (inp1);
      gtk_object_set_data (GTK_OBJECT (inp1), "label", (gpointer)input_name);
      gtk_signal_connect (GTK_OBJECT (inp1), "select",
                          GTK_SIGNAL_FUNC (node_remove_input_select),
                          this);
      gtk_signal_connect (GTK_OBJECT (inp1), "deselect",
                          GTK_SIGNAL_FUNC (node_remove_input_unselect),
                          this);
      gtk_object_set_data_full (GTK_OBJECT (notebook2), "table1", inp1,
                                (GtkDestroyNotify) gtk_widget_unref);
      gtk_widget_show(inp1);
      in_lis = g_list_append (in_lis, inp1);
   }

   for (int i=0;i<outputs.size();i++)
   {
      const char *output_name = outputs[i]->getName().c_str();
      GtkWidget *outp1 = gtk_list_item_new_with_label(output_name);
      gtk_widget_ref (outp1);
      gtk_object_set_data (GTK_OBJECT (outp1), "label", (gpointer)output_name);
      gtk_signal_connect (GTK_OBJECT (outp1), "select",
                          GTK_SIGNAL_FUNC (node_remove_output_select),
                          this);
      gtk_signal_connect (GTK_OBJECT (outp1), "deselect",
                          GTK_SIGNAL_FUNC (node_remove_output_unselect),
                          this);
      gtk_object_set_data_full (GTK_OBJECT (notebook2), "table1", outp1,
                                (GtkDestroyNotify) gtk_widget_unref);
      gtk_widget_show(outp1);
      out_lis = g_list_append (out_lis, outp1);
   }

   label_properties = gtk_label_new(_("Inputs/Outputs"));
   gtk_widget_ref (label_properties);
   gtk_object_set_data_full (GTK_OBJECT (nodeproperty), "label_properties", label_properties,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (label_properties);
      
      

   //GtkWidget *notebook2;

   //window1 = gtk_window_new (GTK_WINDOW_TOPLEVEL);
   //gtk_object_set_data (GTK_OBJECT (window1), "window1", window1);
   //gtk_window_set_title (GTK_WINDOW (window1), _("window1"));

   table1 = gtk_table_new (3, 3, FALSE);
   gtk_widget_ref (table1);
   gtk_object_set_data_full (GTK_OBJECT (notebook2), "table1", table1,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (table1);
   //gtk_container_add (GTK_CONTAINER (notebook2), table1);
   gtk_notebook_append_page (GTK_NOTEBOOK(notebook2),
                             table1,
                             label_properties);

   vseparator1 = gtk_vseparator_new ();
   gtk_widget_ref (vseparator1);
   gtk_object_set_data_full (GTK_OBJECT (notebook2), "vseparator1", vseparator1,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (vseparator1);
   gtk_table_attach (GTK_TABLE (table1), vseparator1, 1, 2, 0, 1,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);

   vseparator2 = gtk_vseparator_new ();
   gtk_widget_ref (vseparator2);
   gtk_object_set_data_full (GTK_OBJECT (notebook2), "vseparator2", vseparator2,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (vseparator2);
   gtk_table_attach (GTK_TABLE (table1), vseparator2, 1, 2, 1, 2,
                     (GtkAttachOptions) (GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);

   vseparator3 = gtk_vseparator_new ();
   gtk_widget_ref (vseparator3);
   gtk_object_set_data_full (GTK_OBJECT (notebook2), "vseparator3", vseparator3,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (vseparator3);
   gtk_table_attach (GTK_TABLE (table1), vseparator3, 1, 2, 2, 3,
                     (GtkAttachOptions) (GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);

   hbox1 = gtk_hbox_new (FALSE, 0);
   gtk_widget_ref (hbox1);
   gtk_object_set_data_full (GTK_OBJECT (notebook2), "hbox1", hbox1,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (hbox1);
   gtk_table_attach (GTK_TABLE (table1), hbox1, 0, 1, 1, 2,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                     (GtkAttachOptions) (GTK_FILL), 0, 0);

   input_entry = gtk_entry_new ();
   gtk_widget_ref (input_entry);
   gtk_object_set_data_full (GTK_OBJECT (notebook2), "input_entry", input_entry,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (input_entry);
   gtk_box_pack_start (GTK_BOX (hbox1), input_entry, TRUE, TRUE, 0);

   button1 = gtk_button_new_with_label (_("Add"));
   gtk_widget_ref (button1);
   gtk_object_set_data_full (GTK_OBJECT (notebook2), "button1", button1,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button1);
   gtk_box_pack_start (GTK_BOX (hbox1), button1, FALSE, FALSE, 0);

   button2 = gtk_button_new_with_label (_("Remove"));
   gtk_widget_ref (button2);
   gtk_object_set_data_full (GTK_OBJECT (notebook2), "button2", button2,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button2);
   gtk_box_pack_start (GTK_BOX (hbox1), button2, FALSE, FALSE, 0);

   hbox2 = gtk_hbox_new (FALSE, 0);
   gtk_widget_ref (hbox2);
   gtk_object_set_data_full (GTK_OBJECT (notebook2), "hbox2", hbox2,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (hbox2);
   gtk_table_attach (GTK_TABLE (table1), hbox2, 2, 3, 1, 2,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                     (GtkAttachOptions) (GTK_FILL), 0, 0);

   output_entry = gtk_entry_new ();
   gtk_widget_ref (output_entry);
   gtk_object_set_data_full (GTK_OBJECT (notebook2), "output_entry", output_entry,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (output_entry);
   gtk_box_pack_start (GTK_BOX (hbox2), output_entry, FALSE, FALSE, 0);

   button3 = gtk_button_new_with_label (_("Add"));
   gtk_widget_ref (button3);
   gtk_object_set_data_full (GTK_OBJECT (notebook2), "button3", button3,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button3);
   gtk_box_pack_start (GTK_BOX (hbox2), button3, FALSE, FALSE, 0);

   button4 = gtk_button_new_with_label (_("Remove"));
   gtk_widget_ref (button4);
   gtk_object_set_data_full (GTK_OBJECT (notebook2), "button4", button4,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button4);
   gtk_box_pack_start (GTK_BOX (hbox2), button4, FALSE, FALSE, 0);

   list2 = gtk_list_new ();
   gtk_widget_ref (list2);
   gtk_object_set_data_full (GTK_OBJECT (notebook2), "list2", list2,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (list2);
   gtk_table_attach (GTK_TABLE (table1), list2, 2, 3, 2, 3,
                     (GtkAttachOptions) (GTK_FILL),
                     (GtkAttachOptions) (GTK_FILL), 0, 0);

   gtk_list_append_items(GTK_LIST(list2), out_lis);

   label1 = gtk_label_new (_("Inputs"));
   gtk_widget_ref (label1);
   gtk_object_set_data_full (GTK_OBJECT (notebook2), "label1", label1,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (label1);
   gtk_table_attach (GTK_TABLE (table1), label1, 0, 1, 0, 1,
                     (GtkAttachOptions) (GTK_FILL),
                     (GtkAttachOptions) (0), 0, 0);
   gtk_misc_set_alignment (GTK_MISC (label1), 0, 0.5);

   label2 = gtk_label_new (_("Outputs"));
   gtk_widget_ref (label2);
   gtk_object_set_data_full (GTK_OBJECT (notebook2), "label2", label2,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (label2);
   gtk_table_attach (GTK_TABLE (table1), label2, 2, 3, 0, 1,
                     (GtkAttachOptions) (GTK_FILL),
                     (GtkAttachOptions) (0), 0, 0);
   gtk_misc_set_alignment (GTK_MISC (label2), 0, 0.5);

   list1 = gtk_list_new ();
   gtk_widget_ref (list1);
   gtk_object_set_data_full (GTK_OBJECT (notebook2), "list1", list1,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (list1);
   gtk_table_attach (GTK_TABLE (table1), list1, 0, 1, 2, 3,
                     (GtkAttachOptions) (GTK_FILL),
                     (GtkAttachOptions) (GTK_FILL), 0, 0);

   gtk_list_append_items(GTK_LIST(list1), in_lis);

   gtk_signal_connect (GTK_OBJECT (button1), "clicked",
                       GTK_SIGNAL_FUNC (node_add_input),
                       this);
   gtk_signal_connect (GTK_OBJECT (button2), "clicked",
                       GTK_SIGNAL_FUNC (node_remove_input),
                       this);
   gtk_signal_connect (GTK_OBJECT (button3), "clicked",
                       GTK_SIGNAL_FUNC (node_add_output),
                       this);
   gtk_signal_connect (GTK_OBJECT (button4), "clicked",
                       GTK_SIGNAL_FUNC (node_remove_output),
                       this);
    
   

   //gnome_property_box_changed(GNOME_PROPERTY_BOX(nodeproperty));
   gtk_signal_connect (GTK_OBJECT (nodeproperty), "apply",
		       GTK_SIGNAL_FUNC(param_apply), this);

   gtk_signal_connect (GTK_OBJECT (nodeproperty), "close",
		       GTK_SIGNAL_FUNC(param_close), this);

   gnome_property_box_set_state(GNOME_PROPERTY_BOX (nodeproperty), false);
   gtk_widget_show(nodeproperty);
   //cerr << "this is " << this << "\n";

}
