// Copyright (C) 2001 Jean-Marc Valin

#include <typeinfo>
#include <pthread.h>
#include <sstream>

#include "vflow.h"
#include "GUIDocument.h"
#include "GUINetwork.h"
#include "ParameterSet.h"
#include "Network.h"
#include "rc_ptrs.h"
#include "object_param.h"
#include "UserException.h"

#include "misc_gui.h"

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif


bool GUIDocument::isRunning=false;
pthread_t GUIDocument::runThread;
Network * GUIDocument::runningNet=NULL;
pthread_mutex_t GUIDocument::del_lock = PTHREAD_MUTEX_INITIALIZER;


#define GLADE_HOOKUP_OBJECT(component,widget,name) \
  g_object_set_data_full (G_OBJECT (component), name, \
    gtk_widget_ref (widget), (GDestroyNotify) gtk_widget_unref)

#define GLADE_HOOKUP_OBJECT_NO_REF(component,widget,name) \
  g_object_set_data (G_OBJECT (component), name, widget)



static GtkWidget *create_view (GnomeMDIChild *child, GUIDocument *doc)
{
   //GUIDocument *doc = gtk_object_get_data (GTK_OBJECT (child), "doc");
   return doc->getView();
}


GUIDocument::GUIDocument(string _name)
   : UIDocument(_name)
   , docproperty(NULL)
   , vbox2(NULL)
   , document_notebook(NULL)
   , less2(NULL)
//   , untitled(true)
//   , modified(false)
{

  //GtkWidget *vflow_notebook = vflowGUI::instance()->get_notebook();
  //cerr<<"GUIDocument getting notebook ptr (vflow app): "<<vflow_notebook<<endl;

  vbox2 = gtk_vpaned_new ();
  //gtk_widget_set_usize(vbox2, -1, 400);
  gtk_widget_ref (vbox2);

  //gtk_object_set_data_full (GTK_OBJECT (mdi), "vbox2", vbox2,
  //                          (GtkDestroyNotify) gtk_widget_unref);

  //cerr<<"GUIDocument creating document : "<<_name<<endl;
  //cerr<<"GUIDocument setting document data ""doc"" to vbox2 : "<<this<<endl;
  gtk_object_set_data(GTK_OBJECT(vbox2), "doc", this);
  gtk_widget_show (vbox2);
  
  
  //gtk_container_add (GTK_CONTAINER (vbox2), vbox2);
  
  

  document_notebook = gtk_notebook_new ();
  //cerr<<"GUIDocument creating document notebook: "<<document_notebook<<endl;
  

  gtk_widget_set_usize(document_notebook, -1, 320);
  gtk_notebook_set_scrollable(GTK_NOTEBOOK(document_notebook), TRUE);
  gtk_notebook_popup_enable (GTK_NOTEBOOK(document_notebook));

  gtk_widget_ref (document_notebook);
  //gtk_object_set_data_full (GTK_OBJECT (mdi), "document_notebook", document_notebook,
  //                          (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (document_notebook);

  gtk_paned_pack1 (GTK_PANED(vbox2), document_notebook, TRUE, TRUE);
  //gtk_container_add (GTK_CONTAINER (vbox2), document_notebook);
  //gtk_box_pack_start (GTK_BOX (vbox2), document_notebook, TRUE, TRUE, 0);

  
  scrolledwindow1 = gtk_scrolled_window_new (NULL, NULL);
  gtk_widget_show (scrolledwindow1);
  gtk_widget_set_size_request(GTK_WIDGET(scrolledwindow1), -1, 100);
  gtk_paned_pack2 (GTK_PANED(vbox2), scrolledwindow1, FALSE, TRUE);
  //gnome_app_set_contents (GNOME_APP (), scrolledwindow1);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolledwindow1), GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);


  less2 = gtk_text_view_new ();
  gtk_widget_show (less2);
  gtk_container_add (GTK_CONTAINER (scrolledwindow1), less2);
  gtk_text_view_set_editable (GTK_TEXT_VIEW (less2), FALSE);
  gtk_text_view_set_wrap_mode (GTK_TEXT_VIEW (less2), GTK_WRAP_WORD);
  gtk_text_view_set_cursor_visible (GTK_TEXT_VIEW (less2), FALSE);
  /*gtk_text_buffer_set_text (gtk_text_view_get_buffer (GTK_TEXT_VIEW (less2)),
        _("FlowDesigner by Jean-Marc Valin & Dominic Letourneau"), -1);
  */

   
  
  gtk_widget_show(vbox2);

  //add document to the vflow application notebook
  vflowGUI::instance()->add_notebook_document(this, vbox2);


  //gtk_container_add (GTK_CONTAINER (notebook), vbox2);
  //label1 = gtk_label_new ((gchar *)docName.c_str());
  //gtk_widget_show (label1);
  //gtk_notebook_set_tab_label (GTK_NOTEBOOK (notebook), gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook), 0), label1);
  //gtk_label_set_justify (GTK_LABEL (label1), GTK_JUSTIFY_LEFT);
  //gtk_notebook_append_page(GTK_NOTEBOOK(vflow_notebook), vbox2, label1);
  //gtk_notebook_set_tab_label (GTK_NOTEBOOK (notebook), vbox2, label1);
  //if negative, last page will be used.
  //gtk_notebook_set_current_page (GTK_NOTEBOOK(vflow_notebook), -1);  


  less_print("FlowDesigner " VERSION " by Jean-Marc Valin & Dominic Letourneau");
  less_print("--");


}


/**********************************************************************************************************
This function is called when a GUINetwork is created and needs to add the document notebook page
Dominic Letourneau (13/09/2003)
**********************************************************************************************************/
void GUIDocument::add_notebook_network(GUINetwork *net, GtkWidget *child) {


  string tabName = net->getName();

  switch(net->getType()) {
  case UINetwork::subnet:
    tabName = tabName + " (subnet)";
    break;
  case UINetwork::iterator:
    tabName = tabName + " (iterator)";
    break;
  case UINetwork::threaded:
    tabName = tabName + " (threaded iterator)";
    break;
  default:
    tabName = tabName + " (unknown)";
  }

  //creating label for the notebook page
  GtkWidget *label1 = gtk_label_new ((gchar *)tabName.c_str());
  gtk_label_set_justify (GTK_LABEL (label1), GTK_JUSTIFY_LEFT);
  gtk_widget_show (label1);
  
  //append page to the notebook
  gtk_notebook_append_page(GTK_NOTEBOOK(getNotebook()), child, label1);

  //if negative, last page will be used.
  gtk_notebook_set_current_page (GTK_NOTEBOOK(getNotebook()), -1);

}

/**********************************************************************************************************
This function is called when a GUINetwork is deleted and needs to remove the document notebook page
Dominic Letourneau (13/09/2003)
**********************************************************************************************************/
void GUIDocument::remove_notebook_network(GUINetwork *net, GtkWidget *child) {

  int page_num = gtk_notebook_page_num(GTK_NOTEBOOK(getNotebook()),child);

  if (page_num != -1) {
    gtk_notebook_remove_page(GTK_NOTEBOOK(getNotebook()),page_num);
  }
  else {
    cerr<<"**ERROR** GUIDocument Widget not in notebook : "<<child<<" doc "<<net->getName()<<endl;
  }
}




GUIDocument::~GUIDocument()
{

  //cerr<<"GUI Document destroyed : "<<destroyed<<endl;
  //cerr<<"number of networks : "<<networks.size()<<endl;
   //Copy of the UIDocument destructor
   if (!destroyed)
   {
      for (unsigned int i=0;i<networks.size();i++)
      {
         delete networks[i];
         networks[i]=NULL;
      }

      for (unsigned int i=0;i<textParams.size();i++)
         delete textParams[i];
      
      for (unsigned int i=0;i<docInputs.size();i++)
         delete docInputs[i];
      
      for (unsigned int i=0;i<docOutputs.size();i++)
         delete docOutputs[i];
      
      for (unsigned int i=0;i<docParams.size();i++)
         delete docParams[i];
      
      vflowGUI::instance()->remove_notebook_document(this,vbox2);
      
      gtk_widget_destroy(vbox2);
      
      destroyed=true;
   }

}

GtkWidget *create_close_dialog (const char *close_str)
{
  GtkWidget *dialog1;
  GtkWidget *dialog_vbox1;
  GtkWidget *hbox1;
  GtkWidget *image1;
  GtkWidget *label2;
  GtkWidget *dialog_action_area1;
  GtkWidget *cancelbutton1;
  GtkWidget *okbutton1;

  dialog1 = gtk_dialog_new ();
  gtk_window_set_title (GTK_WINDOW (dialog1), _("dialog1"));

  dialog_vbox1 = GTK_DIALOG (dialog1)->vbox;
  gtk_widget_show (dialog_vbox1);

  hbox1 = gtk_hbox_new (FALSE, 0);
  gtk_widget_show (hbox1);
  gtk_box_pack_start (GTK_BOX (dialog_vbox1), hbox1, TRUE, TRUE, 0);

  image1 = gtk_image_new_from_stock ("gtk-dialog-warning", GTK_ICON_SIZE_BUTTON);
  gtk_widget_show (image1);
  gtk_box_pack_start (GTK_BOX (hbox1), image1, TRUE, TRUE, 0);

  label2 = gtk_label_new (close_str);
  gtk_widget_show (label2);
  gtk_box_pack_start (GTK_BOX (hbox1), label2, FALSE, FALSE, 0);
  gtk_label_set_justify (GTK_LABEL (label2), GTK_JUSTIFY_LEFT);

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
  GLADE_HOOKUP_OBJECT (dialog1, hbox1, "hbox1");
  GLADE_HOOKUP_OBJECT (dialog1, image1, "image1");
  GLADE_HOOKUP_OBJECT (dialog1, label2, "label2");
  GLADE_HOOKUP_OBJECT_NO_REF (dialog1, dialog_action_area1, "dialog_action_area1");
  GLADE_HOOKUP_OBJECT (dialog1, cancelbutton1, "cancelbutton1");
  GLADE_HOOKUP_OBJECT (dialog1, okbutton1, "okbutton1");

  gtk_widget_show(dialog1);
  return dialog1;
}


int GUIDocument::closeRequest(bool force)
{
   bool close;
   if (modified && !force)
   {
      string mesg = string(_("Do you really want to close ")) + docName + "?";
      GtkWidget *dialog = create_close_dialog(mesg.c_str());
      gint result = gtk_dialog_run (GTK_DIALOG (dialog));
      switch (result)
      {
      case GTK_RESPONSE_OK:
         close = true;
         break;
      default:
         close = false;
         break;
      }
      gtk_widget_destroy (dialog);
   } else {
      close = true;
   }
   
   if (close)
   {
      //Commit suicide, it's not really clean but I think it's OK
      delete this;
   }

   return close;
}

void GUIDocument::save()
{
   less_print(docName + " saved");
   UIDocument::save();
}

void GUIDocument::load()
{
   //cerr << "GUIDocument::load\n";

   UIDocument::load();
	
   for (int i=0;i<networks.size();i++)
   {
      dynamic_cast<GUINetwork *> (networks[i])->updateScroll();
   }

   //resizing parameters
   params.resize(textParams.size());
   
}



void GUIDocument::removeCurrentNet() {


  GUINetwork *current_net = dynamic_cast<GUINetwork*>(getCurrentNet());
  
  if (current_net) {
  
    vector<UINetwork*> new_vect;
   
    for (int i = 0; i < networks.size(); i++) {
      if (networks[i]->getName() == current_net->getName()) {
	
	//cerr<<"remove current net is : "<<current_net->getName()<<endl;
	
	int page_num = gtk_notebook_get_current_page(GTK_NOTEBOOK(document_notebook));

	//cerr<<"page num is : "<<page_num<<endl;

	//remove notebook page
	if (page_num != -1) {
	  //cerr<<"removing notebook page"<<endl;
	  gtk_notebook_remove_page(GTK_NOTEBOOK(document_notebook),page_num);
	  
	  //delete network
	  //cerr<<"deleting network"<<endl;
	  delete networks[i];
	}
      }
      else {
	//rebuilding networks
	new_vect.push_back(networks[i]);
      }     
    }//for

    //update network list
    networks = new_vect;
  
  }
}


void GUIDocument::renameCurrentNet() {

  int netID = gtk_notebook_get_current_page (GTK_NOTEBOOK(document_notebook));
  GUINetwork *net = dynamic_cast<GUINetwork*>(getCurrentNet());
 
  if (net && netID != -1) {
  
    string message;
    message = string("Rename network ") + net->getName();
   
    string newName = ask_string_dialog(message.c_str(), "");
    if (newName != "") {
     
      //update network name
      net->rename(newName);

      //update notebook label
      gtk_notebook_set_tab_label_text(GTK_NOTEBOOK(document_notebook), net->getView(), newName.c_str());

      //SHOULD RENAME ALL SUBNET NODES ?
      
    }
  }   
}

UINetwork* GUIDocument::getCurrentNet() {

  int netID = gtk_notebook_get_current_page (GTK_NOTEBOOK(document_notebook));

  if (netID == -1) {
    return NULL;
  }
  else {
    GtkWidget *page = gtk_notebook_get_nth_page (GTK_NOTEBOOK(document_notebook), netID);
    return (GUINetwork*)gtk_object_get_data(GTK_OBJECT(page), "net");
  }

}


UINetwork *GUIDocument::newNetwork(const string &_name, UINetwork::Type type)
{
   return new GUINetwork(this, _name, type);
}

UINetwork *GUIDocument::newNetwork(xmlNodePtr _net)
{
   return new GUINetwork(this, _net);
}


void GUIDocument::setFullPath(const string &fullpath)
{
   // call the non-gui code in UIDocument
   UIDocument::setFullPath(fullpath);
}

static void param_apply (GnomePropertyBox *propertybox, gint arg1, gpointer user_data)
{
   ((GUIDocument *)(user_data))->applyParams();
}

static void entry_changed (GtkEntry *entry, gpointer user_data)
{
   ((GUIDocument *)(user_data))->changedParams();
}

static void type_changed (GnomePropertyBox *propertybox, gpointer user_data)
{
   ((GUIDocument *)(user_data))->changedParams();
}

void GUIDocument::showParams() {

   UINetwork *uinet = getNetworkNamed("MAIN");
   
   GUINetwork *net = dynamic_cast<GUINetwork*>(uinet);


  //FIXME : FIND A BETTER IMPLEMENTATION !
  if (net) {

    vector<ItemInfo *> my_params;
    
    net->insertNetParams(my_params);
    
    //cerr<<"params size "<<my_params.size()<<endl;
    
    //looking for non existing parameters
    for (int i = 0; i < my_params.size(); i++) {
      
      bool found = false;
      
      for (int j = 0; j < textParams.size(); j++) {
	if (textParams[j]->name == my_params[i]->name) {
	  found = true;
	  break;
	}
      }
      if (!found) {
	//adding parameter
	addParameterText(my_params[i]->name,my_params[i]->value,my_params[i]->type);   
      }
    }
    
    list<DocParameterDataText*> tmp_list;

    for (int i = 0; i < textParams.size(); i++) {

      bool found = false;
 
      for (int j = 0; j < my_params.size(); j++) {
	if (textParams[i]->name == my_params[j]->name) {
	  found = true;
	  break;
	}
      }
      if (!found) {
	//removing parameter
	delete textParams[i];
	textParams[i] = NULL;
      }
      else {
	//keeping the parameter
	tmp_list.push_back(textParams[i]);
      }
    }
    
    textParams.resize(tmp_list.size());

    int i = 0; 
    list<DocParameterDataText*>::iterator iter = tmp_list.begin();

    for ( ;iter != tmp_list.end(); iter++,i++) {
      textParams[i] = (*iter);
    }

    createParamDialog();  
    
    gtk_widget_show (docproperty);
  }
  else {
    cerr<<"MAIN subnet does not exist..."<<endl;
  }

}

void GUIDocument::changedParams()
{
   gnome_property_box_changed(GNOME_PROPERTY_BOX(docproperty));
}

void GUIDocument::applyParams()
{
   for (int i=0;i<params.size();i++)
   {
      //GtkWidget *gtk_option_menu_get_menu(params[i].optionmenu);
      GtkWidget *menu = gtk_menu_get_active (GTK_MENU(params[i].optionmenu_menu));
      textParams[i]->type = (char *)gtk_object_get_user_data (GTK_OBJECT(menu));

      GtkWidget *gtkentr = gnome_entry_gtk_entry(GNOME_ENTRY(params[i].entry));
      textParams[i]->value = gtk_entry_get_text(GTK_ENTRY(gtkentr));
      
      //cerr << "<param: " << params[i].name << ", " << params[i].type << ":" << params[i].value << ">\n";
   }

   //updating category
   GtkCombo *combo = GTK_COMBO(gtk_object_get_data(GTK_OBJECT(docproperty), "category_combo"));
   string entry_text(gtk_entry_get_text(GTK_ENTRY(combo->entry))); //get entry text
   setCategory(entry_text);


   //updating comments
   GtkTextView *text1 = GTK_TEXT_VIEW(gtk_object_get_data(GTK_OBJECT(docproperty), "text1"));
   GtkTextBuffer* buffer = gtk_text_view_get_buffer(text1);
   GtkTextIter start, end;
   gtk_text_buffer_get_bounds(buffer, &start, &end);   
   setComments(gtk_text_buffer_get_text (buffer, &start, &end, TRUE));

   setModified();
}

void GUIDocument::insertLoadedParam(DocParameterData *param, string type, string value)
{

/*
   const vector<string> &types=ObjectParam::allTypes(false);
   for (int i=0;i<types.size();i++)
      if (types[i] == type)
         gtk_option_menu_set_history (GTK_OPTION_MENU (param->optionmenu), i);


   GtkWidget *gtkentr = gnome_entry_gtk_entry(GNOME_ENTRY(param->entry));
   gtk_entry_set_text(GTK_ENTRY(gtkentr),(gchar *)value.c_str());
*/

}

void GUIDocument::createParamDialog()
{
   int i;


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

  docproperty = gnome_property_box_new ();
  //gnome_dialog_close_hides (GNOME_DIALOG(docproperty), TRUE);

  gtk_object_set_data (GTK_OBJECT (docproperty), "docproperty", docproperty);

  notebook2 = GNOME_PROPERTY_BOX (docproperty)->notebook;
  gtk_object_set_data (GTK_OBJECT (docproperty), "notebook2", notebook2);
  gtk_widget_show (notebook2);

  table2 = gtk_table_new (2+params.size(), 3, FALSE);
  gtk_widget_ref (table2);
  gtk_object_set_data_full (GTK_OBJECT (docproperty), "table2", table2,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (table2);
  gtk_container_add (GTK_CONTAINER (notebook2), table2);

  label14 = gtk_label_new (_("Name"));
  gtk_widget_ref (label14);
  gtk_object_set_data_full (GTK_OBJECT (docproperty), "label14", label14,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label14);
  gtk_table_attach (GTK_TABLE (table2), label14, 0, 1, 0, 1,
                    (GtkAttachOptions) (0),
                    (GtkAttachOptions) (0), 0, 0);

  label15 = gtk_label_new (_("Type"));
  gtk_widget_ref (label15);
  gtk_object_set_data_full (GTK_OBJECT (docproperty), "label15", label15,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label15);
  gtk_table_attach (GTK_TABLE (table2), label15, 1, 2, 0, 1,
                    (GtkAttachOptions) (0),
                    (GtkAttachOptions) (0), 0, 0);

  label16 = gtk_label_new (_("Value"));
  gtk_widget_ref (label16);
  gtk_object_set_data_full (GTK_OBJECT (docproperty), "label16", label16,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label16);
  gtk_table_attach (GTK_TABLE (table2), label16, 2, 3, 0, 1,
                    (GtkAttachOptions) (0),
                    (GtkAttachOptions) (0), 0, 0);

  hseparator4 = gtk_hseparator_new ();
  gtk_widget_ref (hseparator4);
  gtk_object_set_data_full (GTK_OBJECT (docproperty), "hseparator4", hseparator4,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (hseparator4);
  gtk_table_attach (GTK_TABLE (table2), hseparator4, 0, 1, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);

  hseparator5 = gtk_hseparator_new ();
  gtk_widget_ref (hseparator5);
  gtk_object_set_data_full (GTK_OBJECT (docproperty), "hseparator5", hseparator5,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (hseparator5);
  gtk_table_attach (GTK_TABLE (table2), hseparator5, 1, 2, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (GTK_FILL), 0, 0);

  hseparator6 = gtk_hseparator_new ();
  gtk_widget_ref (hseparator6);
  gtk_object_set_data_full (GTK_OBJECT (docproperty), "hseparator6", hseparator6,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (hseparator6);
  gtk_table_attach (GTK_TABLE (table2), hseparator6, 2, 3, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (GTK_FILL), 0, 0);


  //making sure we have allocated enough GUI widgets for all parameters
  params.resize(textParams.size());
  
  for (i=0; i < textParams.size(); i++) {

    //creating label
    params[i].label = gtk_label_new (textParams[i]->name.c_str());
    gtk_widget_ref (params[i].label);
    gtk_object_set_data_full (GTK_OBJECT (docproperty), "label", params[i].label,
			      (GtkDestroyNotify) gtk_widget_unref);
    gtk_widget_show (params[i].label);
    gtk_table_attach (GTK_TABLE (table2), params[i].label, 0, 1, 2+i, 3+i,
		      (GtkAttachOptions) (0),
		      (GtkAttachOptions) (0), 0, 0);
    
    
    //creating option menu
    params[i].optionmenu = gtk_option_menu_new ();
    gtk_widget_ref (params[i].optionmenu);
    gtk_object_set_data_full (GTK_OBJECT (docproperty), "optionmenu", params[i].optionmenu,
			      (GtkDestroyNotify) gtk_widget_unref);
    gtk_widget_show (params[i].optionmenu);
    gtk_table_attach (GTK_TABLE (table2), params[i].optionmenu, 1, 2, 2+i, 3+i,
		      (GtkAttachOptions) (0),
		      (GtkAttachOptions) (0), 0, 0);
    params[i].optionmenu_menu = gtk_menu_new ();
    
    const vector<string> &types=ObjectParam::allTypes(false);
    for (int j=0;j<types.size();j++)
      {
        glade_menuitem = gtk_menu_item_new_with_label ((const gchar *)types[j].c_str());
        gtk_object_set_user_data(GTK_OBJECT(glade_menuitem), (void *)types[j].c_str());
        gtk_widget_show (glade_menuitem);
        gtk_menu_append (GTK_MENU (params[i].optionmenu_menu), glade_menuitem);
      }
    
    gtk_option_menu_set_menu (GTK_OPTION_MENU (params[i].optionmenu), params[i].optionmenu_menu);
    
    //setting history according to the type
    for (int j=0;j<types.size();j++) {
      if (types[j] == textParams[i]->type) {
	gtk_option_menu_set_history (GTK_OPTION_MENU (params[i].optionmenu), j);
      }
    }        
    
    gtk_signal_connect (GTK_OBJECT ( params[i].optionmenu_menu ), "selection-done",
                        GTK_SIGNAL_FUNC( type_changed), this);
    
    
    
    //entry
    params[i].entry = gnome_entry_new (NULL);
    gtk_widget_ref (params[i].entry);
    gtk_object_set_data_full (GTK_OBJECT (docproperty), "entry", params[i].entry,
			      (GtkDestroyNotify) gtk_widget_unref);

    gtk_widget_show (params[i].entry);
	

    gtk_table_attach (GTK_TABLE (table2), params[i].entry, 2, 3, 2+i, 3+i,
		      (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		      (GtkAttachOptions) (0), 0, 0);
    
    params[i].combo_entry = gnome_entry_gtk_entry (GNOME_ENTRY (params[i].entry));
    gtk_widget_ref (params[i].combo_entry);
    gtk_object_set_data_full (GTK_OBJECT (docproperty), "combo_entry", params[i].combo_entry,
			      (GtkDestroyNotify) gtk_widget_unref);
    gtk_widget_show (params[i].combo_entry);

    //setting combo entry text
    gtk_entry_set_text (GTK_ENTRY(params[i].combo_entry), textParams[i]->value.c_str());  

    
    GtkWidget *gtkentr = gnome_entry_gtk_entry(GNOME_ENTRY(params[i].entry));
    gtk_signal_connect (GTK_OBJECT ( gtkentr  ), "changed",
			GTK_SIGNAL_FUNC(entry_changed), this);
  }
  
  label12 = gtk_label_new (_("Parameters"));
  gtk_widget_ref (label12);
  gtk_object_set_data_full (GTK_OBJECT (docproperty), "label12", label12,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label12);
  gtk_notebook_set_tab_label (GTK_NOTEBOOK (notebook2), gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook2), 0), label12);


  GtkWidget *vbox_comments = gtk_vbox_new(false,1); //not homogenous, 0 spacing
  gtk_widget_ref(vbox_comments);
  gtk_object_set_data_full(GTK_OBJECT(docproperty), "vbox_comments", vbox_comments,
			   (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show(vbox_comments);
  gtk_container_add(GTK_CONTAINER(notebook2),vbox_comments);

  //category hbox
  GtkWidget *hbox_category = gtk_hbox_new(false,1);
  gtk_widget_ref(hbox_category);
  gtk_object_set_data_full(GTK_OBJECT(docproperty), "hbox_category", hbox_category,
			   (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show(hbox_category);
  gtk_box_pack_start_defaults(GTK_BOX(vbox_comments), GTK_WIDGET(hbox_category));
  gtk_widget_show(hbox_category);

  //category label
  GtkWidget *category_label = gtk_label_new("Category");
  gtk_widget_ref(category_label);
  gtk_object_set_data_full(GTK_OBJECT(docproperty), "category_label", category_label,
			   (GtkDestroyNotify) gtk_widget_unref);
  gtk_box_pack_start_defaults(GTK_BOX(hbox_category), GTK_WIDGET(category_label));
  gtk_widget_show(category_label);


  //category combo box
  GtkWidget *category_combo = gtk_combo_new();
  gtk_widget_ref(category_combo);
  gtk_object_set_data_full(GTK_OBJECT(docproperty),"category_combo",category_combo,
			   (GtkDestroyNotify) gtk_widget_unref);
  gtk_box_pack_end_defaults(GTK_BOX(hbox_category),  GTK_WIDGET(category_combo));

  //write category in the combo box
  GList *combo_list = NULL;
  combo_list = g_list_append(combo_list, (char*) getCategory().c_str() );
  gtk_combo_set_popdown_strings   (GTK_COMBO(category_combo),combo_list);

  //destroy list
  g_list_free(combo_list);

  gtk_widget_show(category_combo);

  //signal for entry in the combo box
  gtk_signal_connect (GTK_OBJECT (GTK_COMBO(category_combo)->entry), "changed",
			GTK_SIGNAL_FUNC(document_category_changed_event), this);

  //comments text area
  scrolledwindow2 = gtk_scrolled_window_new (NULL, NULL);
  gtk_widget_ref (scrolledwindow2);
  gtk_object_set_data_full (GTK_OBJECT (docproperty), "scrolledwindow2", scrolledwindow2,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (scrolledwindow2);
  gtk_box_pack_end_defaults(GTK_BOX(vbox_comments),  GTK_WIDGET(scrolledwindow2));

  //gtk_container_add (GTK_CONTAINER (notebook2),  scrolledwindow2);

  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolledwindow2), GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);

  text1 = gtk_text_view_new ();
  gtk_widget_ref (text1);

  //text view properties
  gtk_text_view_set_editable (GTK_TEXT_VIEW (text1), TRUE);
  gtk_text_view_set_wrap_mode (GTK_TEXT_VIEW (text1), GTK_WRAP_WORD);
  gtk_text_view_set_cursor_visible (GTK_TEXT_VIEW (text1), TRUE);	 

  //set the comments		   
  gtk_text_buffer_set_text (gtk_text_view_get_buffer (GTK_TEXT_VIEW (text1)),
			    getComments().c_str(), -1);

  gtk_object_set_data_full (GTK_OBJECT (docproperty), "text1", text1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (text1);
  gtk_container_add (GTK_CONTAINER (scrolledwindow2), text1);
  gtk_text_view_set_editable(GTK_TEXT_VIEW(text1),TRUE);

  //comments signal handler
  g_object_connect (G_OBJECT (gtk_text_view_get_buffer(GTK_TEXT_VIEW(text1))), "signal::changed",
		    GTK_SIGNAL_FUNC(document_comments_changed_event), this,NULL);

  label13 = gtk_label_new (_("Comments"));
  gtk_widget_ref (label13);
  gtk_object_set_data_full (GTK_OBJECT (docproperty), "label13", label13,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label13);
  gtk_notebook_set_tab_label (GTK_NOTEBOOK (notebook2), gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook2), 1), label13);

  //gnome_property_box_changed(GNOME_PROPERTY_BOX(docproperty));
  gtk_signal_connect (GTK_OBJECT (docproperty), "apply",
                      GTK_SIGNAL_FUNC(param_apply), this);

}

void document_comments_changed_event (GtkTextBuffer *textbuffer, GUIDocument *document) {  
  gnome_property_box_set_modified (GNOME_PROPERTY_BOX(document->docproperty),TRUE);
}

void document_category_changed_event (GtkEntry *entry, GUIDocument *document) {

  //get the entry text
  string entry_text(gtk_entry_get_text(entry));   

  //change the category
  //will be changed when apply is pressed
  //document->setCategory(entry_text);
  
  gnome_property_box_set_modified (GNOME_PROPERTY_BOX(document->docproperty),TRUE);
  
}

static void threadFunct(GUIDocument *doc)
{
   doc->run();
}

void GUIDocument::threadRun()
{
   less_print("Running " + docName + "...");
   if (!isRunning)
   {
      isRunning=true;

      //Let's create the thread already in a detached state
      pthread_attr_t tattr;
      pthread_attr_init(&tattr);
      //pthread_attr_setdetachstate(&tattr,PTHREAD_CREATE_DETACHED);
      pthread_create(&runThread, &tattr, (void * (*)(void *))threadFunct, this);
      //pthread_detach(runThread); //Dominic

      
   }
}

//extern void set_run_mode (bool isRuning);

void GUIDocument::threadStop()
{   
   gdk_threads_leave();
   if (isRunning) {
      pthread_mutex_lock(&del_lock);
      isRunning=false;
      gdk_threads_enter();
      runningNet->stop();
      gdk_threads_leave();
      pthread_mutex_unlock(&del_lock);
      pthread_join(runThread, NULL);
   }
   gdk_threads_enter();
}

//Run with a GUI
void GUIDocument::run()
{
   //Network *net;
   try{
      ParameterSet parameters;
      {
	 //cerr << "there are " << params.size() << " params\n";
	 for (int i=0;i<params.size();i++)
	 {
	    DocParameterDataText *curr = textParams[i];
	    ParameterSet dummy;
	    ObjectRef value = ObjectParam::stringParam(curr->type, curr->value, dummy);
	    if (!value.isNil())
	       parameters.add(curr->name,value);
	 }

      }
      //cerr << "building net...\n";
      //parameters.print();
      
      bool buildError = false;

      runningNet = NULL;
      Network *net=NULL;
      //try {
	 net = build("MAIN", parameters);

      {
      if (net->getInputNode())
	 throw new GeneralException ("main network has input node", __FILE__, __LINE__);
      runningNet = net;
      //verifyConnect done by UIDocument
      //net->verifyConnect();
      
      //processing buffer requests
      for (int i = 0; ;i++) {
	 if (!net->hasOutput(i)) break;

	 ParameterSet req;
	 net->request(i,req);
      }
      
      //initializing
      net->initialize();

      //cerr << "running...\n";
      
      // Getting all the network outputs.
      for (int k=0; ;k++) {
	if (net->hasOutput(k)) {
	   stringstream execOut;
	   execOut << *net->getOutput(k,0);
	   

	   gdk_threads_enter();
	   less_print(execOut.str());
	   gdk_threads_leave();

	}
	else {
	  break;
	}
      }
      
      gdk_threads_enter();
      less_print("Exited normally");
      gdk_threads_leave();
      //delete net;
      //ask for params and desired output
      }  
      //run in a window in a separated thread
   } catch (BaseException *e)
   {
      stringstream excOut;

      e->print (excOut);
      gdk_threads_enter();
      less_print(excOut.str());
      gdk_threads_leave();
      //cerr << "exception caught\n";
      //e->print();
      //cerr << "---\n";

      delete e;
   } catch (UserException *e)
   {
      less_print("User stop");
      delete e;
   }
   catch (...)
   {
      gdk_threads_enter();
      less_print("Unknown exception caught");
      gdk_threads_leave();
   }
     
   gdk_threads_enter();
   vflowGUI::instance()->set_run_mode(false);
   gdk_threads_leave();

   pthread_mutex_lock(&del_lock);
   if (runningNet)
   {
      runningNet->cleanupNotify();
      delete runningNet;
      runningNet=NULL;
   }
   isRunning=false;
   pthread_mutex_unlock(&del_lock);

}


void GUIDocument::less_print(const string &message) 
{
  less_text += message + string("\n");
  
  if (less2) 
  {
     gtk_text_buffer_set_text (gtk_text_view_get_buffer (GTK_TEXT_VIEW (less2)),
                               less_text.c_str(), -1);

     GtkAdjustment* adj = gtk_scrolled_window_get_vadjustment
                                   (GTK_SCROLLED_WINDOW(scrolledwindow1));
     gtk_adjustment_set_value(adj, adj->upper);
     gtk_adjustment_value_changed(adj);
  }
  
}

void GUIDocument::less_print(const char *message) 
{
   /*less_text += string(message) + string("\n");

  if (less2) 
  {
     gtk_text_buffer_set_text (gtk_text_view_get_buffer (GTK_TEXT_VIEW (less2)),
                               less_text.c_str(), -1);
  }
   */
   less_print(string(message));
}

void GUIDocument::less_clear() 
{
   less_text = string();
   if (less2) 
   {
      gtk_text_buffer_set_text (gtk_text_view_get_buffer (GTK_TEXT_VIEW (less2)),
                                less_text.c_str(), -1);
   }
}

string GUIDocument::getNewNetName(UINetwork::Type type)
{
   string baseName;
   if (type == UINetwork::subnet)
      baseName = "SUBNET";
   else if (type == UINetwork::iterator)
      baseName = "LOOP";
   else if (type == UINetwork::threaded)
      baseName = "THREAD";
   bool OK=false;
   int nb=0;
   string newName;
   while (!OK)
   {
      OK=true;
      stringstream st;
      st << nb;
      newName = baseName + st.str();
      for (int i=0;i<networks.size();i++)
	 if (networks[i]->getName() == newName)
	 {
	    OK=false;
	    nb++;
	    break;
	 }
   }
   return newName;
}

void error_dismiss(GtkWidget *widget, gpointer user_data)
{
   gtk_widget_destroy(widget);
   //cerr << "called\n";
}

void GUIDocument::error(char *err)
{
   GtkWidget *errDialog = gnome_error_dialog (err);

   gtk_signal_connect (GTK_OBJECT ( errDialog ), "close",
		       GTK_SIGNAL_FUNC( error_dismiss), NULL);
}

void GUIDocument::updateSubnet() {

  for (int j = 0; j < networks.size(); j++) {
    for (int i= 0; i < networks.size(); i++) {
      networks[i]->newNetNotify("Subnet",networks[j]->getName());
    }
  }
}

