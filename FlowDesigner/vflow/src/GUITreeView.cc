#include "GUITreeView.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <bonobo.h>
#include <gnome.h>
#include <sstream>


#include "GUIDocument.h"
#include "GUINetwork.h"
#include "UINodeRepository.h"
#include "Node.h"
#include "vflow.h"

using namespace std;

namespace FD {

  void treeview_onRowActivated (GtkTreeView *treeview, GtkTreePath  *path, GtkTreeViewColumn  *col, GUITreeView *gView);
  gboolean treeview_onCursorChanged(GtkTreeView *treeview, GUITreeView *gView);

  void on_m_treeview_drag_begin               (GtkWidget       *widget,
					       GdkDragContext  *drag_context,
					       GUITreeView* View);

  void on_m_treeview_drag_data_get            (GtkWidget       *widget,
					       GdkDragContext  *drag_context,
					       GtkSelectionData *data,
					       guint            info,
					       guint            time,
					       GUITreeView*         gView);

  gboolean on_m_treeview_drag_drop                (GtkWidget       *widget,
						   GdkDragContext  *drag_context,
						   gint             x,
						   gint             y,
						   guint            time,
						   GUITreeView*         gView);

  void on_m_treeview_drag_end                 (GtkWidget       *widget,
					       GdkDragContext  *drag_context,
					       GUITreeView*         gView);

  gboolean on_canvas_button_press_event           (GtkWidget       *widget,
						   GdkEventButton  *event,
						   GUITreeView*         gView);

  gboolean on_canvas_button_release_event         (GtkWidget       *widget,
						   GdkEventButton  *event,
						   GUITreeView*         gView);

  void on_m_canvas_drag_begin                 (GtkWidget       *widget,
					       GdkDragContext  *drag_context,
					       GUITreeView*         gView);

  gboolean on_m_canvas_drag_drop                  (GtkWidget       *widget,
						   GdkDragContext  *drag_context,
						   gint             x,
						   gint             y,
						   guint            time,
						   GUITreeView*         gView);

  void on_m_canvas_grab_focus                 (GtkWidget       *widget,
					       GUITreeView*         gView);

  void on_m_searchEntry_changed (GtkEditable *editable, GUITreeView *gView);

#define GLADE_HOOKUP_OBJECT(component,widget,name)			\
  g_object_set_data_full (G_OBJECT (component), name,			\
			  gtk_widget_ref (widget), (GDestroyNotify) gtk_widget_unref)

#define GLADE_HOOKUP_OBJECT_NO_REF(component,widget,name)	\
  g_object_set_data (G_OBJECT (component), name, widget)

  GUITreeView::GUITreeView()
  {
   
    //TODO NULL WIDGETS


    //create widgets
    //createTreeView();
    //fillNodeTreeView();
  }


  GUITreeView::~GUITreeView() 
  {
    if (m_window) {
      gtk_object_destroy(GTK_OBJECT(m_window));
    }
  }


  static void  
  target_drag_data_received  (GtkWidget          *widget,
			      GdkDragContext     *context,
			      gint                x,
			      gint                y,
			      GtkSelectionData   *data,
			      guint               info,
			      guint               time)
  {
    g_print("Got: %s\n",data->data);
  }

  GtkWidget* GUITreeView::createTreeView() 
  {
    //m_window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    //gtk_window_set_title (GTK_WINDOW (m_window), _("TestTreeView"));


    m_window = gtk_handle_box_new ();    
    gtk_handle_box_set_shadow_type (GTK_HANDLE_BOX (m_window), GTK_SHADOW_ETCHED_IN);
    gtk_handle_box_set_handle_position (GTK_HANDLE_BOX (m_window), GTK_POS_LEFT);
    gtk_handle_box_set_snap_edge(GTK_HANDLE_BOX(m_window),GTK_POS_LEFT);

    m_horizontalPane = gtk_hpaned_new ();    
    gtk_widget_show (m_horizontalPane);
    gtk_container_add (GTK_CONTAINER (m_window), m_horizontalPane);
    gtk_paned_set_position (GTK_PANED (m_horizontalPane), 300);

    vbox2 = gtk_vbox_new (FALSE, 0);
    gtk_widget_show (vbox2);
    gtk_paned_pack1 (GTK_PANED (m_horizontalPane), vbox2, FALSE, TRUE);

    m_verticalPane = gtk_vpaned_new ();
    gtk_widget_show (m_verticalPane);
    gtk_box_pack_start (GTK_BOX (vbox2), m_verticalPane, TRUE, TRUE, 0);
    gtk_paned_set_position (GTK_PANED (m_verticalPane), 400);

    vbox3 = gtk_vbox_new (FALSE, 0);
    gtk_widget_show (vbox3);
    gtk_paned_pack1 (GTK_PANED (m_verticalPane), vbox3, FALSE, TRUE);

    hbox1 = gtk_hbox_new (FALSE, 0);
    gtk_widget_show (hbox1);
    gtk_box_pack_start (GTK_BOX (vbox3), hbox1, FALSE, FALSE, 0);

    m_searchEntry = gtk_entry_new ();
    gtk_widget_show (m_searchEntry);
    gtk_box_pack_start (GTK_BOX (hbox1), m_searchEntry, TRUE, TRUE, 0);

    hbuttonbox2 = gtk_hbutton_box_new ();
    gtk_widget_show (hbuttonbox2);
    gtk_box_pack_start (GTK_BOX (hbox1), hbuttonbox2, FALSE, FALSE, 0);

    m_searchButton = gtk_button_new_with_mnemonic (_("Search"));
    gtk_widget_show (m_searchButton);
    gtk_container_add (GTK_CONTAINER (hbuttonbox2), m_searchButton);
    GTK_WIDGET_SET_FLAGS (m_searchButton, GTK_CAN_DEFAULT);

    m_createButton = gtk_button_new_with_mnemonic (_("Create"));
    gtk_widget_show (m_createButton);
    gtk_container_add (GTK_CONTAINER (hbuttonbox2), m_createButton);
    GTK_WIDGET_SET_FLAGS (m_createButton, GTK_CAN_DEFAULT);

    m_treeviewScrollWindow = gtk_scrolled_window_new (NULL, NULL);
    gtk_widget_show (m_treeviewScrollWindow);
    gtk_box_pack_start (GTK_BOX (vbox3), m_treeviewScrollWindow, TRUE, TRUE, 0);
    gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (m_treeviewScrollWindow), GTK_SHADOW_IN);

    m_treeview = gtk_tree_view_new ();
    gtk_widget_show (m_treeview);
    gtk_container_add (GTK_CONTAINER (m_treeviewScrollWindow), m_treeview);

    m_textviewScrollWindow = gtk_scrolled_window_new (NULL, NULL);
    gtk_widget_show (m_textviewScrollWindow);
    gtk_paned_pack2 (GTK_PANED (m_verticalPane), m_textviewScrollWindow, TRUE, TRUE);
    gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (m_textviewScrollWindow), GTK_SHADOW_IN);

 

    m_textview = gtk_text_view_new ();
    gtk_widget_show (m_textview);
    gtk_container_add (GTK_CONTAINER (m_textviewScrollWindow), m_textview);
    gtk_widget_set_size_request (m_textview, 200, 500);
    gtk_text_view_set_editable(GTK_TEXT_VIEW(m_textview),FALSE);

    //TAGS FOR FONT TYPES
    GtkTextTag* tag_header = gtk_text_buffer_create_tag (gtk_text_view_get_buffer (GTK_TEXT_VIEW (m_textview)),
							 "header",
							 "font",
							 "courier 16",NULL);
    
    GtkTextTag*  tag_description = gtk_text_buffer_create_tag (gtk_text_view_get_buffer (GTK_TEXT_VIEW (m_textview)),
							       "normal",
							       "font",
							       "courier 12",NULL);



    gtk_tree_view_set_enable_search (GTK_TREE_VIEW(m_treeview),TRUE);
                                             
    //m_arrow = gtk_arrow_new (GTK_ARROW_LEFT, GTK_SHADOW_OUT);
    //gtk_widget_show (m_arrow);    
    //gtk_widget_set_size_request (m_arrow, 51, 51);

    //gtk_paned_pack2 (GTK_PANED (m_horizontalPane),m_arrow, FALSE, FALSE);
    



    g_signal_connect ((gpointer) m_searchEntry, "changed",
                      G_CALLBACK (on_m_searchEntry_changed),
                     this);

    g_signal_connect(m_treeview, "row-activated", (GCallback) treeview_onRowActivated, this);

    g_signal_connect(m_treeview, "cursor-changed", (GCallback) treeview_onCursorChanged,this);
			
    g_signal_connect ((gpointer) m_treeview, "drag_begin",
		      G_CALLBACK (on_m_treeview_drag_begin),
		      this);
    g_signal_connect ((gpointer) m_treeview, "drag_data_get",
		      G_CALLBACK (on_m_treeview_drag_data_get),
		      this);
    g_signal_connect ((gpointer) m_treeview, "drag_drop",
		      G_CALLBACK (on_m_treeview_drag_drop),
		      this);
    g_signal_connect ((gpointer) m_treeview, "drag_end",
		      G_CALLBACK (on_m_treeview_drag_end),
		      this);
    //g_signal_connect ((gpointer) m_canvas, "button_press_event",
    //		      G_CALLBACK (on_canvas_button_press_event),
    //		      this);
    //g_signal_connect ((gpointer) m_canvas, "button_release_event",
    //		      G_CALLBACK (on_canvas_button_release_event),
    //		      this);
    //g_signal_connect ((gpointer) m_canvas, "drag_begin",
    //		      G_CALLBACK (on_m_canvas_drag_begin),
    //		      this);
    //g_signal_connect ((gpointer) m_canvas, "drag_drop",
    //		      G_CALLBACK (on_m_canvas_drag_drop),
    //		      this);
    //g_signal_connect ((gpointer) m_canvas, "grab_focus",
    //		      G_CALLBACK (on_m_canvas_grab_focus),
    //		      this); 
	

    static GtkTargetEntry target_table[] = {
      { "text/plain", 0, 0 }
    };
    
    gtk_drag_source_set (m_treeview,
		     GDK_BUTTON1_MASK,
                     target_table, 1,
                     GDK_ACTION_COPY);
  	
    /* Store pointers to all widgets, for use by lookup_widget(). */
    GLADE_HOOKUP_OBJECT_NO_REF (m_window, m_window, "m_window");
    GLADE_HOOKUP_OBJECT (m_window, m_horizontalPane, "m_horizontalPane");
    GLADE_HOOKUP_OBJECT (m_window, vbox2, "vbox2");
    GLADE_HOOKUP_OBJECT (m_window, m_verticalPane, "m_verticalPane");
    GLADE_HOOKUP_OBJECT (m_window, vbox3, "vbox3");
    GLADE_HOOKUP_OBJECT (m_window, hbox1, "hbox1");
    GLADE_HOOKUP_OBJECT (m_window, m_searchEntry, "m_searchEntry");
    GLADE_HOOKUP_OBJECT (m_window, hbuttonbox2, "hbuttonbox2");
    GLADE_HOOKUP_OBJECT (m_window, m_searchButton, "m_searchButton");
    GLADE_HOOKUP_OBJECT (m_window, m_createButton, "m_createButton");
    GLADE_HOOKUP_OBJECT (m_window, m_treeviewScrollWindow, "m_treeviewScrollWindow");
    GLADE_HOOKUP_OBJECT (m_window, m_treeview, "m_treeview");
    GLADE_HOOKUP_OBJECT (m_window, m_textviewScrollWindow, "m_textViewScrollWindow");
    GLADE_HOOKUP_OBJECT (m_window, m_textview, "m_textview");
	
    gtk_widget_show (m_window);
    return m_window;
  }

  void GUITreeView::insertCategory(int level, const std::vector<std::string> &category, GtkTreeIter root, GtkTreeStore* treeStore) {

    GtkTreeIter treeIter;
    GtkTreeIter newIter;
    //try to find category named X
    bool found;
	
    if (level >= category.size()) return;

    //cerr<<"processing level  :"<<level<<endl;

    if (gtk_tree_model_iter_children (GTK_TREE_MODEL(treeStore),&treeIter,&root))
      {
	//we do have a children
	//lets find out if if the category was already inserted
	//cerr<<"level "<<level<<" ("<<category[level]<<") "<<" has children"<<endl;

	bool categoryFound = false;
	do {
	  gchar* categoryName;
	  gtk_tree_model_get(GTK_TREE_MODEL(treeStore),&treeIter,0,&categoryName,-1);
			
	  if (category[level] == string(categoryName)) {			
	    categoryFound = true;				
	    break;
	  }

	}
	while(gtk_tree_model_iter_next(GTK_TREE_MODEL(treeStore), &treeIter));
		
	if (!categoryFound) {
	  //was not found
	  //inserting category
	  gtk_tree_store_append (treeStore, &treeIter, &root);
	  gtk_tree_store_set (treeStore, &treeIter,0, category[level].c_str(),-1);
	}

	//call next level
	insertCategory(level + 1,category,treeIter,treeStore);
      }
    else {
      //cerr<<"creating new children for "<<category[level]<<endl;
      //create the children
      gtk_tree_store_append (treeStore, &newIter, &root);
      gtk_tree_store_set (treeStore, &newIter,0, category[level].c_str(),-1);
      //call next level
      insertCategory(level + 1,category,newIter,treeStore);
    }
  }


  std::vector<std::string> GUITreeView::extractCategory(const std::string &category) {

    vector<string> catLevels;
    string my_string(category);

    while (!my_string.empty())
      {
	int colonPos = my_string.find(":");
	if (colonPos == string::npos) {			
	  catLevels.push_back(my_string);
	  my_string.resize(0);
	}
	else {
	  catLevels.push_back(my_string.substr(0,colonPos));			
	  my_string.erase(0,colonPos + 1);
	}
      }
	
    return catLevels;
  }


  void GUITreeView::insertType(const std::string &typeName,  const std::vector<std::string> &category, GtkTreeIter root, GtkTreeStore* treeStore) {


    std::vector<std::string> vect = category;
    vect.push_back(typeName);
	
    insertCategory(0, vect, root, treeStore);


  }


  void GUITreeView::fillNodeTreeView()
  {

    //Getting all categories
    //std::map<std::string, NodeInfo *>
    UINodeRepository::iterator info = UINodeRepository::Begin();
    set<string> strCategories;
    while (info != UINodeRepository::End()) 
      {
	strCategories.insert(info->second->category);
	//addType(info->second->category,info->first);
	info++;
      }
 
    GtkTreeStore* treeStore = gtk_tree_store_new(1,G_TYPE_STRING);
    GtkTreeIter treeIter;
    GtkTreeIter rootIter;

    gtk_tree_store_append (treeStore, &rootIter, NULL);
    gtk_tree_store_set (treeStore, &rootIter,0,"Nodes",-1);

    //cerr<<"filling tree"<<endl;
    for(set<string>::iterator iter = strCategories.begin(); iter != strCategories.end(); iter++)
      {
	//cerr<<"processing : "<<(*iter)<<endl;
	vector<string> catLevels = extractCategory(*iter);	
	insertCategory(0,catLevels, rootIter, treeStore);
      }



    info = UINodeRepository::Begin();
    while (info != UINodeRepository::End()) 
      {
	if ((info->second->kind != NodeInfo::builtin) || (Node::getFactoryNamed(info->first)))
	  {
	    insertType(info->first, extractCategory(info->second->category), rootIter,treeStore);
	  }

	info++;
      }




    //cerr<<"creating renderer"<<endl;
    GtkCellRenderer* renderer = gtk_cell_renderer_text_new();

    //cerr<<"creating column"<<endl;
    GtkTreeViewColumn* column = gtk_tree_view_column_new_with_attributes ("Category",
									  renderer,
									  "text", 0,
									  NULL); 

    //cerr<<"append column"<<endl;
    gtk_tree_view_append_column(GTK_TREE_VIEW(m_treeview),column);

    //cerr<<"set model"<<endl;
    gtk_tree_view_set_model (GTK_TREE_VIEW (m_treeview), GTK_TREE_MODEL(treeStore));


    //g_object_unref (G_OBJECT (column));

    //gtk_tree_view_columns_autosize  (GTK_TREE_VIEW(m_treeview));
  
    //gtk_widget_show(GTK_WIDGET(m_treeview));

  }

  void GUITreeView::displayNode(const std::string &name) {
 

    double x1,y1,x2,y2;

    //creating the node name
    GnomeCanvasItem *nodeText = gnome_canvas_item_new(m_canvasGroup,
						      gnome_canvas_text_get_type(),
						      "x", 0.0,
						      "y", 0.0,
						      "text", name.c_str(),
						      "anchor", GTK_ANCHOR_CENTER,
						      "fill_color", "black",
						      "font", "sans 10",
						      NULL);

    //getting the node bounds
    gnome_canvas_item_get_bounds(nodeText, &x1,&y1, &x2, &y2);      
    gnome_canvas_item_raise_to_top(nodeText);


  }

  void GUITreeView::show()
  {
    if (m_window) 
    {
      gtk_widget_show(m_window);
    }
  }

  void GUITreeView::hide()
  {
    if (m_window) 
    {
      gtk_widget_hide(m_window);
    }
  }


  void GUITreeView::displayInfo(const std::string &name) 
  {
    NodeInfo *info = UINodeRepository::Find(name);

    if (info) {

      
      GtkTextIter start;
      GtkTextIter end;

      gtk_text_buffer_get_bounds (gtk_text_view_get_buffer (GTK_TEXT_VIEW (m_textview)),&start,&end);
      
  

      gtk_text_buffer_delete (gtk_text_view_get_buffer (GTK_TEXT_VIEW (m_textview)),&start,&end);
      
      gtk_text_buffer_get_end_iter(gtk_text_view_get_buffer (GTK_TEXT_VIEW (m_textview)),&end);
      gtk_text_buffer_insert_with_tags_by_name(gtk_text_view_get_buffer (GTK_TEXT_VIEW (m_textview)),
					       &end,
					       (const gchar*) "GENERAL INFORMATIONS\n",
					       21,
					       "header",
					       NULL);

      

      stringstream out1;
      out1<<"NAME: "<<name<<endl;

      out1<<"KIND: ";
      switch(info->kind) {
      case NodeInfo::builtin:
	out1<<"builtin"<<endl;
	break;
      case NodeInfo::subnet:
	out1<<"subnet"<<endl;
	break;
      case NodeInfo::external:
	out1<<"external"<<endl;
	break;
      }
      out1<<"CATEGORY: ";
      out1<<info->category<<endl;
      out1<<"SOURCE FILE: ";
      out1<<info->sourceFile<<endl;
      out1<<"REQUIRE LIST:";
      out1<<info->requireList<<endl;
      out1<<"DESCRIPTION: "<<endl;
      out1<<info->description<<endl;
      out1<<endl;


      gtk_text_buffer_get_end_iter(gtk_text_view_get_buffer (GTK_TEXT_VIEW (m_textview)),&end);
      gtk_text_buffer_insert_with_tags_by_name(gtk_text_view_get_buffer (GTK_TEXT_VIEW (m_textview)),
				       &end,
				       (const gchar*) out1.str().c_str(),
				       out1.str().size(),
				       "normal",
				       NULL);

      //INPUTS
      if (info->inputs.size() > 0)
      {

	gtk_text_buffer_get_end_iter(gtk_text_view_get_buffer (GTK_TEXT_VIEW (m_textview)),&end);
	gtk_text_buffer_insert_with_tags_by_name(gtk_text_view_get_buffer (GTK_TEXT_VIEW (m_textview)),
						 &end,
						 (const gchar*) "INPUTS\n",
						 7,
						 "header",
						 NULL);



	stringstream out2;
	
	for (int i = 0; i < info->inputs.size(); i++) {
	  out2<<"name : "<<info->inputs[i]->name<<endl;
	  out2<<"type : "<<info->inputs[i]->type<<endl;
	  out2<<"value : "<<info->inputs[i]->value<<endl;
	  out2<<"description : "<<info->inputs[i]->description<<endl;
	}
	out2<<endl;
      

      gtk_text_buffer_get_end_iter(gtk_text_view_get_buffer (GTK_TEXT_VIEW (m_textview)),&end);
      gtk_text_buffer_insert_with_tags_by_name(gtk_text_view_get_buffer (GTK_TEXT_VIEW (m_textview)),
				       &end,
				       (const gchar*) out2.str().c_str(),
				       out2.str().size(),
				       "normal",
				       NULL);
      }

      //OUTPUTS
      if (info->outputs.size() > 0)
      {
	gtk_text_buffer_get_end_iter(gtk_text_view_get_buffer (GTK_TEXT_VIEW (m_textview)),&end);
	gtk_text_buffer_insert_with_tags_by_name(gtk_text_view_get_buffer (GTK_TEXT_VIEW (m_textview)),
						 &end,
						 (const gchar*) "OUTPUTS\n",
						 8,
						 "header",
						 NULL);
	
	stringstream out3;
	for (int i = 0; i < info->outputs.size(); i++) {
	  out3<<"name : "<<info->outputs[i]->name<<endl;
	  out3<<"type : "<<info->outputs[i]->type<<endl;
	  out3<<"value : "<<info->outputs[i]->value<<endl;
	  out3<<"description : "<<info->outputs[i]->description<<endl;
	}
	out3<<endl;
	
      

      gtk_text_buffer_get_end_iter(gtk_text_view_get_buffer (GTK_TEXT_VIEW (m_textview)),&end);
      gtk_text_buffer_insert_with_tags_by_name(gtk_text_view_get_buffer (GTK_TEXT_VIEW (m_textview)),
					       &end,
					       (const gchar*) out3.str().c_str(),
					       out3.str().size(),
					       "normal",
					       NULL);

      }

      //PARAMETERS
      if (info->params.size() > 0)
      {
	gtk_text_buffer_get_end_iter(gtk_text_view_get_buffer (GTK_TEXT_VIEW (m_textview)),&end);
	gtk_text_buffer_insert_with_tags_by_name(gtk_text_view_get_buffer (GTK_TEXT_VIEW (m_textview)),
						 &end,
						 (const gchar*) "PARAMETERS\n",
						 11,
						 "header",
						 NULL);
	
	stringstream out4;
	for (int i = 0; i < info->params.size(); i++) {
	  out4<<"name : "<<info->params[i]->name<<endl;
	  out4<<"type : "<<info->params[i]->type<<endl;
	  out4<<"value : "<<info->params[i]->value<<endl;
	  out4<<"description : "<<info->params[i]->description<<endl;
	}
	out4<<endl;
	gtk_text_buffer_get_end_iter(gtk_text_view_get_buffer (GTK_TEXT_VIEW (m_textview)),&end);
	gtk_text_buffer_insert_with_tags_by_name(gtk_text_view_get_buffer (GTK_TEXT_VIEW (m_textview)),
						 &end,
						 (const gchar*) out4.str().c_str(),
						 out4.str().size(),
						 "normal",
						 NULL);
      }
    }

  }



  
  gboolean treeview_onCursorChanged(GtkTreeView *treeview, GUITreeView *gView)
  {
   
    GtkTreePath *path;
    GtkTreeViewColumn *focus_column;
    GtkTreeModel *model = gtk_tree_view_get_model(treeview);
    GtkTreeIter iter;
    GtkTreeIter children;

    gtk_tree_view_get_cursor(GTK_TREE_VIEW(treeview),&path,&focus_column);

    if (gtk_tree_model_get_iter(model, &iter, path))
    {
      //check if it's a leaf node (actual node type) only.
      if (!gtk_tree_model_iter_children (GTK_TREE_MODEL(model),&children,&iter)) 
      {
	gchar *name;		
	gtk_tree_model_get(model, &iter, 0, &name, -1);		
	//g_print ("Double-clicked row contains name %s\n", name);
	gView->displayInfo(string(name));		
	g_free(name);
      }
    }

    return TRUE;

  }


  void treeview_onRowActivated (GtkTreeView *treeview, GtkTreePath  *path, 
				GtkTreeViewColumn  *col, GUITreeView *gView)
  {
        
    GtkTreeModel *model = gtk_tree_view_get_model(treeview);
    GtkTreeIter iter;
    GtkTreeIter children;


    if (gtk_tree_model_get_iter(model, &iter, path))
    {
      //check if it's a leaf node (actual node type) only.
      if (!gtk_tree_model_iter_children (GTK_TREE_MODEL(model),&children,&iter)) 
      {
	gchar *name;		
	gtk_tree_model_get(model, &iter, 0, &name, -1);		

	//create node in the network
	GUINetwork *net = dynamic_cast<GUINetwork*>(vflowGUI::instance()->getCurrentDoc()->getCurrentNet());


	//virtual UINode *newNode(UINetwork* _net, std::string _name, std::string _type, double _x, double _y, bool doInit);

	if (net)
	{
	  double x1,y1,x2,y2;
	  net->get_visible_bounds(x1,y1,x2,y2);
	  net->addNode(name,x1 + (rand() % (int)(x2 - x1)),y1 + (rand() % (int)(y2-y1)));
	}

	g_free(name);
      }
    }
  }



  void
  on_m_treeview_drag_begin               (GtkWidget       *widget,
					  GdkDragContext  *drag_context,
					  GUITreeView*         gView)
  {
    //cerr<<"on_m_treeview_drag_begin"<<endl;
  }


  void
  on_m_treeview_drag_data_get            (GtkWidget       *widget,
					  GdkDragContext  *drag_context,
					  GtkSelectionData *data,
					  guint            info,
					  guint            time,
					  GUITreeView*         gView)
  {
    //cerr<<"on_m_treeview_drag_data_get"<<endl;

    GtkTreeView *treeView = GTK_TREE_VIEW(widget);

    
    GtkTreePath *path;
    GtkTreeViewColumn *column;
    gtk_tree_view_get_cursor(GTK_TREE_VIEW(widget),&path,&column);


    if (path) {

      GtkTreeIter iter;
      //cerr<<"path is not null"<<endl;
      gchar* categoryName;
      // gtk_tree_model_get(GTK_TREE_MODEL(path),NULL,0,&categoryName,-1);
      
      GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(gView->getTreeView()));
      gtk_tree_model_get_iter (model, &iter, path);


      gtk_tree_model_get(GTK_TREE_MODEL(model),&iter,0,&categoryName,-1);

      NodeInfo *nodeInfo = UINodeRepository::Find(categoryName);

      if (nodeInfo) {
	gtk_selection_data_set(data,GDK_SELECTION_TYPE_STRING,8,	
			       (const guchar*) categoryName, strlen(categoryName));	     
      }

      free(categoryName);      		
      gtk_tree_path_free(path);
      
    }

    //cerr<<"info"<<info<<endl;
    // cerr<<"time"<<time<<endl;
  }


  gboolean
  on_m_treeview_drag_drop                (GtkWidget       *widget,
					  GdkDragContext  *drag_context,
					  gint             x,
					  gint             y,
					  guint            time,
					  GUITreeView*         gView)
  {

    //cerr<<"on_m_treeview_drag_drop"<<endl;
    return FALSE;
  }


  void
  on_m_treeview_drag_end                 (GtkWidget       *widget,
					  GdkDragContext  *drag_context,
					  GUITreeView*         gView)
  {

  }


  gboolean
  on_canvas_button_press_event           (GtkWidget       *widget,
					  GdkEventButton  *event,
					  GUITreeView*         gView)
  {
    //cerr<<"on_canvas_button_press_event"<<endl;
    return FALSE;
  }


  gboolean
  on_canvas_button_release_event         (GtkWidget       *widget,
					  GdkEventButton  *event,
					  GUITreeView*         gView)
  {
    //cerr<<"on_canvas_button_release_event"<<endl;
    return FALSE;
  }


  void
  on_m_canvas_drag_begin                 (GtkWidget       *widget,
					  GdkDragContext  *drag_context,
					  GUITreeView*         gView)
  {
    //cerr<<"on_canvas_drag_begin"<<endl;
  }


  gboolean
  on_m_canvas_drag_drop                  (GtkWidget       *widget,
					  GdkDragContext  *drag_context,
					  gint             x,
					  gint             y,
					  guint            time,
					  GUITreeView*         gView)
  {
    //cerr<<"on_canvas_drag_drop"<<endl;
    //cerr<<"x"<<x<<" y"<<y<<endl;
    return TRUE;
  }


  void
  on_m_canvas_grab_focus                 (GtkWidget       *widget,
					  GUITreeView*         gView)
  {
    //cerr<<"on_m_canvas_grab_focus"<<endl;
  }



  void on_m_searchEntry_changed (GtkEditable *editable, GUITreeView *gView)
  {
    cerr<<"on_m_searchEntry_changed"<<endl;
  }





}//namespace FD
