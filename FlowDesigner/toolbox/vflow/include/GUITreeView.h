#ifndef _GUI_TREE_VIEW_H_
#define _GUI_TREE_VIEW_H_

#include <string>
#include <gnome.h>
#include <vector>


namespace FD {
  
  class GUITreeView 
  {
  public:

    GUITreeView();
    virtual ~GUITreeView();
    void displayInfo(const std::string &name);
    void displayNode(const std::string &name);			

    GtkWidget* getTreeView(){return m_treeview;}
    GtkWidget* createTreeView();
    void fillNodeTreeView();
    void show();
    void hide();

  protected:


    void insertCategory(int level, const std::vector<std::string> &category, 
			GtkTreeIter root, GtkTreeStore* treeStore);
    std::vector<std::string> extractCategory(const std::string &category);
    void insertType(const std::string &typeName,  const std::vector<std::string> &category, 
		    GtkTreeIter root, GtkTreeStore* treeStore);




    GtkWidget *m_window;
    GtkWidget *m_horizontalPane;
    GtkWidget *vbox2;
    GtkWidget *m_verticalPane;
    GtkWidget *vbox3;
    GtkWidget *hbox1;
    GtkWidget *m_searchEntry;
    GtkWidget *hbuttonbox2;
    GtkWidget *m_searchButton;
    GtkWidget *m_createButton;
    GtkWidget *m_treeviewScrollWindow;
    GtkWidget *m_treeview;    
    GtkWidget *m_textviewScrollWindow;
    GtkWidget *m_textview;
    GtkWidget *m_arrow;

    //Additionnal canvas items
    GnomeCanvasGroup *m_canvasGroup;
    GnomeCanvasItem *m_canvasNode;
  };



}//namespace FD

#endif
