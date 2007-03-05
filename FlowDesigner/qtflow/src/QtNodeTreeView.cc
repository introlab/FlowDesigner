//Copyright (C) 2006 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca)
#include "QtNodeTreeView.h"
#include <iostream>
#include <vector>
#include <string>
#include <set>
#include <QTreeWidgetItem>
#include <QLabel>
#include "Node.h"

using namespace std;

namespace FD
{

    QtNodeTreeView::QtNodeTreeView(QWidget *parent)
            : QTreeWidget(parent)
    {
        
        m_root = new QTreeWidgetItem(this);
        m_root->setText(0,"Nodes");
        scanNodeRepository();
        setDragEnabled(true);
    }

    
    void QtNodeTreeView::insert(const QTreeWidgetItem* root, const std::vector<std::string> &path, int level)
    {
        if (root)
        {
            bool found = false;         
            int childCount = root->childCount();
                     
            for (int i =0; i < childCount; i++)
            {
                QTreeWidgetItem *child = root->child(i);
                                        
                if (child->text(0).toStdString() == path[level])
                {
                    found = true;
                    if (level + 1 < path.size())
                    {                                                
                        insert(child,path,level + 1);
                    }                  
                }                                                                          
            }
                         
            //if not found, insert all nodes
            if (!found)
            {
                QTreeWidgetItem *newroot = const_cast<QTreeWidgetItem *>(root);
				
                                          
                for (int i = level; i < path.size(); i++)
                {
                    QTreeWidgetItem *item = new QTreeWidgetItem(newroot);
                    item->setText(0,path[i].c_str());
					
					
					
                    newroot = item;                                                            
                }
                //Create tooltip
                NodeInfo* info = UINodeRepository::Find(path.back());
                if (info)
                {                           
                    newroot->setToolTip(0,info->description.c_str());
                }                                           
            }        
        }                             
    }      
    
    void QtNodeTreeView::scanNodeRepository()
    {
        cerr<<"QtNodeTreeView::scanNodeRepository()"<<endl;

        //Getting all categories (first pass)
        UINodeRepository::iterator info = UINodeRepository::Begin();
        //set<string> strCategories;
        while (info != UINodeRepository::End())
        {
            //strCategories.insert(info->second->category);                    
            //addType(info->second->category,info->first);
            
            vector<string> categories = extractCategory(info->second->category);
                 
            switch(info->second->kind)
            {					
				case NodeInfo::builtin:
				    if (Node::getFactoryNamed(info->first))
                    {
                        //add node name at the end                  
                        categories.push_back(info->first);
                        insert(m_root,categories,0);                                                   
                    }                  
				break;
				
				case NodeInfo::subnet:
						
				break;
				
				case NodeInfo::external:
					//add node name at the end    
					categories.push_back(info->first);
					insert(m_root,categories,0);
				break;
			
                default:
					cerr<<"QtNodeTreeView::scanNodeRepository() - Unknown kind : "<<info->second->kind<<endl;					               
                break;                              
                                
            }                        
            info++;
        }

    }

    std::vector<std::string> QtNodeTreeView::extractCategory(const std::string &category)
    {

        vector<string> catLevels;
        string my_string(category);

        while (!my_string.empty())
        {
            int colonPos = my_string.find(":");
            if (colonPos == string::npos)
            {
                catLevels.push_back(my_string);
                my_string.resize(0);
            }
            else
            {
                catLevels.push_back(my_string.substr(0,colonPos));
                my_string.erase(0,colonPos + 1);
            }
        }
        
        return catLevels;
    }
    
    void QtNodeTreeView::dragEnterEvent(QDragEnterEvent *event)
    {
        cerr<<"QtNodeTreeView::dragEnterEvent(QDragEnterEvent *event)"<<endl;
        event->accept();
    }

    void QtNodeTreeView::dragMoveEvent(QDragMoveEvent *event)
    {
        cerr<<"QtNodeTreeView::dragMoveEvent(QDragMoveEvent *event)"<<endl;
        event->accept();
    }

    void QtNodeTreeView::dropEvent(QDropEvent *event)
    {
        cerr<<"tNodeTreeView::dropEvent(QDropEvent *event)"<<endl;
        event->accept();
    }   
    
	bool QtNodeTreeView::dropMimeData(QTreeWidgetItem *parent, int index, const QMimeData *data, Qt::DropAction action)
	{
	
		cerr<<"QtNodeTreeView::dropMimeData"<<endl;
	}
	
	QStringList QtNodeTreeView::mimeTypes () const
	{
		QStringList qstrList;
		// list of accepted mime types for drop
		qstrList.append("text/uri-list");
		return qstrList;
	}
 
 
	Qt::DropActions QtNodeTreeView::supportedDropActions () const
	{
		// returns what actions are supported when dropping
		return Qt::CopyAction | Qt::MoveAction;
	}

	
	
	void QtNodeTreeView::mouseMoveEvent(QMouseEvent *event)
	{
	    // if not left button - return
	    if (!(event->buttons() & Qt::LeftButton)) return;
	 
	    // if no item selected, return (else it would crash)
	    if (currentItem() == NULL) return;
	 
	    QDrag *drag = new QDrag(this);
	    QMimeData *mimeData = new QMimeData;
	 	 
	    // mime stuff
		mimeData->setText(currentItem()->text(0));
	    drag->setMimeData(mimeData);
	 
	    // start drag
	    drag->start(Qt::CopyAction | Qt::MoveAction);
	}
	
	
	
	
	
	
}//namespace FD
