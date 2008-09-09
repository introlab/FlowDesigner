/***********************************************************************************
** Copyright (C) 2006-2008 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca). 
** All rights reserved. 
**
** This program is free software; you can redistribute it and/or
** modify it under the terms of the GNU General Public License
** as published by the Free Software Foundation; either version 2
** of the License, or (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
***********************************************************************************/
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
        setSortingEnabled(true);
		sortByColumn(0,Qt::AscendingOrder);
        setDragEnabled(true);
    }

    
    void QtNodeTreeView::insert(const QTreeWidgetItem* root, const std::vector<std::string> &path, int level)
    {
    	
    	/*
    	for(unsigned int i = 0; i < path.size(); i++)
    		cerr<<":"<<path[i];
    	cerr<<endl;
    	 */                
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
                    if (level + 1 < (int) path.size())
                    {                                                
                        insert(child,path,level + 1);
                    }                  
                }                                                                          
            }
                         
            //if not found, insert all nodes
            if (!found)
            {
                QTreeWidgetItem *newroot = const_cast<QTreeWidgetItem *>(root);
				
                                          
                for (int i = level; i < (int) path.size(); i++)
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
      

        //Getting all categories (first pass)
        UINodeRepository::iterator info = UINodeRepository::Begin();
        //set<string> strCategories;
        while (info != UINodeRepository::End())
        {
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
				    else
				    {
				    	cerr<<"****** ERROR Unknown factory named :" << info->first << endl;
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
            size_t colonPos = my_string.find(":");
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
        //cerr<<"QtNodeTreeView::dragEnterEvent(QDragEnterEvent *event)"<<endl;
        event->accept();
    }

    void QtNodeTreeView::dragMoveEvent(QDragMoveEvent *event)
    {
        //cerr<<"QtNodeTreeView::dragMoveEvent(QDragMoveEvent *event)"<<endl;
        event->accept();
    }

    void QtNodeTreeView::dropEvent(QDropEvent *event)
    {
        //cerr<<"tNodeTreeView::dropEvent(QDropEvent *event)"<<endl;
        event->accept();
    }   
    
	bool QtNodeTreeView::dropMimeData(QTreeWidgetItem *parent, int index, const QMimeData *data, Qt::DropAction action)
	{
	
		//cerr<<"QtNodeTreeView::dropMimeData"<<endl;
		return false;
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
	
	
	void QtNodeTreeView::closeEvent ( QCloseEvent * event )
	{
		event->ignore();
	}
	
	
	
}//namespace FD
