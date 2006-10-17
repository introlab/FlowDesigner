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
                default:
                    //verify if this node is valid               
                    if (Node::getFactoryNamed(info->first))
                    {
                        //add node name at the end                  
                        categories.push_back(info->first);
                        insert(m_root,categories,0);                                                   
                    }                  
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

}//namespace FD
