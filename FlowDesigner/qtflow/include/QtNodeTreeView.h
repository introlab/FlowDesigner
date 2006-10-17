//Copyright (C) 2006 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca) 
#ifndef _QTNODETREEVIEW_H_
#define _QTNODETREEVIEW_H_

#include <QTreeWidget>
#include "UINodeRepository.h"
#include <vector>
#include <string>
#include <QtGui>

namespace FD
{

class QtNodeTreeView : public  QTreeWidget
{

	Q_OBJECT
	public:
	QtNodeTreeView(QWidget *parent = NULL);


	protected:
	void scanNodeRepository();
	std::vector<std::string> extractCategory(const std::string &category);
    void insert(const QTreeWidgetItem* root, const std::vector<std::string> &path, int level);

    //Drag & Drop   
    void dragEnterEvent(QDragEnterEvent *event);
    void dragMoveEvent(QDragMoveEvent *event);
    void dropEvent(QDropEvent *event);
    
    
    QTreeWidgetItem *m_root;   

};






} //namespace FD

#endif

