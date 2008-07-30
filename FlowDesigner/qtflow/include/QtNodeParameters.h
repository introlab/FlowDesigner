/***********************************************************************************
** Copyright (C) 2006-2008 Laborius (http://www.gel.usherbrooke.ca/laborius/). 
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
#ifndef _QTNODEPARAMETERS_H_
#define _QTNODEPARAMETERS_H_

#include "UINode.h"
#include "UINodeParameters.h"
#include <QtGui/QDialog>
#include <QtGui/QDialogButtonBox>
#include <QtGui/QVBoxLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QTabWidget>
#include <QLineEdit>
#include <QComboBox>
#include <QGridLayout>

namespace FD {
    
    using namespace std;
    
	class QtNodeParameters : public QDialog
	{
		Q_OBJECT;
        
		public:
		
		QtNodeParameters(UINode *node);
		static std::vector<std::string> getObjectTypes();
		
		protected:
		
		QWidget* buildParametersTable();	
		UINode *m_node;
		QVBoxLayout *m_vLayout;
        QGridLayout *m_paramsLayout;
		QTabWidget *m_tabWidget;
		QDialogButtonBox *m_buttonBox;
        UINodeParameters *m_params;
        QList<QWidget*> m_valuesWidge;
        QList<QComboBox*> m_typesWidge;
        
        protected:
        void addValues(int index, string type);
        void validParameters();     
        void setView(const std::vector<ItemInfo *> &textParams);
        
        protected slots:
        void buttonClicked( QAbstractButton * button );
        void valueChanged(); 
        void typeChanged();
        
	};
    
}//namespace FD


#endif
