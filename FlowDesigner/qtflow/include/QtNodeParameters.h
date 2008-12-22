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
#include <QAbstractTableModel>
#include <QTableView>
#include "UINode.h"
#include <QDialogButtonBox>
#include <QLabel>
#include <QTextEdit>

namespace FD {

    using namespace std;

    class QtNodeParameters;

    class AddTerminalDialog : public QDialog
    {
    	Q_OBJECT;
    public:

    	AddTerminalDialog(QWidget *parent = NULL)
    	 : QDialog(parent)
    	 {

    		setObjectName(QString::fromUtf8("AddTerminalDialog"));
			resize(400, 300);

			m_verticalLayout = new QVBoxLayout(this);
			m_verticalLayout->setObjectName(QString::fromUtf8("m_verticalLayout"));
			m_gridLayout = new QGridLayout();
			m_gridLayout->setObjectName(QString::fromUtf8("m_gridLayout"));
			m_nameLabel = new QLabel(this);
			m_nameLabel->setObjectName(QString::fromUtf8("m_nameLabel"));


			m_gridLayout->addWidget(m_nameLabel, 0, 0, 1, 1);

			m_typeLabel = new QLabel(this);
			m_typeLabel->setObjectName(QString::fromUtf8("m_typeLabel"));

			m_gridLayout->addWidget(m_typeLabel, 0, 1, 1, 1);

			m_terminalTypeLineEdit = new QLineEdit(this);
			m_terminalTypeLineEdit->setObjectName(QString::fromUtf8("m_terminalTypeLineEdit"));
			m_terminalTypeLineEdit->setText("any");

			m_gridLayout->addWidget(m_terminalTypeLineEdit, 1, 0, 1, 1);

			m_terminalNameLineEdit = new QLineEdit(this);
			m_terminalNameLineEdit->setObjectName(QString::fromUtf8("m_terminalNameLineEdit"));
			m_terminalNameLineEdit->setText("NEW_TERMINAL");

			m_gridLayout->addWidget(m_terminalNameLineEdit, 1, 1, 1, 1);


			m_verticalLayout->addLayout(m_gridLayout);

			m_descriptionLabel = new QLabel(this);
			m_descriptionLabel->setObjectName(QString::fromUtf8("m_descriptionLabel"));

			m_verticalLayout->addWidget(m_descriptionLabel);

			m_terminalDescriptionTextEdit = new QTextEdit(this);
			m_terminalDescriptionTextEdit->setObjectName(QString::fromUtf8("m_terminalDescriptionTextEdit"));
			m_terminalDescriptionTextEdit->setText("No description available.");

			m_verticalLayout->addWidget(m_terminalDescriptionTextEdit);





			//Button box
			m_buttonBox = new QDialogButtonBox(this);
			m_buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
			m_buttonBox->setOrientation(Qt::Horizontal);
			m_buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);
			m_verticalLayout->addWidget(m_buttonBox);


			//Connect signals
			connect(m_buttonBox, SIGNAL(accepted()), this, SLOT(accept()));
			connect(m_buttonBox, SIGNAL(rejected()), this, SLOT(reject()));
    	 }

    	QString getTerminalName()
    	{
    		return m_terminalNameLineEdit->text();
    	}

    	QString getTerminalType()
    	{
    		return m_terminalTypeLineEdit->text();
    	}

    	QString getTerminalDescription()
    	{
    		return m_terminalDescriptionTextEdit->toPlainText();
    	}

    protected:
    	QDialogButtonBox *m_buttonBox;
    	QVBoxLayout *m_verticalLayout;
		QGridLayout *m_gridLayout;
		QLabel *m_nameLabel;
		QLabel *m_typeLabel;
		QLineEdit *m_terminalTypeLineEdit;
		QLineEdit *m_terminalNameLineEdit;
		QLabel *m_descriptionLabel;
		QTextEdit *m_terminalDescriptionTextEdit;

    };


    class InputOutputModel : public QAbstractTableModel, public UINode::UINodeObserverIF
    {
    public:
    	InputOutputModel(QtNodeParameters *params, bool isInput);
    	~InputOutputModel();
    	virtual int rowCount ( const QModelIndex & parent = QModelIndex() ) const;
    	virtual int columnCount ( const QModelIndex & parent = QModelIndex() ) const;
    	virtual QVariant data ( const QModelIndex & index, int role = Qt::DisplayRole ) const;
    	virtual QVariant headerData ( int section, Qt::Orientation orientation, int role = Qt::DisplayRole ) const;
    	virtual bool setData ( const QModelIndex & index, const QVariant & value, int role = Qt::EditRole );
    	virtual Qt::ItemFlags flags ( const QModelIndex & index ) const;
    	virtual void notifyChanged(const FD::UINode* node);
    protected:
    	QtNodeParameters *m_params;
    	bool m_input;
    };




	class QtNodeParameters : public QDialog
	{
		Q_OBJECT;

		public:

		QtNodeParameters(UINode *node);
		static std::vector<std::string> getObjectTypes();
		UINode* getUINode(){return m_node;}

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
        InputOutputModel *m_inputModel;
        QTableView *m_inputModelView;
        InputOutputModel *m_outputModel;
        QTableView *m_outputModelView;

        protected:
        void addValues(int index, string type);
        void validParameters();
        void setView(const std::vector<ItemInfo *> &textParams);

        protected slots:
        void buttonClicked( QAbstractButton * button );

        void inputAddButtonClicked();
        void inputRemoveButtonClicked();
        void outputAddButtonClicked();
        void outputRemoveButtonClicked();

        void valueChanged();
        void typeChanged();

	};

}//namespace FD


#endif
