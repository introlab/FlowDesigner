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
#include "QtNodeParameters.h"
#include <QTextEdit>
#include <QDoubleSpinBox>
#include <QSpinBox>
#include <QLabel>
#include <QPushButton>
#include <vector>
#include <map>
#include "Object.h"
#include "UINodeRepository.h"
#include "UINode.h"
#include "UITerminal.h"
#include <QInputDialog>
#include <sstream>
#include <QtWebKit>
#include <QSplitter>
#include <QGroupBox>
#include <QToolBar>
#include <QHeaderView>

namespace FD {

	std::vector<std::string> QtNodeParameters::getObjectTypes()
	{
		std::map<std::string, _ObjectFactory*>& factoryMap = Object::ObjectFactoryDictionary();

		std::vector<std::string> typeVect;

		for (std::map<std::string, _ObjectFactory*>::iterator iter = factoryMap.begin(); iter != factoryMap.end(); iter++)
		{
			typeVect.push_back(iter->first);
		}

		return typeVect;
	}


	QtNodeParameters::QtNodeParameters(UINode *node)
    :	m_node(node)
  	{

		m_params  = m_node->getParameters();
		m_vLayout = new QVBoxLayout(this);

		m_tabWidget = new QTabWidget(this);

  		m_tabWidget->addTab(buildParametersTable(), "Parameters");
		m_tabWidget->addTab(new QTextEdit(this),"Comments");


		//Inputs / Outputs Layout
		QSplitter *splitter = new QSplitter(Qt::Vertical, this);
		QGroupBox *inputBox = new QGroupBox("Inputs",splitter);
		QGroupBox *outputBox = new QGroupBox("Outputs",splitter);
		QVBoxLayout *inputLayout = new QVBoxLayout(inputBox);
		inputLayout->setObjectName(QString::fromUtf8("InputsVerticalLayout"));
		QVBoxLayout *outputLayout = new QVBoxLayout(outputBox);
		outputLayout->setObjectName(QString::fromUtf8("OutputsVerticalLayout"));


		//Create TableView & Model for inputs
		m_inputModel = new InputOutputModel(this,true);
		m_inputModelView = new QTableView(inputBox);
		m_inputModelView->setModel(m_inputModel);
		m_inputModelView->setColumnWidth(0,200);
		m_inputModelView->horizontalHeader()->setStretchLastSection(true);
		//m_inputModelView->setCornerButtonEnabled(false);

		//Create TableView & Model for outputs
		m_outputModel = new InputOutputModel(this,false);
		m_outputModelView = new QTableView(outputBox);
		m_outputModelView->setModel(m_outputModel);
		m_outputModelView->setColumnWidth(0,200);
		m_outputModelView->horizontalHeader()->setStretchLastSection(true);
		//m_outputModelView->setCornerButtonEnabled(false);

		//Add widgets to layout
		//INPUTS
		QToolBar *inputToolbar = new QToolBar(this);
		QPushButton *addInputButton = new QPushButton("Add Input",this);
		QPushButton *removeInputButton = new QPushButton("Remove Input(s)",this);
		inputToolbar->addWidget(addInputButton);
		inputToolbar->addWidget(removeInputButton);
		connect(addInputButton,SIGNAL(clicked()),this,SLOT(inputAddButtonClicked()));
		connect(removeInputButton,SIGNAL(clicked()),this,SLOT(inputRemoveButtonClicked()));
		inputLayout->addWidget(inputToolbar);
		inputLayout->addWidget(m_inputModelView);

		//OUTPUTS
		QToolBar *outputToolbar = new QToolBar(this);
		QPushButton *addOutputButton = new QPushButton("Add Output",this);
		QPushButton *removeOutputButton = new QPushButton("Remove Output(s)",this);
		outputToolbar->addWidget(addOutputButton);
		outputToolbar->addWidget(removeOutputButton);
		connect(addOutputButton,SIGNAL(clicked()),this,SLOT(outputAddButtonClicked()));
		connect(removeOutputButton,SIGNAL(clicked()),this,SLOT(outputRemoveButtonClicked()));
		outputLayout->addWidget(outputToolbar);
		outputLayout->addWidget(m_outputModelView);



		//Add tab
		m_tabWidget->addTab(splitter,"Inputs/Outputs");

		QWebView *view = new QWebView(this);
		view->load(QUrl(QString("http://flowdesigner.sourceforge.net/phpwiki/index.php?") + QString(m_node->getType().c_str())));
		view->show();
		m_tabWidget->addTab(view,"Community Wiki");

	    QTextEdit *nodeInfo = new QTextEdit(this);
		m_tabWidget->addTab(nodeInfo,"Node Info");

		std::stringstream outStream;

		NodeInfo* info = UINodeRepository::Find(m_node->getType());

		if (info)
		{
			outStream << (*info);
		}
		nodeInfo->setText(outStream.str().c_str());

		nodeInfo->setReadOnly(true);

		m_vLayout->addWidget(m_tabWidget);

		m_buttonBox = new QDialogButtonBox(QDialogButtonBox::Apply | QDialogButtonBox::Cancel
        | QDialogButtonBox::Ok | QDialogButtonBox::RestoreDefaults
        , Qt::Horizontal, this);

        m_buttonBox->button(QDialogButtonBox::Apply)->setDisabled(true);

		m_vLayout->addWidget(m_buttonBox);

        connect(m_buttonBox, SIGNAL( clicked(QAbstractButton*) ), this, SLOT(buttonClicked( QAbstractButton*)) );

		setLayout(m_vLayout);

		resize(640,480);

	}


	QWidget* QtNodeParameters::buildParametersTable()
	{
        cerr<<"QtNodeParameters::buildParametersTable()"<<endl;
		QWidget *widget = new QWidget(this);

        /**************************/
        /**    Tab parameters    **/
        /**************************/

		m_paramsLayout = new QGridLayout(widget);

        //Title
		m_paramsLayout->addWidget(new QLabel("<b>NAME</b>",this),0,0,Qt::AlignLeft | Qt::AlignTop);
		m_paramsLayout->addWidget(new QLabel("<b>TYPE</b>",this),0,1,Qt::AlignLeft | Qt::AlignTop);
		m_paramsLayout->addWidget(new QLabel("<b>DESCRIPTION</b>",this),0,2,Qt::AlignLeft | Qt::AlignTop);
		m_paramsLayout->addWidget(new QLabel("<b>VALUE</b>",this),0,3,Qt::AlignLeft | Qt::AlignTop);



		if (m_params)
		{
			std::vector<ItemInfo *> &textParams = m_params->get_textParams();
            //m_typesWidge =  new QComboBox[textParams.size()];


			for (unsigned int i = 0; i < textParams.size(); i++)
			{
				//ADD NAME

                m_paramsLayout->addWidget(new QLabel(textParams[i]->name.c_str(),this),i+1,0,Qt::AlignLeft | Qt::AlignTop);

                //ADD TYPE

                if(m_params->get_defaultTextParams()[i]->type == "any" ||
                   m_params->get_defaultTextParams()[i]->type == "subnet_param")
                {
                    //Fill combo for "any" types
                    std::vector<std::string> typeVect = QtNodeParameters::getObjectTypes();
                    QComboBox *combo = new QComboBox(this);
                    combo->addItem("int");
                    combo->addItem("string");
                    combo->addItem("float");
                    combo->addItem("bool");
                    combo->addItem("object");
                    combo->addItem("subnet_param");
                    int index = combo->findText( textParams[i]->type.c_str() );
                    if( index != -1)
                        combo->setCurrentIndex(index);
                    else
                        combo->setCurrentIndex(0);

                    /*for (unsigned int j = 0; j < typeVect.size(); j++)
                    {
                        combo->addItem(typeVect[j].c_str());
                        //problem minuscule majuscule 1er lettre
                        if(textParams[i]->type.substr(1) == typeVect[j].substr(1))
                        {
                            combo->setCurrentIndex( j );
                        }
                    }*/

                    m_paramsLayout->addWidget(combo,i+1,1,Qt::AlignLeft);
                    connect(combo, SIGNAL( currentIndexChanged(int) ), this, SLOT( typeChanged() ) );
                    //The name is used to associate the comnbo with the value :
                    combo->setAccessibleName(QString::number(i));
                    m_typesWidge.append(combo);
                }
                else
                {
                    m_paramsLayout->addWidget(new QLabel(textParams[i]->type.c_str(),this),i+1,1,Qt::AlignLeft | Qt::AlignTop);
                }

                //ADD DESCRIPTION

				m_paramsLayout->addWidget(new QLabel(textParams[i]->description.c_str(),this),i+1,2,Qt::AlignLeft | Qt::AlignTop);

                //ADD VALUE
                addValues(i,textParams[i]->type);

			}
		}
		return widget;
	}


    //ADD different objects to  m_valuesWidge[index] by using the type of m_params->get_textParams()[index]->type
    //string -> QComboBox (value1;value2...) QLineEdit (value?)
    //bool   -> QComboBox (true or false)
    //int    -> QSpinBox
    //float  -> QDoubleSpinBox
    //other  -> QLineEdit

    void QtNodeParameters::addValues(int index, string type)
    {
        cerr<<"QtNodeParameters::addValues(int index) : ";
        cerr<<index<<endl;
        std::vector<ItemInfo *> &textParams = m_params->get_textParams();
        if( index < m_valuesWidge.size() )
        {
            m_paramsLayout->removeWidget( m_valuesWidge[index] );
            delete m_valuesWidge.takeAt(index);
        }
        unsigned int lastPos=0;
        if(type == "string")
        {
            string defaultString = m_params->get_defaultTextParams()[index]->value;
            QComboBox* combo =  new QComboBox(this);
            combo->setAccessibleName("QComboBox");

            ////Fill combo with string like : "value1;value2;value3"
            for(unsigned int j = 0;j < defaultString.size(); j++)
            {
                if(defaultString[j]==';')
                {
                    combo->addItem(defaultString.substr(lastPos,j-lastPos).c_str());
                    lastPos = j+1;
                }
            }
            if(lastPos!=0)
            {
                combo->addItem(defaultString.substr(lastPos).c_str());
                combo->setCurrentIndex( combo->findText(textParams[index]->value.c_str(), Qt::MatchExactly));
                connect(combo, SIGNAL( currentIndexChanged(int) ), this, SLOT( valueChanged() ) );
                m_valuesWidge.insert(index, combo);
            }
            else
            {
                delete combo;
                QLineEdit* lineEdit = new QLineEdit(textParams[index]->value.c_str(),this);
                connect(lineEdit, SIGNAL( textChanged(const QString & ) ), this, SLOT( valueChanged() ) );
                lineEdit->setAccessibleName("QLineEdit");
                m_valuesWidge.insert(index, lineEdit);
            }

        }
        else if(type == "bool")
        {
            QComboBox* combo =  new QComboBox(this);
            combo->setAccessibleName("QComboBox");
            combo->addItem("true");
            combo->addItem("false");
            combo->setCurrentIndex( combo->findText( textParams[index]->value.c_str(), Qt::MatchExactly));
            connect(combo, SIGNAL( currentIndexChanged(int) ), this, SLOT( valueChanged() ) );
            m_valuesWidge.insert(index, combo);
        }
        else if(type == "int")
        {
            QSpinBox* spinBox =  new QSpinBox(this);
            spinBox->setAccessibleName("QSpinBox");
            //Maybe to change... extremum value of int generate a warning
            spinBox->setRange ( -10000000, 10000000 );
            spinBox->setValue( QString(textParams[index]->value.c_str()).toInt()  ) ;
            connect(spinBox, SIGNAL( valueChanged(int) ), this, SLOT( valueChanged() ) );
            m_valuesWidge.insert(index, spinBox);
        }
        else if(type == "float")
        {
            QDoubleSpinBox* doubleSpinBox =  new QDoubleSpinBox(this);
            doubleSpinBox->setAccessibleName("QDoubleSpinBox");
            //Maybe to change...
            doubleSpinBox->setRange( -1000000, 1000000 );
            doubleSpinBox->setValue( QString(textParams[index]->value.c_str()).toFloat()  ) ;
            connect(doubleSpinBox, SIGNAL( valueChanged(double) ), this, SLOT( valueChanged() ) );
            m_valuesWidge.insert(index, doubleSpinBox);
        }
        else
        {
            QLineEdit* lineEdit = new QLineEdit(textParams[index]->value.c_str(),this);
            connect(lineEdit, SIGNAL( textChanged(const QString & ) ), this, SLOT( valueChanged() ) );
            lineEdit->setAccessibleName("QLineEdit");
            m_valuesWidge.insert(index, lineEdit);
        }
        m_paramsLayout->addWidget(m_valuesWidge[index],index+1,3,Qt::AlignLeft | Qt::AlignTop);
    }

    void QtNodeParameters::validParameters()
    {
        cerr<<"QtNodeParameters::validParameters()"<<endl;
        if (m_params)
		{
            std::vector<ItemInfo *> &textParams = m_params->get_textParams();
            //Valid Types
            for (int i = 0; i < m_typesWidge.size(); i++)
            {
                int paramsIndex = m_typesWidge[i]->accessibleName().toInt();
                textParams[paramsIndex]->type = m_typesWidge[i]->currentText().toStdString();
            }
            //Valid Values
            for (unsigned int i = 0; i < textParams.size(); i++)
            {
                if(m_valuesWidge[i]->accessibleName() == "QLineEdit")
                    textParams[i]->value = dynamic_cast<QLineEdit *>(m_valuesWidge[i])->text().toStdString();
                else if(m_valuesWidge[i]->accessibleName() == "QComboBox")
                    textParams[i]->value = dynamic_cast<QComboBox *>(m_valuesWidge[i])->currentText().toStdString();
                else if(m_valuesWidge[i]->accessibleName() == "QSpinBox")
                    textParams[i]->value = dynamic_cast<QSpinBox *>(m_valuesWidge[i])->text().toStdString();
                else if(m_valuesWidge[i]->accessibleName() == "QDoubleSpinBox")
                    textParams[i]->value = dynamic_cast<QDoubleSpinBox *>(m_valuesWidge[i])->text().toStdString();
            }

            //Tell repository the parameters have changed.
            m_params->updateNetParams(textParams);
        }
    }

    void QtNodeParameters::buttonClicked (QAbstractButton * button )
    {
        cerr<<"QtNodeParameters::buttonClicked()"<<endl;
        if(button == (QAbstractButton*)m_buttonBox->button( QDialogButtonBox::Apply ))
        {
            validParameters();
            m_buttonBox->button(QDialogButtonBox::Apply)->setDisabled(true);
        }
        else if(button == (QAbstractButton*)m_buttonBox->button( QDialogButtonBox::Ok ))
        {
        	//TODO ASK FOR PARAMETERS UPDATE IF REQUIRED
        	if (m_buttonBox->button(QDialogButtonBox::Apply)->isEnabled())
        	{
        		validParameters();
            	m_buttonBox->button(QDialogButtonBox::Apply)->setDisabled(true);
        	}

            close();
        }
        else if(button == (QAbstractButton*)m_buttonBox->button( QDialogButtonBox::Cancel ))
        {
            setView(m_params->get_textParams());
            close();
        }
        else  if(button == (QAbstractButton*)m_buttonBox->button( QDialogButtonBox::RestoreDefaults ))
        {
            setView(m_params->get_defaultTextParams());
        }
        else
        {
            cerr<<"QtNodeParameters::buttonClicked() : unknown button"<<endl;
        }
    }


    void QtNodeParameters::inputAddButtonClicked()
    {
    	AddTerminalDialog dialog;

    	if (dialog.exec() == QDialog::Accepted)
    	{
    		//TODO : check for errors (duplicated names for instance)
    		m_node->addTerminal(dialog.getTerminalName().toStdString(),
    				UINetTerminal::INPUT,
    				dialog.getTerminalType().toStdString(),
    				dialog.getTerminalDescription().toStdString());
    	}
    }

    void QtNodeParameters::inputRemoveButtonClicked()
    {
    	QItemSelectionModel *selectionModel = m_inputModelView->selectionModel();

    	QModelIndexList indexes = selectionModel->selectedRows();

    	QModelIndex index;

    	foreach(index, indexes)   // loop through and remove them
    	{
    	        int row = index.row();
    	        m_inputModel->removeRows(row, 1, QModelIndex());
    	}
    }

    void QtNodeParameters::outputAddButtonClicked()
    {
    	AddTerminalDialog dialog;

    	if (dialog.exec() == QDialog::Accepted)
    	{
    		//TODO : check for errors (duplicated names for instance)
    		m_node->addTerminal(dialog.getTerminalName().toStdString(),
    				UINetTerminal::OUTPUT,
    				dialog.getTerminalType().toStdString(),
    				dialog.getTerminalDescription().toStdString());
    	}

    }

    void QtNodeParameters::outputRemoveButtonClicked()
    {
    	QItemSelectionModel *selectionModel = m_outputModelView->selectionModel();

    	QModelIndexList indexes = selectionModel->selectedRows();

    	QModelIndex index;

    	foreach(index, indexes)   // loop through and remove them
    	{
    	        int row = index.row();
    	        m_outputModel->removeRows(row, 1, QModelIndex());
    	}
    }


    //Set the parameters view with parameters give
    void QtNodeParameters::setView(const std::vector<ItemInfo *> &textParams)
    {
        cerr<<"QtNodeParameters::setView()"<<endl;
        //set type
        for (int i = 0; i < m_typesWidge.size(); i++)
        {
            int paramsIndex = m_typesWidge[i]->accessibleName().toInt();
            int index = m_typesWidge[i]->findText( textParams[paramsIndex]->type.c_str() );
            if( index != -1)
                m_typesWidge[i]->setCurrentIndex(index);
            else
            {
                m_typesWidge[i]->setCurrentIndex(0);
            }
        }
        //set Value
        for (unsigned int i = 0; i < textParams.size(); i++)
        {
            //Update Values View
            if(m_valuesWidge[i]->accessibleName() == "QLineEdit")
                dynamic_cast<QLineEdit *>(m_valuesWidge[i])->setText( textParams[i]->value.c_str() );
            else if(m_valuesWidge[i]->accessibleName() == "QSpinBox")
                dynamic_cast<QSpinBox *>(m_valuesWidge[i])->setValue( QString(textParams[i]->value.c_str()).toInt()  ) ;
            else if(m_valuesWidge[i]->accessibleName() == "QDoubleSpinBox")
                dynamic_cast<QDoubleSpinBox *>(m_valuesWidge[i])->setValue( QString(textParams[i]->value.c_str()).toInt()  ) ;
            else if(m_valuesWidge[i]->accessibleName() == "QComboBox")
            {
                QComboBox *comboBox = dynamic_cast<QComboBox *>(m_valuesWidge[i]);
                int index = comboBox->findText( textParams[i]->value.c_str() );
                if( index != -1)
                    comboBox->setCurrentIndex(index);
                else
                {
                    comboBox->setCurrentIndex(0);
                }
            }
        }
    }

    void QtNodeParameters::valueChanged()
    {
        cerr<<"QtNodeParameters::valueChanged()"<<endl;
        m_buttonBox->button(QDialogButtonBox::Apply)->setEnabled(true);
    }

    void QtNodeParameters::typeChanged()
    {
        cerr<<"QtNodeParameters::typeChanged()"<<endl;

        for (int i = 0; i < m_typesWidge.size(); i++)
        {
            int paramsIndex = m_typesWidge[i]->accessibleName().toInt();
            addValues(paramsIndex, m_typesWidge[i]->currentText().toStdString());
        }
        m_buttonBox->button(QDialogButtonBox::Apply)->setEnabled(true);
    }


	InputOutputModel::InputOutputModel(QtNodeParameters *params, bool isInput)
		: QAbstractTableModel(params), m_params(params), m_input(isInput)
	{
		if (m_params && m_params->getUINode())
		{
			m_params->getUINode()->registerEvents(this);
		}
	}

	InputOutputModel::~InputOutputModel()
	{
		if (m_params && m_params->getUINode())
		{
			m_params->getUINode()->unregisterEvents(this);
		}
	}

	int InputOutputModel::rowCount ( const QModelIndex & parent) const
	{
			UINode* node = m_params->getUINode();
			int size_input = node->getInputs().size();
			int size_output = node->getOutputs().size();
			if (m_input)
			{
				return size_input;
			}
			else
			{
				return size_output;
			}
	}

	int InputOutputModel::columnCount ( const QModelIndex & parent) const
	{
		return 2;
	}

	QVariant InputOutputModel::data ( const QModelIndex & index, int role) const
	{
		int row = index.row();
		int col = index.column();

		if (col < 2)
		{
			vector<UITerminal*> terminals;

			if (m_input)
			{
				terminals = m_params->getUINode()->getInputs();
			}
			else
			{
				terminals = m_params->getUINode()->getOutputs();
			}

			if (row < terminals.size())
			{
				switch(col)
				{
				case 0:
					return QVariant(terminals[row]->getName().c_str());
					break;
				case 1:
					return QVariant(terminals[row]->getDescription().c_str());
					break;
				}
			}
		}
		return QVariant();
	}

	QVariant InputOutputModel::headerData ( int section, Qt::Orientation orientation, int role) const
	{
		if (role == Qt::DisplayRole)
		{
			if (orientation == Qt::Vertical)
			{
				return QVariant(section);
			}
			else
			{
				switch(section)
				{
				case 0:
					return QVariant("Name");
					break;
				case 1:
					return QVariant("Description");
					break;
				}

			}
		}
		return QAbstractTableModel::headerData(section,orientation,role);
	}

	bool InputOutputModel::setData ( const QModelIndex & index, const QVariant & value, int role)
	{

		//TODO IMPLEMENTATION OF INPUT/OUTPUT RENAMING & DESCRIPTION CHANGING...
		/*
		int row = index.row();
		int col = index.column();

		vector<UITerminal*> terminals;

		if (m_input)
		{
			terminals = m_params->getUINode()->getInputs();
		}
		else
		{
			terminals = m_params->getUINode()->getOutputs();
		}

		switch (col)
		{
			case 0:
				terminals[row]->setName(value.toString().toStdString());
				break;

			case 1:
				terminals[row]->setDescription(value.toString().toStdString());
				break;
		}
		*/

		return false;
	}

	Qt::ItemFlags InputOutputModel::flags ( const QModelIndex & index ) const
	{
		return (Qt::ItemIsEditable | Qt::ItemIsSelectable | Qt::ItemIsEnabled);
	}

	void InputOutputModel::notifyChanged(const FD::UINode* node)
	{
		reset();
	}

	bool InputOutputModel::removeRows ( int row, int count, const QModelIndex & parent)
	{
		beginRemoveRows (parent, row, row + count);

		for (int currentRow = row; currentRow < row + count; currentRow++)
		{
			if (m_input)
			{
				vector<UITerminal*> terminals = m_params->getUINode()->getInputs();

				if (currentRow < (int) terminals.size())
				{

					delete terminals[currentRow];
				}
				else
				{
					return false;
				}
			}
			else
			{
				vector<UITerminal*> terminals = m_params->getUINode()->getOutputs();
				if (currentRow < (int) terminals.size())
				{
					delete terminals[currentRow];

				}
				else
				{
					return false;
				}
			}
		}

		endRemoveRows();
		return true;
	}

} //namespace FD
