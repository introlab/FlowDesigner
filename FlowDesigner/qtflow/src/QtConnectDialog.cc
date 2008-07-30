#include "QtConnectDialog.h"

#include <QtGui/QHBoxLayout>
#include <QtGui/QVBoxLayout>
#include <QtCore/QTimer>
#include <QtCore/QTime>

#include "UIDocument.h"

//public:
QtConnectDialog::QtConnectDialog(QWidget* parent) : 
	QDialog(parent),
	m_udpSocket(NULL),
	m_host("localhost"), 
	m_port(FD::UIDocument::DEFAULT_CONNECTION_PORT)
{
	setupUi(this);
	setupModel();
	retranslateUi(this);
}

QtConnectDialog::~QtConnectDialog() 
{
}

QString QtConnectDialog::getHost() const
{
	return m_host;
}

int QtConnectDialog::getPort() const
{
	return m_port;
}

//public slots:
void QtConnectDialog::accept()
{
	if(validateForm()) {
		QDialog::accept();
		return;
	}
}

void QtConnectDialog::reject()
{
	lineEdit_host->setText(m_host);
	lineEdit_port->setText(QString("%1").arg(m_port));
	QDialog::reject();
}

//protected:
void QtConnectDialog::hideEvent(QHideEvent* event)
{
	enableSocket(false);
	QDialog::hideEvent(event);
}

void QtConnectDialog::showEvent(QShowEvent* event)
{
	enableSocket(true);
	QDialog::showEvent(event);
}
   
//private slots:
void QtConnectDialog::processPendingDatagrams()
{
	while (m_udpSocket->hasPendingDatagrams()) {
		QByteArray datagram;
		datagram.resize(m_udpSocket->pendingDatagramSize());
		m_udpSocket->readDatagram(datagram.data(), datagram.size());
		
		//For debug
		//std::cerr.write(datagram.data(), datagram.size());
		//std::cerr << std::endl;
		
		//Parse the datatgram
		QString str(datagram);
		QStringList data = str.split("|");
		if(data.size() == 4 && data[0].compare("QtFlow") == 0) {
			//Update the model
			updateModel(data[1], data[2], data[3]);
		}
	}
}

void QtConnectDialog::indexSelected(const QModelIndex &index)
{
	if(index.isValid()) {
		this->accept();
	}
}

void QtConnectDialog::updateModelTime()
{
	for(int row=0; row<m_model->rowCount(); row++) {
		QModelIndex index = m_model->index(row, 3);
		if(index.data().toTime() < QTime::currentTime().addSecs(-UPDATE_INTERVAL_MS/1000)) 
		{
			m_model->removeRow(row);
			row--;
		}
	}
}
 
//private:
void QtConnectDialog::setupUi(QDialog *Dialog)
{
    if (Dialog->objectName().isEmpty())
        Dialog->setObjectName(QString::fromUtf8("Dialog"));
    Dialog->resize(350, 300);
    
    QWidget* centralWidget = new QWidget(Dialog);
    
    QVBoxLayout* mainLayout = new QVBoxLayout();
    centralWidget->setLayout(mainLayout);
    
    m_tabWidget = new QTabWidget(centralWidget);
    
    QWidget* tab1 = new QWidget();
    QHBoxLayout* layout1 = new QHBoxLayout();
    tab1->setLayout(layout1);
        
    m_listView = new QTreeView(tab1);
    m_listView->setMinimumWidth(310);
    layout1->addWidget(m_listView);
    
    //Add tab 1
    m_tabWidget->addTab(tab1, tr("Select a document"));
    
    QWidget* tab2 = new QWidget();
    lineEdit_host = new QLineEdit(tab2);
    lineEdit_host->setObjectName(QString::fromUtf8("lineEdit_host"));
    lineEdit_host->setGeometry(QRect(60, 30, 158, 28));
    
    //Set the default host text
    lineEdit_host->setText(m_host);
    
    lineEdit_port = new QLineEdit(tab2);
    lineEdit_port->setObjectName(QString::fromUtf8("lineEdit_port"));
    lineEdit_port->setGeometry(QRect(60, 70, 61, 28));
    
    //Set the default port text
    lineEdit_port->setText(QString("%1").arg(m_port));
    
    lineEdit_port->setMaxLength(5);
    label = new QLabel(tab2);
    label->setObjectName(QString::fromUtf8("label"));
    label->setGeometry(QRect(20, 30, 31, 28));
    label_2 = new QLabel(tab2);
    label_2->setObjectName(QString::fromUtf8("label_2"));
    label_2->setGeometry(QRect(20, 70, 28, 28));
    label_3 = new QLabel(tab2);
    label_3->setObjectName(QString::fromUtf8("label_3"));
    label_3->setGeometry(QRect(130, 70, 150, 28));
    
    //Add tab 2
    m_tabWidget->addTab(tab2, tr("Connect to..."));
    
    mainLayout->addWidget(m_tabWidget);

	buttonBox = new QDialogButtonBox(centralWidget);
    buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
    buttonBox->setOrientation(Qt::Horizontal);
    buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);
    buttonBox->setCenterButtons(false);
    
    mainLayout->addWidget(buttonBox);

    retranslateUi(Dialog);
    
    QObject::connect(buttonBox, SIGNAL(accepted()), this, SLOT(accept()));
    QObject::connect(buttonBox, SIGNAL(rejected()), this, SLOT(reject()));

    QMetaObject::connectSlotsByName(Dialog);
} // setupUi

void QtConnectDialog::setupModel()
{
	//Setup model
    m_model = new QStandardItemModel(0, 4);
    m_model->setHeaderData(0, Qt::Horizontal, QObject::tr("Name"));
 	m_model->setHeaderData(1, Qt::Horizontal, QObject::tr("Host"));
 	m_model->setHeaderData(2, Qt::Horizontal, QObject::tr("Port"));     
 	m_model->setHeaderData(3, Qt::Horizontal, QObject::tr("Activity")); 
    
    //Setup the view
    m_listView->setRootIsDecorated(false);
 	m_listView->setAlternatingRowColors(true);
 	m_listView->setSelectionBehavior(QAbstractItemView::SelectRows); // Select rows
 	m_listView->setSelectionMode(QAbstractItemView::SingleSelection); // Single selection
 	m_listView->setEditTriggers(QAbstractItemView::NoEditTriggers); // Not editable
    m_listView->setModel(m_model);
    m_listView->setColumnHidden(3, true);
    
    //Timer for refreshing the list
    QTimer* timer = new QTimer(this);
    
    //Behavior on doubleClick, same as select a row and click 'ok'
    QObject::connect(m_listView, SIGNAL(doubleClicked (const QModelIndex &)), this, SLOT(indexSelected(const QModelIndex &)));
    
    connect(timer, SIGNAL(timeout()), this, SLOT(updateModelTime()));
    timer->start(UPDATE_INTERVAL_MS);
}

void QtConnectDialog::enableSocket(bool enable)
{
	if(!m_udpSocket) {
		m_udpSocket = new QUdpSocket(this);
		connect(m_udpSocket, SIGNAL(readyRead()), this, SLOT(processPendingDatagrams()));
	}
	
	if(enable) {
 		if(!m_udpSocket->bind(QtFlowIpBroadcaster::BROADCAST_PORT)) {
 			m_udpSocket->close();	
 			QMessageBox::warning(this, tr("Warning"), tr("Unable to link socket to the port '%1'.").arg(QtFlowIpBroadcaster::BROADCAST_PORT), QMessageBox::Ok);
 		}	
	}
	else {
		m_udpSocket->close();	
	}
}

void QtConnectDialog::retranslateUi(QDialog *Dialog)
{
    Dialog->setWindowTitle(QApplication::translate("Dialog", "Remote document", 0, QApplication::UnicodeUTF8));
    label->setText(QApplication::translate("Dialog", "Host :", 0, QApplication::UnicodeUTF8));
    label_2->setText(QApplication::translate("Dialog", "Port :", 0, QApplication::UnicodeUTF8));
    label_3->setText(QApplication::translate("Dialog", "(49152 through 65535)", 0, QApplication::UnicodeUTF8));
    Q_UNUSED(Dialog);
} // retranslateUi

bool QtConnectDialog::validateForm()
{
	if(m_tabWidget->currentIndex() == 0) {
		QItemSelectionModel* selection = m_listView->selectionModel();
		if(selection) {
			QModelIndexList indexes = selection->selectedIndexes();
		    //Normally, only one row can be selected
		    m_host = indexes[1].data().toString();
		    m_port = indexes[2].data().toInt();
		}
		else {
			QMessageBox::warning(this, tr("Error"), tr("Select an address first."), QMessageBox::Ok);
			return false;
		}
	}
	else if(m_tabWidget->currentIndex() == 1) {
		QString host = lineEdit_host->text();
		int port = lineEdit_port->text().toInt();
		if(host.isEmpty() || port < 49152 || port > 65535)
		{
			QString msg;
			if(host.isEmpty()) {
				msg.append(tr("-The host is empty.\n"));
			}
			if(port < 49152 || port > 65535) {
				msg.append(tr("-The port is invalid."));
			}
			QMessageBox::warning(this, tr("Error"), msg, QMessageBox::Ok);
			return false;
		}
		else
		{
			m_host = host;
			m_port = port;
		}
	}
	return true;
}

void QtConnectDialog::updateModel(const QString &name, const QString &host, const QString &port)
{
	bool insertIt = true;
	if(m_model->hasChildren()) {
		//Verify if the address already exists in the model
		QModelIndex indexName;
		QModelIndex indexHost;
		QModelIndex indexPort;
		for(int row=0; row<m_model->rowCount(); row++) {
			indexName = m_model->index(row, 0);
			indexHost = m_model->index(row, 1);
			indexPort = m_model->index(row, 2);
			if(indexName.data().toString().compare(name) == 0 &&
				indexHost.data().toString().compare(host) == 0 &&
				indexPort.data().toString().compare(port) == 0) 
			{
				//Refresh the time
				m_model->setData(m_model->index(row, 3), QTime::currentTime());
				
				insertIt = false;
				break;
			}
		}
	}
	
	if(insertIt) {
		int row = m_model->rowCount();
		m_model->insertRow(row);
		m_model->setData(m_model->index(row, 0), name);
		m_model->setData(m_model->index(row, 1), host);
		m_model->setData(m_model->index(row, 2), port);
		m_model->setData(m_model->index(row, 3), QTime::currentTime());
	}
}
