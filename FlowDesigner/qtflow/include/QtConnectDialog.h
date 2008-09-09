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
#ifndef QTCONNECTDIALOG_H_
#define QTCONNECTDIALOG_H_

#include <QtCore/QVariant>
#include <QtNetwork/QUdpSocket>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QDialog>
#include <QtGui/QDialogButtonBox>
#include <QtGui/QGroupBox>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QMessageBox>
#include <QtGui/QStandardItemModel>
#include <QtGui/QTabWidget>
#include <QtGui/QTreeView>
#include <QtGui/QItemSelectionModel>

#include "QtFlowIpBroadcaster.h"

#include <iostream>

namespace FD
{

/**
 * This dialog is used to show FlowDesigner processes 
 * running on the network. When accepted, it returns the 
 * host and the port selected.
 * 
 * @author Mathieu Labbe
 */
class QtConnectDialog : public QDialog
{
	Q_OBJECT;
private:
	/**
	 * Update interval to remove ended processes in the list.
	 */
	static const int UPDATE_INTERVAL_MS = QtFlowIpBroadcaster::BROADCAST_INTERVAL_MS*2;

public:
	/**
	 * The constructor.
	 * @param parent the QWidget parent
	 */
	QtConnectDialog(QWidget* parent);
	
	/**
	 * The destructor.
	 */
    ~QtConnectDialog();
    
    /**
     * @return the host
     */
    QString getHost() const;
    
    /**
     * @return the port
     */
    int getPort() const;
    
public slots:
	/**
	 * Called when dialog is accepted.
	 */
    virtual void accept();
    
    /**
	 * Called when dialog is rejected.
	 */
    virtual void reject();
    
protected:
	/**
	 * Called when the dialog is hided.
	 * @param event the hide event
	 */
    virtual void hideEvent(QHideEvent* event);
    
    /**
	 * Called when the dialog is showed.
	 * @param event the show event
	 */
    virtual void showEvent(QShowEvent* event);
   
private slots:
	/**
	 * Called when UDP datagrams are received.
	 */
	void processPendingDatagrams();
	
	/**Called when UDP datagrams are received.
	 * Called when an item in the list is selected (double-click).
	 * @param index the index of the selected item
	 */
	void indexSelected(const QModelIndex &index);
	
	/**
	 * Called to remove ended processes in the list.
	 */
	void updateModelTime();
 
private:
	/**
	 * Setup the interface of the dialog.
	 */
    void setupUi();
    
    /**
     * Setup the model of the list. It creates the model and it 
     * setups the view of the list.
     */
    void setupModel();
    
    /**
     * Retranslate the interface. 
     */
    void retranslateUi();
    
    /**
     * Validate the form when the dialog is accepted.
     * @return true if there is no error in the form, otherwise false
     */
    bool validateForm();
    
    /**
     * Enable or disable the udp socket
     * depending if the dialog is showed or not. 
     * @param enable enable or disable the socket
     */
    void enableSocket(bool enable);
    
    /**
     * Update the model of the list. If the entry doesn't exist, a new 
     * one is created. Otherwise, the activity time is updated.
     * @param name the name of the document/process
     * @param host the host name of the process
     * @param port the port of the process
     */
    void updateModel(const QString &name, const QString &host, const QString &port);
    
private:   
    QTabWidget* m_tabWidget;
    
    // First tab stuff
    QTreeView* m_listView;
    QStandardItemModel* m_model;
    QItemSelectionModel* m_selectionModel;
	QUdpSocket* m_udpSocket;
	
	// Second tab stuff
	QLabel* m_label;
	QLabel* m_label_2;
	QLabel* m_label_3;
	QLineEdit *m_lineEdit_host;
    QLineEdit *m_lineEdit_port;
    QString m_host;
    int m_port;
};
}//namespace FD
#endif /*QTCONNECTDIALOG_H_*/
