/*
 *  MailBox.cc
 *  FlowDesigner
 *
 *  Created by Pascal Beaudry on 08-11-17.
 *  Copyright 2008 Laborius. All rights reserved.
 *
 */

#include "MailBox.h" 

namespace FD{

MailBoxManager* MailBoxManager::instance =0;
MessageBoxManager* MessageBoxManager::instance =0;
pthread_mutex_t MessageBoxManager::instancelock=PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t MailBoxManager::instancelock=PTHREAD_MUTEX_INITIALIZER;

}