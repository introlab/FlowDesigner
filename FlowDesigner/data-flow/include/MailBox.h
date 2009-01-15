/*
 *  MailBox.h
 *  FlowDesigner
 *
 *  Created by Pascal Beaudry on 08-11-17.
 *  Copyright 2008 Laborius. All rights reserved.
 *
 */

#define DEBUG false

#ifndef MAILBOX_H
#define MAILBOX_H

#include "BufferedNode.h" 
#include <semaphore.h>
#include "Node.h"
#include <queue> 
#include "Timer.h"
#include <unistd.h>
#include <sys/time.h>
#include <unistd.h>

using namespace std;

namespace FD {

class MailBox;

class PopFromMailBoxNode : public BufferedNode {

	protected:
       int outputID;
	   std::string mailboxName;
	   MailBox* mailbox;
	   queue <ObjectRef> msgQueue;
	   pthread_mutex_t pushmodifLock;
	   ObjectRef popObject();
	   
    public:
	  PopFromMailBoxNode(string nodeName, ParameterSet params);
	  void calculate(int output_id, int count, Buffer &out);
	  void pushObject(ObjectRef obj);

};
 
class MailBox{

private:
   pthread_mutex_t pushmodifLock;
   pthread_mutex_t registermodifLock;
   ObjectRef mailObject;
   vector<PopFromMailBoxNode*> mailBoxList;
   vector<PopFromMailBoxNode*>::iterator Iter;
   std::string name;
   
public:

	MailBox(std::string nname){
	      pthread_mutex_init(&pushmodifLock, NULL);
		  pthread_mutex_init(&registermodifLock, NULL);
		  name=nname;
	}

	void push(ObjectRef obj){
	
			if(DEBUG)
				cout<<'\n'<< "push mail : "<< name<<" "<< obj <<'\n';
			if(mailBoxList.empty())
				return;
			
			pthread_mutex_lock(&pushmodifLock);
			for (Iter = mailBoxList.begin(); Iter != mailBoxList.end(); Iter++ )
			{
				(*Iter)->pushObject(obj->clone());
			}
				
			pthread_mutex_unlock(&pushmodifLock);
	}
	
	void registerReceiver(PopFromMailBoxNode* receiver){
			pthread_mutex_lock(&registermodifLock);
			mailBoxList.push_back(receiver);
			pthread_mutex_unlock(&registermodifLock);
	}
};


class MailBoxManager{

private:
	std::map<std::string,MailBox*> mailBoxList;
	std::map<std::string,MailBox*>::iterator mailboxIt;
	static MailBoxManager* instance;
	static pthread_mutex_t instancelock;
	
	MailBoxManager(){
		
		//init Mailbox system
	}
	
public:

	MailBox* getMailBoxFromName(std::string name)
	{
		
		pthread_mutex_lock(&instancelock);
					
		//Check if Mailbox exist
		mailboxIt=mailBoxList.find(name);
		
		if(mailboxIt==mailBoxList.end()){
			if(DEBUG)
				cout<<'\n'<< "Create new MailBox instance : "<< name <<'\n';
			mailBoxList[name]=new MailBox(name);
		}

		pthread_mutex_unlock(&instancelock);
				  
		return mailBoxList[name];
	}
	
	static MailBoxManager* getInstance()
	{
		if(!instance){
			pthread_mutex_lock(&instancelock);
			if(!instance){
				
				if(DEBUG)
					cout<<'\n'<< "Create new mailBoxManagerInstance " <<'\n';
				instance=new MailBoxManager();	
			}
			pthread_mutex_unlock(&instancelock);
			
		}
		
		return instance;
	}

};

class MessageBox{

private:
   pthread_mutex_t mutexLock;
   ObjectRef mailObject;
   bool empty;
   std::string name;
   
public:

	MessageBox(std::string nname){
		  empty=true;
		  pthread_mutex_init(&mutexLock,0);
		  name=nname;
	}

	void set(ObjectRef obj){
			if(DEBUG)
				cout<<'\n'<< "Lock : "<< name <<'\n';
			pthread_mutex_lock(&mutexLock);
			if(DEBUG)
				cout<<'\n'<< "msg Set : "<< name<<" "<< obj <<'\n';
			mailObject = obj->clone();
			pthread_mutex_unlock(&mutexLock);
			if(DEBUG)
				cout<<'\n'<< "Unlock : "<< name<<" "<<mailObject <<'\n';
			empty=false;
	}
	
	bool isEmpty(){
			return empty;
	}
	
	ObjectRef get(){
			ObjectRef tmp;
			
			while(isEmpty()){
					if(DEBUG)
						cout<<'\n'<< "mailObject "<< name<<" isEmpty()"<<'\n';
					 thread_usleep(1000);
			}
			
			
			if(DEBUG)
				cout<<'\n'<< "get wait lock "<< name<<'\n';
			pthread_mutex_lock(&mutexLock);
			tmp=mailObject;
			
			if(DEBUG)
				cout<<'\n'<< "get "<< name<<" "<< tmp <<'\n';
				
			pthread_mutex_unlock(&mutexLock);
			
			if(DEBUG)
				cout<<'\n'<< "get lock"<< tmp <<'\n';
			return tmp;
	}
};



class MessageBoxManager{

private:
	std::map<std::string,MessageBox*> messageBoxList;
	std::map<std::string,MessageBox*>::iterator messageboxIt;
	static MessageBoxManager* instance ;
	static pthread_mutex_t instancelock;
	
	MessageBoxManager(){

	}
	
public:

	MessageBox* getMessageBoxFromName(std::string name)
	{

		pthread_mutex_lock(&instancelock);
					
		//Check if Mailbox exist
		messageboxIt=messageBoxList.find(name);
		
		if(messageboxIt==messageBoxList.end()){
			if(DEBUG)
				cout<<'\n'<< "Create new MessageBox instance : "<< name <<'\n';
			messageBoxList[name]=new MessageBox(name);
		}

		pthread_mutex_unlock(&instancelock);
				  
		return messageBoxList[name];
	}
	
	static MessageBoxManager* getInstance()
	{
		if(!instance){
			pthread_mutex_lock(&instancelock);
			if(!instance){
				
				if(DEBUG)
					cout<<'\n'<< "Create new messageBoxManagerInstance " <<'\n';
				instance=new MessageBoxManager();	
			}
			pthread_mutex_unlock(&instancelock);
			
		}
		
		return instance;
	}

};

}

#endif