/*
 * player.h
 *
 *  Created on: 21 janv. 2009
 *      Author: pascalbeaudry
 */

#ifndef PLAYER_H_
#define PLAYER_H_

/*#include <iostream>
#include <string>
#include <fstream>
*/
#include "playerConnect.h"
#include "erl_interface.h"
#include "ei.h"

using namespace std;

class Player{

private:
	PlayerConnect* _playerInstance;
	bool _initConnect;

public:

	Player(){
		_initConnect=false;
	}

	void connect(const char* hostname, int port=PLAYER_PORTNUM){

		if(!(_initConnect)){

			_playerInstance=new PlayerConnect(hostname,port,true,true,false,false);
			_initConnect=true;
			_playerInstance->refresh();
			getPlayer()->getLaserProxy()->RequestConfigure();
			getPlayer()->getLaserProxy()->RequestGeom();


		}
	}

	PlayerConnect* getPlayer(){

		if(!(_initConnect))
			connect("localhost");

		_playerInstance->refresh();

		return _playerInstance;

	}

	ETERM** getPlayerLaserScan(){

		 LaserProxy* laser=(getPlayer()->getLaserProxy());

	     int max=laser->GetCount() ;

	     ETERM** rangeVect = new ETERM*[max];

	     //copy range & intensity values
	     for (int i = 0; i < max; i++) {
	         (rangeVect)[i] = erl_mk_int(laser->GetRange(i) * 1000.0);
	     }

	     return rangeVect;
	}

	int getLaserScan(int index){

		 LaserProxy* laser=(getPlayer()->getLaserProxy());

	     int laserScan = laser->GetRange(index) * 1000.0;

	     return laserScan;
	}
};


#endif /* PLAYER_H_ */
