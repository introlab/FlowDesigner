/*
 * playerConnect.h
 *
 *  Created on: 21 janv. 2009
 *      Author: pascalbeaudry
 */

#ifndef PLAYERCONNECT_H_
#define PLAYERCONNECT_H_

#include <libplayerc++/playerc++.h>
#include <libplayerc++/playerclient.h>

using namespace PlayerCc;

class PlayerConnect{

private:

  PlayerClient *m_client;
  Position2dProxy *m_positionProxy;
  LaserProxy *m_laserProxy;
  SonarProxy *m_sonarProxy;
  GpsProxy *m_gpsProxy;
  bool m_isConnected;

  void createProxies(bool startPositionProxy,
		     bool startLaserProxy,
		     bool startSonarProxy,
		     bool startGPSProxy)
  {

    //create all proxies
   if(startPositionProxy)
    {
       m_positionProxy = new Position2dProxy(m_client,0);
    }

    if(startLaserProxy)
    {
       m_laserProxy = new LaserProxy(m_client,0);
    }

    if(startSonarProxy)
    {
       m_sonarProxy =  new SonarProxy(m_client,0);
    }

    if(startGPSProxy)
    {
       m_gpsProxy =  new GpsProxy(m_client,0);
    }
  }

  void destroyProxies()
  {
    // delete all proxies
    if (m_positionProxy)
    {
       delete m_positionProxy;
       m_positionProxy = NULL;
    }


    if (m_laserProxy)
    {
       delete m_laserProxy;
       m_laserProxy = NULL;
    }

    if (m_sonarProxy)
    {
       delete m_sonarProxy;
       m_sonarProxy = NULL;
    }


    if (m_gpsProxy)
    {
       delete m_gpsProxy;
       m_gpsProxy = NULL;
    }

  }

 public:

	 PlayerConnect(const char* hostname=NULL, int port=PLAYER_PORTNUM,
                   bool startPositionProxy = true,
		   bool startLaserProxy = true,
		   bool startSonarProxy = false,
		   bool startGPSProxy = false)
    :m_positionProxy(NULL),
     m_laserProxy(NULL),
    m_sonarProxy (NULL), m_gpsProxy(NULL)
  {
	m_client = new PlayerCc::PlayerClient(hostname, port);

    createProxies(startPositionProxy, startLaserProxy,
                  startSonarProxy,
		  startGPSProxy);

  }

  ~PlayerConnect() {
    destroyProxies();
  }

  Position2dProxy * getPositionProxy() {return m_positionProxy;}

  LaserProxy * getLaserProxy() {return m_laserProxy;}

  SonarProxy *getSonarProxy() {return m_sonarProxy;}

  GpsProxy * getGpsProxy() {return m_gpsProxy;}

  PlayerClient* get_player_client() {return m_client;}

  void printOn(std::ostream &out = std::cout) const {
    out<<"<PlayerConnect>\n";
  }

  void refresh(){

	  m_client->Read();
  }


};

#endif /* PLAYERCONNECT_H_ */
