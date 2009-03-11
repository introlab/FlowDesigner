//============================================================================
// Name        : Erlang-proto-player-cc.cpp
// Author      :
// Version     :
// Description : C part of Erlang-proto-player, a linker to Player/stage
//============================================================================

#include "Erlang-proto-player-cc.h"

#include "erl_interface.h"
#include "ei.h"
#include <iostream>
#include <fstream>

typedef unsigned char byte;

int main() {
  ETERM *tuplep, *intp;
  ETERM *fnp, *argp, *argp2, *resp;
  ETERM** listLaser;
  int res;
  byte buf[50000];

  /**
   * Pour débugger
   *
   */
  ofstream fichier("trace.txt", ios::out | ios::trunc);  // ouverture en écriture avec effacement du fichier ouvert

  fichier << "=================================================" << endl;
  /**
   * Fin
   */

  erl_init(NULL, 0);

  Player pinf=Player();
  pinf.connect("localhost",6665);

  PlayerConnect* player=pinf.getPlayer();

  while (read_cmd(buf) > 0) {

	fichier << "refresh" << endl;
	player->refresh();

	fichier << "decode tuple" << endl;
    tuplep = erl_decode(buf);
    fichier << "decode fonction name" << endl;
    fnp = erl_element(1, tuplep);

    fichier << "find good fonction" << endl;

    if (strncmp((const char *) ERL_ATOM_PTR(fnp), "setSpeed", 8) == 0) {
       argp = erl_element(2, tuplep);
       argp2 = erl_element(3, tuplep);
       fichier << "setSpeed" << endl;
       player->getPositionProxy()->SetSpeed(ERL_FLOAT_VALUE(argp),ERL_FLOAT_VALUE(argp2));
       res=1;
       fichier << "mk" << endl;
       intp = erl_mk_int(res);
       fichier << "encode" << endl;
       erl_encode(intp, buf);
       fichier << "write" << endl;
       write_cmd(buf, erl_term_len(intp));
       fichier << "free intp" << endl;
       erl_free_term(intp);
       fichier << "free argp" << endl;
       erl_free_term(argp);
       fichier << "free argp 2" << endl;
       erl_free_term(argp2);
       fichier << "end setSpeed" << endl;
    } else if ( strncmp((const char *) ERL_ATOM_PTR(fnp), "setCarLike", 10) == 0) {
        argp = erl_element(2, tuplep);
        argp2 = erl_element(3, tuplep);
        //fichier << "setCarLike" << endl;
        player->getPositionProxy()->SetCarlike(ERL_FLOAT_VALUE(argp),ERL_FLOAT_VALUE(argp2));
        res=1;
        intp = erl_mk_int(res);
        erl_encode(intp, buf);
        write_cmd(buf, erl_term_len(intp));
        fichier << "free intp" << endl;
        erl_free_term(intp);
        fichier << "free argp" << endl;
        erl_free_term(argp);
        fichier << "free argp 2" << endl;
        erl_free_term(argp2);
    } else if ( strncmp((const char *) ERL_ATOM_PTR(fnp), "playerLaser", 11) == 0) {

        fichier << "PlayerLaser" << endl;
    	listLaser = pinf.getPlayerLaserScan();
        fichier << "createList" << endl;
        if(player->getLaserProxy()->GetCount()!=0)
        fichier << "count2: "<< player->getLaserProxy()->GetCount() << endl;
        int size=player->getLaserProxy()->GetCount();
        resp = erl_mk_list(listLaser,size);

        fichier << "encode" << endl;
        erl_encode(resp, buf);
        fichier << "write" << endl;
        write_cmd(buf, erl_term_len(resp));
        fichier << "end" << endl;
        erl_free_array(listLaser,361);

        fichier << "free resp" << endl;
        erl_free_term(resp);

    }else if ( strncmp((const char *) ERL_ATOM_PTR(fnp), "getLaserScanSize", 16) == 0) {

        fichier << "getLaserScanSize" << endl;
        res = player->getLaserProxy()->GetCount();
        fichier << "mk" << endl;
        intp = erl_mk_int(res);
        fichier << "encode" << endl;
        erl_encode(intp, buf);
        fichier << "write" << endl;
        write_cmd(buf, erl_term_len(intp));
        fichier << "free intp" << endl;
        erl_free_term(intp);
        fichier << "end getLaserScanSize" << endl;

    }else if ( strncmp((const char *) ERL_ATOM_PTR(fnp), "getLaserScan", 12) == 0) {

        argp = erl_element(2, tuplep);
        fichier << "getLaserScan" << endl;
        res = pinf.getLaserScan(ERL_INT_VALUE(argp));
        fichier << "mk" << endl;
        intp = erl_mk_int(res);
        fichier << "encode" << endl;
        erl_encode(intp, buf);
        fichier << "write" << endl;
        write_cmd(buf, erl_term_len(intp));
        fichier << "free intp" << endl;
        erl_free_term(intp);
        fichier << "free argp" << endl;
        erl_free_term(argp);
        fichier << "end getLaserScan" << endl;

    }

    fichier << "free tuplep" << endl;
    erl_free_compound(tuplep);
    fichier << "free fnp" << endl;
    erl_free_term(fnp);
    fichier << "end loop" << endl;


  }

  /**
   * Pour débugger
   */
          fichier.close();
  /**
   * Fin
   */
}



/*int main() {
 int fn, arg, res;
 double d1, d2;
  byte buf[256];

  Player pinf=Player();
  pinf.connect("localhost",6665);

  PlayerConnect* player=pinf.getPlayer();

  while (read_cmd(buf) > 0) {
    fn = buf[0];
//    arg = buf[1];

    if (fn == SETSPEED) {
    	d1=buf[1];
    	d2=buf[2];
    	player->getPositionProxy()->SetSpeed(buf[1],buf[2]);

    } else if (fn == SETCARLIKE) {
    	d1=buf[1];
    	d2=buf[2];
    	player->getPositionProxy()->SetCarlike(buf[1],buf[2]);

    }

    buf[0] = res;
    write_cmd(buf, 1);
  }

  delete(player);

  return 0;
}
*/


