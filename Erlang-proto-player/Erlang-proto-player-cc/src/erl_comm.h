/*
 * erl_comm.h
 *
 *  Created on: 22 janv. 2009
 *      Author: pascalbeaudry
 */

#ifndef ERL_COMM_H_
#define ERL_COMM_H_

typedef unsigned char byte;

int read_cmd(byte *buf);

int write_cmd(byte *buf, int len);

int read_exact(byte *buf, int len);

int write_exact(byte *buf, int len);

#endif /* ERL_COMM_H_ */
