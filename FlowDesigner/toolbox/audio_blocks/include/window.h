// Copyright (C) 2001 Jean-Marc Valin

/********************************************************************
 *                                                                  *
 * THIS FILE IS PART OF THE Ogg Vorbis SOFTWARE CODEC SOURCE CODE.  *
 * USE, DISTRIBUTION AND REPRODUCTION OF THIS SOURCE IS GOVERNED BY *
 * THE GNU PUBLIC LICENSE 2, WHICH IS INCLUDED WITH THIS SOURCE.    *
 * PLEASE READ THESE TERMS DISTRIBUTING.                            *
 *                                                                  *
 * THE OggSQUISH SOURCE CODE IS (C) COPYRIGHT 1994-2000             *
 * by Monty <monty@xiph.org> and The XIPHOPHORUS Company            *
 * http://www.xiph.org/                                             *
 *                                                                  *
 ********************************************************************

 function: window functions
 last mod: $Id: window.h,v 1.2 2000/07/06 05:55:37 jmvalin Exp $

 ********************************************************************/

#ifndef _V_WINDOW_
#define _V_WINDOW_

#ifdef __cplusplus
extern "C"
{
#endif /* __cplusplus */


#define VI_WINDOWB 1
extern double *_vorbis_window(int type,int window,int left,int right);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif
