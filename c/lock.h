/*  Part of SWI-Prolog Semweb package

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2011, University of Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef LOCK_H_INCLUDED
#define LOCK_H_INCLUDED

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

extern "C"
{

#ifdef __WINDOWS__
#include <windows.h>
#if defined(_REENTRANT) && defined(__WINDOWS__)
static enum
{ SIGNAL     = 0,
  MAX_EVENTS = 1
} win32_event_t;

typedef struct
{ HANDLE events[MAX_EVENTS];		/* events to be signalled */
  int    waiters;			/* # waiters */
} win32_cond_t;
#endif
#else
#include <pthread.h>
#endif

typedef struct rwlock
{
#ifdef _REENTRANT
#ifdef __WINDOWS__
  CRITICAL_SECTION	mutex;
  CRITICAL_SECTION	misc_mutex;
  win32_cond_t		rdcondvar;
  win32_cond_t		wrcondvar;
  win32_cond_t		upcondvar;
#else
  pthread_mutex_t	mutex;
  pthread_mutex_t	misc_mutex;
  pthread_cond_t	rdcondvar;
  pthread_cond_t	wrcondvar;
  pthread_cond_t	upcondvar;
#endif
  int			waiting_readers;
  int			waiting_writers;
  int			waiting_upgrade;
  size_t		thread_max;
  int		       *read_by_thread;
  int			allow_readers;
  int			lock_level;	/* recursive locks */
#endif
  int			writer;
  int			readers;
} rwlock;

int	rdlock(rwlock *lock);
int	wrlock(rwlock *lock, int allow_readers);
int	lockout_readers(rwlock *lock);
void	reallow_readers(rwlock *lock);
int	unlock(rwlock *lock, int rd);
int	lock_misc(rwlock *lock);
int	unlock_misc(rwlock *lock);
int	init_lock(rwlock *lock);
int	destroy_lock(rwlock *lock);

}

#endif /*LOCK_H_INCLUDED*/
