/** emacs: this is -*- c++ -*- **/
/**
 **     @file    simpletimer.h
 **
 **     @brief   these functions have a precision of about 0.001 ms
 **
 **     @author  mark sutton
 **     @date    Thu 22 Jan 2009 15:51:52 GMT 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: simpletimer.h, v0.0   Thu 22 Jan 2009 15:51:52 GMT sutt $
 **
 **/


#ifndef SIMPLETIMER_H
#define SIMPLETIMER_H

#include <time.h>
#include <sys/time.h>

//#ifdef __cplusplus
//extern "C" {
//#endif


inline struct timeval simpletimer_start(void) {
  struct timeval start_time;
  gettimeofday(&start_time, NULL);            
  return start_time;
}

inline double simpletimer_stop(const struct timeval& start_time)
{
  struct timeval stop_time;
  struct timeval diff_time;
  gettimeofday(&stop_time, NULL);            
  diff_time.tv_sec  = stop_time.tv_sec  - start_time.tv_sec;
  diff_time.tv_usec = stop_time.tv_usec - start_time.tv_usec;
  return (diff_time.tv_sec*1000.0) + (diff_time.tv_usec/1000.0);
}


//#ifdef __cplusplus
//}
//#endif

#endif /* SIMPLETIMER_H */










