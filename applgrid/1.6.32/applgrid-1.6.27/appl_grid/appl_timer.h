/** emacs: this is -*- c++ -*- **/
/**************************************************************************
 **
 **   File:         appl_timer.h  
 **
 **   Description:  small timer wrapper  
 **                   
 **                   
 ** 
 **   Author:       M.Sutton  
 **
 **   Created:      Fri Jun 29 18:59:09 BST 2001
 **   Modified:     
 **                   
 **                   
 **
 **************************************************************************/ 


#ifndef __APPL_TIMER
#define __APPL_TIMER

#include <time.h>
#include <sys/time.h>

#define APPL_TIMER_START  0
#define APPL_TIMER_STOP   1
#define APPL_TIMER_SINCE -1

#ifdef __cplusplus
// namespace appl { 
extern "C" {
#endif


double appl_timer(int   );
void   appl_timer_initkey(void);
struct timeval appl_timer_start(void);
double         appl_timer_stop(struct timeval );

#ifdef __cplusplus
}
// };
#endif

#endif /* __APPL_TIMER */










