#include <stddef.h>
#include <time.h>
#include <f2c.h>

integer time_()
{
  return time(NULL);
}

VOID ctime_(ret_val, ret_val_len, stime)
char *ret_val;
ftnlen ret_val_len;
integer *stime;
{
    int s_copy();
    s_copy(ret_val, ctime(stime), 24L, 24L);
    return ;
}

void ltime_(stime, tarray)
integer *stime;
integer *tarray;
{
  struct tm *tp;
  tp = localtime(stime);
  tarray[0] = tp -> tm_sec;
  tarray[1] = tp -> tm_min;
  tarray[2] = tp -> tm_hour;
  tarray[3] = tp -> tm_mday;
  tarray[4] = tp -> tm_mon;
  tarray[5] = tp -> tm_year;
  tarray[6] = tp -> tm_wday;
  tarray[7] = tp -> tm_yday;
  tarray[8] = tp -> tm_isdst;
}

void gmtime_(stime, tarray)
integer *stime;
integer *tarray;
{
  struct tm *tp;
  tp = localtime(stime);
  tarray[0] = tp -> tm_sec;
  tarray[1] = tp -> tm_min;
  tarray[2] = tp -> tm_hour;
  tarray[3] = tp -> tm_mday;
  tarray[4] = tp -> tm_mon;
  tarray[5] = tp -> tm_year;
  tarray[6] = tp -> tm_wday;
  tarray[7] = tp -> tm_yday;
  tarray[8] = tp -> tm_isdst;
}
