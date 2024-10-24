#include <time.h>
#include <f2c.h>

VOID fdate_(ret_val, ret_val_len)
char *ret_val;
ftnlen ret_val_len;
{
    time_t tp;
    time(&tp);
    s_copy(ret_val, ctime(&tp), 24L, 24L);
    return;
}
